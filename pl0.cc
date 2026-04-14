//
//  pl0.cc - PL/0 JIT compiler
//
//  Copyright © 2018 Yuji Hirose. All rights reserved.
//  MIT License
//

#include <peglib.h>

#include <fstream>
#include <sstream>

#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"

using namespace peg;
using namespace peg::udl;
using namespace llvm;
using namespace std;

auto grammar = R"(
  program    <- _ block '.' _

  block      <- const var procedure statement
  const      <- ('CONST' __ ident '=' _ number (',' _ ident '=' _ number)* ';' _)?
  var        <- ('VAR' __ ident (',' _ ident)* ';' _)?
  procedure  <- ('PROCEDURE' __ ident ';' _ block ';' _)*

  statement  <- (assignment / call / statements / if / while / out / in)?
  assignment <- ident ':=' _ expression
  call       <- 'CALL' __ ident
  statements <- 'BEGIN' __ statement (';' _ statement )* 'END' __
  if         <- 'IF' __ condition 'THEN' __ statement
  while      <- 'WHILE' __ condition 'DO' __ statement
  out        <- ('out' __ / 'write' __ / '!' _) expression
  in         <- ('in' __ / 'read' __ / '?' _) ident

  condition  <- odd / compare
  odd        <- 'ODD' __ expression
  compare    <- expression compare_op expression
  compare_op <- < '=' / '#' / '<=' / '<' / '>=' / '>' > _

  expression <- sign term (term_op term)*
  sign       <- < [-+]? > _
  term_op    <- < [-+] > _

  term       <- factor (factor_op factor)*
  factor_op  <- < [*/] > _

  factor     <- ident / number / '(' _ expression ')' _

  ident      <- < [a-z] [a-z0-9]* > _
  number     <- < [0-9]+ > _

  ~_         <- [ \t\r\n]*
  ~__        <- ![a-z0-9_] _
)";

inline string format_error_message(const string& path, size_t ln, size_t col,
                                   const string& msg) {
  stringstream ss;
  ss << path << ":" << ln << ":" << col << ": " << msg << endl;
  return ss.str();
}

struct SymbolScope;

struct Annotation {
  shared_ptr<SymbolScope> scope;
};

typedef AstBase<Annotation> AstPL0;

inline shared_ptr<SymbolScope> get_closest_scope(shared_ptr<AstPL0> ast) {
  ast = ast->parent.lock();
  while (ast->tag != "block"_) {
    ast = ast->parent.lock();
  }
  return ast->scope;
}

struct SymbolScope {
  SymbolScope(shared_ptr<SymbolScope> outer) : outer(outer) {}

  bool has_symbol(string_view ident, bool extend = true) const {
    auto ret = constants.count(ident) || variables.count(ident);
    return ret ? true : (extend && outer ? outer->has_symbol(ident) : false);
  }

  bool has_constant(string_view ident) const {
    return constants.count(ident)
               ? true
               : (outer ? outer->has_constant(ident) : false);
  }

  bool has_variable(string_view ident) const {
    return variables.count(ident)
               ? true
               : (outer ? outer->has_variable(ident) : false);
  }

  bool has_procedure(string_view ident) const {
    return procedures.count(ident)
               ? true
               : (outer ? outer->has_procedure(ident) : false);
  }

  shared_ptr<AstPL0> get_procedure(string_view ident) const {
    auto it = procedures.find(ident);
    return it != procedures.end() ? it->second : outer->get_procedure(ident);
  }

  map<string_view, int> constants;
  set<string_view> variables;
  map<string_view, shared_ptr<AstPL0>> procedures;
  set<string_view> free_variables;

 private:
  shared_ptr<SymbolScope> outer;
};

inline void throw_runtime_error(const shared_ptr<AstPL0> node,
                                const string& msg) {
  throw runtime_error(
      format_error_message(node->path, node->line, node->column, msg));
}

struct SymbolTableBuilder {
  static void build_on_ast(const shared_ptr<AstPL0> ast,
                           shared_ptr<SymbolScope> scope = nullptr) {
    switch (ast->tag) {
      case "block"_:
        block(ast, scope);
        break;
      case "assignment"_:
        assignment(ast, scope);
        break;
      case "call"_:
        call(ast, scope);
        break;
      case "ident"_:
        ident(ast, scope);
        break;
      default:
        for (auto node : ast->nodes) {
          build_on_ast(node, scope);
        }
        break;
    }
  }

 private:
  static void block(const shared_ptr<AstPL0> ast,
                    shared_ptr<SymbolScope> outer) {
    auto scope = make_shared<SymbolScope>(outer);
    const auto& nodes = ast->nodes;
    constants(nodes[0], scope);
    variables(nodes[1], scope);
    procedures(nodes[2], scope);
    build_on_ast(nodes[3], scope);
    ast->scope = scope;
  }

  static void constants(const shared_ptr<AstPL0> ast,
                        shared_ptr<SymbolScope> scope) {
    const auto& nodes = ast->nodes;
    for (auto i = 0u; i < nodes.size(); i += 2) {
      auto ident = nodes[i + 0]->token;
      if (scope->has_symbol(ident)) {
        throw_runtime_error(
            nodes[i], "'" + std::string(ident) + "' is already defined...");
      }
      auto number = nodes[i + 1]->token_to_number<int>();
      scope->constants.emplace(ident, number);
    }
  }

  static void variables(const shared_ptr<AstPL0> ast,
                        shared_ptr<SymbolScope> scope) {
    const auto& nodes = ast->nodes;
    for (auto i = 0u; i < nodes.size(); i += 1) {
      auto ident = nodes[i]->token;
      if (scope->has_symbol(ident)) {
        throw_runtime_error(
            nodes[i], "'" + std::string(ident) + "' is already defined...");
      }
      scope->variables.emplace(ident);
    }
  }

  static void procedures(const shared_ptr<AstPL0> ast,
                         shared_ptr<SymbolScope> scope) {
    const auto& nodes = ast->nodes;
    for (auto i = 0u; i < nodes.size(); i += 2) {
      auto ident = nodes[i + 0]->token;
      auto block = nodes[i + 1];
      scope->procedures[ident] = block;
      build_on_ast(block, scope);
    }
  }

  static void assignment(const shared_ptr<AstPL0> ast,
                         shared_ptr<SymbolScope> scope) {
    auto ident = ast->nodes[0]->token;
    if (scope->has_constant(ident)) {
      throw_runtime_error(ast->nodes[0], "cannot modify constant value '" +
                                             std::string(ident) + "'...");
    } else if (!scope->has_variable(ident)) {
      throw_runtime_error(ast->nodes[0],
                          "undefined variable '" + std::string(ident) + "'...");
    }

    build_on_ast(ast->nodes[1], scope);

    if (!scope->has_symbol(ident, false)) {
      scope->free_variables.emplace(ident);
    }
  }

  static void call(const shared_ptr<AstPL0> ast,
                   shared_ptr<SymbolScope> scope) {
    auto ident = ast->nodes[0]->token;
    if (!scope->has_procedure(ident)) {
      throw_runtime_error(
          ast->nodes[0], "undefined procedure '" + std::string(ident) + "'...");
    }

    auto block = scope->get_procedure(ident);
    if (block->scope) {
      for (const auto& free : block->scope->free_variables) {
        if (!scope->has_symbol(free, false)) {
          scope->free_variables.emplace(free);
        }
      }
    }
  }

  static void ident(const shared_ptr<AstPL0> ast,
                    shared_ptr<SymbolScope> scope) {
    auto ident = ast->token;
    if (!scope->has_symbol(ident)) {
      throw_runtime_error(ast,
                          "undefined variable '" + std::string(ident) + "'...");
    }

    if (!scope->has_symbol(ident, false)) {
      scope->free_variables.emplace(ident);
    }
  }
};

struct JIT {
  static void run(const shared_ptr<AstPL0> ast, bool emit_llvm) {
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();

    auto ctx = make_unique<LLVMContext>();
    auto mod = make_unique<Module>("pl0", *ctx);
    IRBuilder<> builder(*ctx);

    auto tyinfo =
        new GlobalVariable(*mod, builder.getPtrTy(), true,
                           GlobalValue::ExternalLinkage, nullptr, "_ZTIPKc");

    compile(builder, mod.get(), tyinfo, ast);

    if (emit_llvm) {
      mod->print(llvm::outs(), nullptr);
    } else {
      exec(std::move(ctx), std::move(mod));
    }
  }

 private:
  static void exec(unique_ptr<LLVMContext> ctx, unique_ptr<Module> mod) {
    auto jit = cantFail(orc::LLJITBuilder().create());

    auto& jd = jit->getMainJITDylib();
    auto gen = cantFail(
        orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
            jit->getDataLayout().getGlobalPrefix()));
    jd.addGenerator(std::move(gen));

    orc::ThreadSafeContext tsctx(std::move(ctx));
    cantFail(jit->addIRModule(
        orc::ThreadSafeModule(std::move(mod), std::move(tsctx))));

    auto mainFn = cantFail(jit->lookup("main")).toPtr<void (*)()>();
    mainFn();
  }

  static void compile(IRBuilder<>& builder, Module* module,
                       GlobalVariable* tyinfo,
                       const shared_ptr<AstPL0> ast) {
    compile_libs(builder, module);
    compile_program(builder, module, tyinfo, ast);
  }

  static void compile_switch(IRBuilder<>& builder, Module* module,
                              GlobalVariable* tyinfo,
                              const shared_ptr<AstPL0> ast) {
    switch (ast->tag) {
      case "assignment"_:
        compile_assignment(builder, module, tyinfo, ast);
        break;
      case "call"_:
        compile_call(builder, module, ast);
        break;
      case "statements"_:
        compile_statements(builder, module, tyinfo, ast);
        break;
      case "if"_:
        compile_if(builder, module, tyinfo, ast);
        break;
      case "while"_:
        compile_while(builder, module, tyinfo, ast);
        break;
      case "out"_:
        compile_out(builder, module, tyinfo, ast);
        break;
      default:
        compile_switch(builder, module, tyinfo, ast->nodes[0]);
        break;
    }
  }

  static Value* compile_switch_value(IRBuilder<>& builder, Module* module,
                                     GlobalVariable* tyinfo,
                                     const shared_ptr<AstPL0> ast) {
    switch (ast->tag) {
      case "odd"_:
        return compile_odd(builder, module, tyinfo, ast);
      case "compare"_:
        return compile_compare(builder, module, tyinfo, ast);
      case "expression"_:
        return compile_expression(builder, module, tyinfo, ast);
      case "ident"_:
        return compile_ident(builder, ast);
      case "number"_:
        return compile_number(builder, ast);
      default:
        return compile_switch_value(builder, module, tyinfo, ast->nodes[0]);
    }
  }

  static void compile_libs(IRBuilder<>& builder, Module* module) {
    // `out` function
    auto outFn =
        cast<Function>(module
                           ->getOrInsertFunction("out", builder.getVoidTy(),
                                                 builder.getInt32Ty())
                           .getCallee());

    {
      auto BB = BasicBlock::Create(builder.getContext(), "entry", outFn);
      builder.SetInsertPoint(BB);

      auto printFn = module->getOrInsertFunction(
          "printf",
          FunctionType::get(builder.getInt32Ty(), builder.getPtrTy(), true));

      auto val = &*outFn->arg_begin();
      auto fmt = builder.CreateGlobalString("%d\n", ".printf.fmt");
      builder.CreateCall(printFn, {fmt, val});

      builder.CreateRetVoid();
      verifyFunction(*outFn);
    }
  }

  static void compile_program(IRBuilder<>& builder, Module* module,
                               GlobalVariable* tyinfo,
                               const shared_ptr<AstPL0> ast) {
    // `start` function
    auto startFn = cast<Function>(
        module->getOrInsertFunction("__pl0_start", builder.getVoidTy())
            .getCallee());

    {
      auto BB = BasicBlock::Create(builder.getContext(), "entry", startFn);
      builder.SetInsertPoint(BB);

      compile_block(builder, module, tyinfo, ast->nodes[0]);

      builder.CreateRetVoid();
      verifyFunction(*startFn);
    }

    // `main` function
    auto mainFn = cast<Function>(
        module->getOrInsertFunction("main", builder.getVoidTy()).getCallee());

    {
      auto personalityFn = Function::Create(
          FunctionType::get(builder.getInt32Ty(), {}, true),
          GlobalValue::ExternalLinkage, "__gxx_personality_v0", module);

      mainFn->setPersonalityFn(personalityFn);

      auto BB = BasicBlock::Create(builder.getContext(), "entry", mainFn);
      builder.SetInsertPoint(BB);

      auto fn = builder.GetInsertBlock()->getParent();
      auto lpadBB = BasicBlock::Create(builder.getContext(), "lpad", fn);
      auto endBB = BasicBlock::Create(builder.getContext(), "end");
      builder.CreateInvoke(startFn, endBB, lpadBB);

      builder.SetInsertPoint(lpadBB);

      auto exc = builder.CreateLandingPad(
          StructType::get(builder.getPtrTy(), builder.getInt32Ty()), 1,
          "exc");

      exc->addClause(tyinfo);

      auto ptr = builder.CreateExtractValue(exc, {0}, "exc.ptr");
      auto sel = builder.CreateExtractValue(exc, {1}, "exc.sel");

      auto id = builder.CreateCall(
          cast<Function>(module
                             ->getOrInsertFunction("llvm.eh.typeid.for",
                                                   builder.getInt32Ty(),
                                                   builder.getPtrTy())
                             .getCallee()),
          {tyinfo}, "tid.int");

      auto catch_with_message =
          BasicBlock::Create(builder.getContext(), "catch_with_message", fn);
      auto catch_unknown = BasicBlock::Create(builder.getContext(), "catch_unknown", fn);
      auto cmp = builder.CreateCmp(CmpInst::ICMP_EQ, sel, id, "tst.int");
      builder.CreateCondBr(cmp, catch_with_message, catch_unknown);

      auto beginCatchFn = cast<Function>(
          module
              ->getOrInsertFunction("__cxa_begin_catch", builder.getPtrTy(),
                                    builder.getPtrTy())
              .getCallee());

      auto endCatchFn =
          module->getOrInsertFunction("__cxa_end_catch", builder.getVoidTy());

      auto putFn = module->getOrInsertFunction("puts", builder.getInt32Ty(),
                                                builder.getPtrTy());

      {
        builder.SetInsertPoint(catch_with_message);

        auto str = builder.CreateCall(beginCatchFn, ptr, "str");
        builder.CreateCall(putFn, str);
        builder.CreateCall(endCatchFn);
        builder.CreateBr(endBB);
      }

      {
        builder.SetInsertPoint(catch_unknown);

        builder.CreateCall(beginCatchFn, ptr);
        auto str =
            builder.CreateGlobalString("unknown error...", ".str.unknown");
        builder.CreateCall(putFn, str);
        builder.CreateCall(endCatchFn);
        builder.CreateBr(endBB);
      }

      {
        fn->insert(fn->end(), endBB);
        builder.SetInsertPoint(endBB);

        builder.CreateRetVoid();
      }

      verifyFunction(*mainFn);
    }
  }

  static void compile_block(IRBuilder<>& builder, Module* module,
                             GlobalVariable* tyinfo,
                             const shared_ptr<AstPL0> ast) {
    compile_const(builder, ast->nodes[0]);
    compile_var(builder, ast->nodes[1]);
    compile_procedure(builder, module, tyinfo, ast->nodes[2]);
    compile_statement(builder, module, tyinfo, ast->nodes[3]);
  }

  static void compile_const(IRBuilder<>& builder,
                             const shared_ptr<AstPL0> ast) {
    for (auto i = 0u; i < ast->nodes.size(); i += 2) {
      auto ident = ast->nodes[i]->token;
      auto number = ast->nodes[i + 1]->token_to_number<int>();

      auto alloca =
          builder.CreateAlloca(builder.getInt32Ty(), nullptr, ident);
      builder.CreateStore(builder.getInt32(number), alloca);
    }
  }

  static void compile_var(IRBuilder<>& builder,
                           const shared_ptr<AstPL0> ast) {
    for (const auto node : ast->nodes) {
      auto ident = node->token;
      builder.CreateAlloca(builder.getInt32Ty(), nullptr, ident);
    }
  }

  static void compile_procedure(IRBuilder<>& builder, Module* module,
                                 GlobalVariable* tyinfo,
                                 const shared_ptr<AstPL0> ast) {
    for (auto i = 0u; i < ast->nodes.size(); i += 2) {
      auto ident = ast->nodes[i]->token;
      auto block = ast->nodes[i + 1];

      std::vector<Type*> pt(block->scope->free_variables.size(),
                            builder.getPtrTy());
      auto fn = cast<Function>(
          module
              ->getOrInsertFunction(
                  ident, FunctionType::get(builder.getVoidTy(), pt, false))
              .getCallee());

      {
        auto it = block->scope->free_variables.begin();
        for (auto& arg : fn->args()) {
          auto& sv = *it;
          arg.setName(sv);
          ++it;
        }
      }

      {
        auto prevBB = builder.GetInsertBlock();
        auto BB = BasicBlock::Create(builder.getContext(), "entry", fn);
        builder.SetInsertPoint(BB);
        compile_block(builder, module, tyinfo, block);
        builder.CreateRetVoid();
        verifyFunction(*fn);
        builder.SetInsertPoint(prevBB);
      }
    }
  }

  static void compile_statement(IRBuilder<>& builder, Module* module,
                                 GlobalVariable* tyinfo,
                                 const shared_ptr<AstPL0> ast) {
    if (!ast->nodes.empty()) {
      compile_switch(builder, module, tyinfo, ast->nodes[0]);
    }
  }

  static void compile_assignment(IRBuilder<>& builder, Module* module,
                                  GlobalVariable* tyinfo,
                                  const shared_ptr<AstPL0> ast) {
    auto ident = ast->nodes[0]->token;

    auto fn = builder.GetInsertBlock()->getParent();
    auto tbl = fn->getValueSymbolTable();
    auto var = tbl->lookup(ident);
    if (!var) {
      throw_runtime_error(ast,
                          "'" + std::string(ident) + "' is not defined...");
    }

    auto val = compile_expression(builder, module, tyinfo, ast->nodes[1]);
    builder.CreateStore(val, var);
  }

  static void compile_call(IRBuilder<>& builder, Module* module,
                            const shared_ptr<AstPL0> ast) {
    auto ident = ast->nodes[0]->token;

    auto scope = get_closest_scope(ast);
    auto block = scope->get_procedure(ident);

    std::vector<Value*> args;
    for (auto& free : block->scope->free_variables) {
      auto fn = builder.GetInsertBlock()->getParent();
      auto tbl = fn->getValueSymbolTable();
      auto var = tbl->lookup(free);
      if (!var) {
        throw_runtime_error(ast,
                            "'" + std::string(free) + "' is not defined...");
      }
      args.push_back(var);
    }

    auto fn = module->getFunction(ident);
    builder.CreateCall(fn, args);
  }

  static void compile_statements(IRBuilder<>& builder, Module* module,
                                  GlobalVariable* tyinfo,
                                  const shared_ptr<AstPL0> ast) {
    for (auto node : ast->nodes) {
      compile_statement(builder, module, tyinfo, node);
    }
  }

  static void compile_if(IRBuilder<>& builder, Module* module,
                          GlobalVariable* tyinfo,
                          const shared_ptr<AstPL0> ast) {
    auto cond = compile_condition(builder, module, tyinfo, ast->nodes[0]);

    auto fn = builder.GetInsertBlock()->getParent();
    auto ifTenBB = BasicBlock::Create(builder.getContext(), "if.then", fn);
    auto ifEndBB = BasicBlock::Create(builder.getContext(), "if.end");

    builder.CreateCondBr(cond, ifTenBB, ifEndBB);

    builder.SetInsertPoint(ifTenBB);
    compile_statement(builder, module, tyinfo, ast->nodes[1]);
    builder.CreateBr(ifEndBB);

    fn->insert(fn->end(), ifEndBB);
    builder.SetInsertPoint(ifEndBB);
  }

  static void compile_while(IRBuilder<>& builder, Module* module,
                             GlobalVariable* tyinfo,
                             const shared_ptr<AstPL0> ast) {
    auto whileCondBB = BasicBlock::Create(builder.getContext(), "while.cond");
    builder.CreateBr(whileCondBB);

    auto fn = builder.GetInsertBlock()->getParent();
    fn->insert(fn->end(), whileCondBB);
    builder.SetInsertPoint(whileCondBB);

    auto cond = compile_condition(builder, module, tyinfo, ast->nodes[0]);

    auto whileBodyBB = BasicBlock::Create(builder.getContext(), "while.body", fn);
    auto whileEndBB = BasicBlock::Create(builder.getContext(), "while.end");
    builder.CreateCondBr(cond, whileBodyBB, whileEndBB);

    builder.SetInsertPoint(whileBodyBB);
    compile_statement(builder, module, tyinfo, ast->nodes[1]);

    builder.CreateBr(whileCondBB);

    fn->insert(fn->end(), whileEndBB);
    builder.SetInsertPoint(whileEndBB);
  }

  static Value* compile_condition(IRBuilder<>& builder, Module* module,
                                   GlobalVariable* tyinfo,
                                   const shared_ptr<AstPL0> ast) {
    return compile_switch_value(builder, module, tyinfo, ast->nodes[0]);
  }

  static Value* compile_odd(IRBuilder<>& builder, Module* module,
                             GlobalVariable* tyinfo,
                             const shared_ptr<AstPL0> ast) {
    auto val = compile_expression(builder, module, tyinfo, ast->nodes[0]);
    return builder.CreateICmpNE(val, builder.getInt32(0), "icmpne");
  }

  static Value* compile_compare(IRBuilder<>& builder, Module* module,
                                 GlobalVariable* tyinfo,
                                 const shared_ptr<AstPL0> ast) {
    auto lhs = compile_expression(builder, module, tyinfo, ast->nodes[0]);
    auto rhs = compile_expression(builder, module, tyinfo, ast->nodes[2]);

    auto ope = ast->nodes[1]->token;
    switch (ope[0]) {
      case '=':
        return builder.CreateICmpEQ(lhs, rhs, "icmpeq");
      case '#':
        return builder.CreateICmpNE(lhs, rhs, "icmpne");
      case '<':
        if (ope.size() == 1) {
          return builder.CreateICmpSLT(lhs, rhs, "icmpslt");
        }
        // '<='
        return builder.CreateICmpSLE(lhs, rhs, "icmpsle");
      case '>':
        if (ope.size() == 1) {
          return builder.CreateICmpSGT(lhs, rhs, "icmpsgt");
        }
        // '>='
        return builder.CreateICmpSGE(lhs, rhs, "icmpsge");
    }
    return nullptr;
  }

  static void compile_out(IRBuilder<>& builder, Module* module,
                           GlobalVariable* tyinfo,
                           const shared_ptr<AstPL0> ast) {
    auto val = compile_expression(builder, module, tyinfo, ast->nodes[0]);
    auto fn = module->getFunction("out");
    builder.CreateCall(fn, val);
  }

  static Value* compile_expression(IRBuilder<>& builder, Module* module,
                                    GlobalVariable* tyinfo,
                                    const shared_ptr<AstPL0> ast) {
    const auto& nodes = ast->nodes;

    auto sign = nodes[0]->token;
    auto negative = !(sign.empty() || sign == "+");

    auto val = compile_term(builder, module, tyinfo, nodes[1]);
    if (negative) {
      val = builder.CreateNeg(val, "negative");
    }

    for (auto i = 2u; i < nodes.size(); i += 2) {
      auto ope = nodes[i + 0]->token[0];
      auto rval = compile_term(builder, module, tyinfo, nodes[i + 1]);
      switch (ope) {
        case '+':
          val = builder.CreateAdd(val, rval, "add");
          break;
        case '-':
          val = builder.CreateSub(val, rval, "sub");
          break;
      }
    }
    return val;
  }

  static Value* compile_term(IRBuilder<>& builder, Module* module,
                              GlobalVariable* tyinfo,
                              const shared_ptr<AstPL0> ast) {
    const auto& nodes = ast->nodes;
    auto val = compile_factor(builder, module, tyinfo, nodes[0]);
    for (auto i = 1u; i < nodes.size(); i += 2) {
      auto ope = nodes[i + 0]->token[0];
      auto rval = compile_switch_value(builder, module, tyinfo, nodes[i + 1]);
      switch (ope) {
        case '*':
          val = builder.CreateMul(val, rval, "mul");
          break;
        case '/': {
          // Zero divide check
          auto cond =
              builder.CreateICmpEQ(rval, builder.getInt32(0), "icmpeq");

          auto fn = builder.GetInsertBlock()->getParent();
          auto ifZeroBB = BasicBlock::Create(builder.getContext(), "zdiv.zero", fn);
          auto ifNonZeroBB = BasicBlock::Create(builder.getContext(), "zdiv.non_zero");
          builder.CreateCondBr(cond, ifZeroBB, ifNonZeroBB);

          // zero
          {
            builder.SetInsertPoint(ifZeroBB);

            Value* eh = nullptr;
            {
              auto fn = cast<Function>(
                  module
                      ->getOrInsertFunction("__cxa_allocate_exception",
                                            builder.getPtrTy(),
                                            builder.getInt64Ty())
                      .getCallee());

              eh = builder.CreateCall(fn, builder.getInt64(8), "eh");

              auto msg = builder.CreateGlobalString(
                  "divide by 0", ".str.zero_divide");

              builder.CreateStore(msg, eh);
            }

            {
              auto fn = cast<Function>(
                  module
                      ->getOrInsertFunction("__cxa_throw", builder.getVoidTy(),
                                            builder.getPtrTy(),
                                            builder.getPtrTy(),
                                            builder.getPtrTy())
                      .getCallee());

              builder.CreateCall(
                  fn,
                  {eh, tyinfo, ConstantPointerNull::get(builder.getPtrTy())});
            }

            builder.CreateUnreachable();
          }

          // no_zero
          {
            fn->insert(fn->end(), ifNonZeroBB);
            builder.SetInsertPoint(ifNonZeroBB);
            val = builder.CreateSDiv(val, rval, "div");
          }
          break;
        }
      }
    }
    return val;
  }

  static Value* compile_factor(IRBuilder<>& builder, Module* module,
                                GlobalVariable* tyinfo,
                                const shared_ptr<AstPL0> ast) {
    return compile_switch_value(builder, module, tyinfo, ast->nodes[0]);
  }

  static Value* compile_ident(IRBuilder<>& builder,
                               const shared_ptr<AstPL0> ast) {
    auto ident = ast->token;

    auto fn = builder.GetInsertBlock()->getParent();
    auto tbl = fn->getValueSymbolTable();
    auto var = tbl->lookup(ident);
    if (!var) {
      throw_runtime_error(ast,
                          "'" + std::string(ident) + "' is not defined...");
    }

    return builder.CreateLoad(builder.getInt32Ty(), var);
  }

  static Value* compile_number(IRBuilder<>& builder,
                                const shared_ptr<AstPL0> ast) {
    return ConstantInt::getIntegerValue(builder.getInt32Ty(),
                                        APInt(32, ast->token, 10));
  }
};

int main(int argc, const char** argv) {
  if (argc < 2) {
    cout << "usage: pl0 file [-emit-llvm]" << endl;
    return 1;
  }

  // Source file path
  auto path = argv[1];
  auto emit_llvm = (argc >= 3 && !strcmp("-emit-llvm", argv[2]));

  // Read a source file into memory
  vector<char> source;
  ifstream ifs(path, ios::in | ios::binary);
  if (ifs.fail()) {
    cerr << "can't open the source file." << endl;
    return -1;
  }

  source.resize(static_cast<unsigned int>(ifs.seekg(0, ios::end).tellg()));
  if (!source.empty()) {
    ifs.seekg(0, ios::beg)
        .read(&source[0], static_cast<streamsize>(source.size()));
  }

  // Setup a PEG parser
  parser parser(grammar);
  parser.enable_ast<AstPL0>();
  parser.set_logger([&](size_t ln, size_t col, const string& msg) {
    cerr << format_error_message(path, ln, col, msg) << endl;
  });

  // Parse the source and make an AST
  shared_ptr<AstPL0> ast;
  if (parser.parse_n(source.data(), source.size(), ast, path)) {
    try {
      // Make a symbol table on the AST
      SymbolTableBuilder::build_on_ast(ast);

      // JIT compile and execute
      JIT::run(ast, emit_llvm);
    } catch (const runtime_error& e) {
      cerr << e.what() << endl;
    }
    return 0;
  }

  return -1;
}
