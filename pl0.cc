//
//  pl0.cc - PL/0 JIT compiler
//
//  Copyright © 2018 Yuji Hirose. All rights reserved.
//  MIT License
//

#include <peglib.h>

#include <fstream>
#include <sstream>

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/MCJIT.h"
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

inline llvm::StringRef to_StringRef(const std::string_view& sv) {
  return llvm::StringRef(sv.data(), sv.size());
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

inline void throw_runtime_error(const shared_ptr<AstPL0> node, const string& msg) {
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
  static void run(const shared_ptr<AstPL0> ast) {
    JIT jit;
    jit.compile(ast);
    jit.exec();
  }

 private:
  LLVMContext context_;
  IRBuilder<> builder_;
  unique_ptr<Module> module_;

  JIT() : builder_(context_) { module_ = make_unique<Module>("pl0", context_); }

  void compile(const shared_ptr<AstPL0> ast) {
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    compile_libs();
    compile_program(ast);
  }

  void exec() {
    unique_ptr<ExecutionEngine> ee(EngineBuilder(std::move(module_)).create());
    std::vector<GenericValue> noargs;
    auto fn = ee->FindFunctionNamed("main");
    auto ret = ee->runFunction(fn, noargs);
  }

  void compile_switch(const shared_ptr<AstPL0> ast) {
    switch (ast->tag) {
      case "assignment"_:
        compile_assignment(ast);
        break;
      case "call"_:
        compile_call(ast);
        break;
      case "statements"_:
        compile_statements(ast);
        break;
      case "if"_:
        compile_if(ast);
        break;
      case "while"_:
        compile_while(ast);
        break;
      case "out"_:
        compile_out(ast);
        break;
      default:
        compile_switch(ast->nodes[0]);
        break;
    }
  }

  Value* compile_switch_value(const shared_ptr<AstPL0> ast) {
    switch (ast->tag) {
      case "odd"_:
        return compile_odd(ast);
      case "compare"_:
        return compile_compare(ast);
      case "expression"_:
        return compile_expression(ast);
      case "ident"_:
        return compile_ident(ast);
      case "number"_:
        return compile_number(ast);
      default:
        return compile_switch_value(ast->nodes[0]);
    }
  }

  void compile_libs() {
    auto printfF = module_->getOrInsertFunction(
        "printf",
        FunctionType::get(builder_.getInt32Ty(),
                          PointerType::get(builder_.getInt8Ty(), 0), true));

    auto funccallee = module_->getOrInsertFunction("out", builder_.getVoidTy(),
                                                   builder_.getInt32Ty());
    auto outC = funccallee.getCallee();
    auto outF = cast<Function>(outC);

    {
      auto BB = BasicBlock::Create(context_, "entry", outF);
      builder_.SetInsertPoint(BB);

      auto val = &*outF->arg_begin();

      auto fmt = builder_.CreateGlobalStringPtr("%d\n");
      std::vector<Value*> args = {fmt, val};
      builder_.CreateCall(printfF, args);

      builder_.CreateRetVoid();
    }
  }

  void compile_program(const shared_ptr<AstPL0> ast) {
    auto funccallee =
        module_->getOrInsertFunction("main", builder_.getVoidTy());
    auto c = funccallee.getCallee();
    auto fn = cast<Function>(c);

    {
      auto BB = BasicBlock::Create(context_, "entry", fn);
      builder_.SetInsertPoint(BB);
      compile_block(ast->nodes[0]);
      builder_.CreateRetVoid();
      verifyFunction(*fn);
    }
  }

  void compile_block(const shared_ptr<AstPL0> ast) {
    compile_const(ast->nodes[0]);
    compile_var(ast->nodes[1]);
    compile_procedure(ast->nodes[2]);
    compile_statement(ast->nodes[3]);
  }

  void compile_const(const shared_ptr<AstPL0> ast) {
    for (auto i = 0u; i < ast->nodes.size(); i += 2) {
      auto ident = ast->nodes[i]->token;
      auto number = ast->nodes[i + 1]->token_to_number<int>();

      auto alloca = builder_.CreateAlloca(builder_.getInt32Ty(), nullptr,
                                          to_StringRef(ident));
      builder_.CreateStore(builder_.getInt32(number), alloca);
    }
  }

  void compile_var(const shared_ptr<AstPL0> ast) {
    for (const auto node : ast->nodes) {
      auto ident = node->token;
      builder_.CreateAlloca(builder_.getInt32Ty(), nullptr,
                            to_StringRef(ident));
    }
  }

  void compile_procedure(const shared_ptr<AstPL0> ast) {
    for (auto i = 0u; i < ast->nodes.size(); i += 2) {
      auto ident = ast->nodes[i]->token;
      auto block = ast->nodes[i + 1];

      std::vector<Type*> pt(block->scope->free_variables.size(),
                            Type::getInt32PtrTy(context_));
      auto ft = FunctionType::get(builder_.getVoidTy(), pt, false);
      auto funccallee = module_->getOrInsertFunction(to_StringRef(ident), ft);
      auto c = funccallee.getCallee();
      auto fn = cast<Function>(c);

      {
        auto it = block->scope->free_variables.begin();
        for (auto& arg : fn->args()) {
          auto& sv = *it;
          arg.setName(to_StringRef(sv));
          ++it;
        }
      }

      {
        auto prevBB = builder_.GetInsertBlock();
        auto BB = BasicBlock::Create(context_, "entry", fn);
        builder_.SetInsertPoint(BB);
        compile_block(block);
        builder_.CreateRetVoid();
        verifyFunction(*fn);
        builder_.SetInsertPoint(prevBB);
      }
    }
  }

  void compile_statement(const shared_ptr<AstPL0> ast) {
    if (!ast->nodes.empty()) {
      compile_switch(ast->nodes[0]);
    }
  }

  void compile_assignment(const shared_ptr<AstPL0> ast) {
    auto ident = ast->nodes[0]->token;

    auto fn = builder_.GetInsertBlock()->getParent();
    auto tbl = fn->getValueSymbolTable();
    auto var = tbl->lookup(to_StringRef(ident));
    if (!var) {
      throw_runtime_error(ast,
                          "'" + std::string(ident) + "' is not defined...");
    }

    auto val = compile_expression(ast->nodes[1]);
    builder_.CreateStore(val, var);
  }

  void compile_call(const shared_ptr<AstPL0> ast) {
    auto ident = ast->nodes[0]->token;

    auto scope = get_closest_scope(ast);
    auto block = scope->get_procedure(ident);

    std::vector<Value*> args;
    for (auto& free : block->scope->free_variables) {
      auto fn = builder_.GetInsertBlock()->getParent();
      auto tbl = fn->getValueSymbolTable();
      auto var = tbl->lookup(to_StringRef(free));
      if (!var) {
        throw_runtime_error(ast,
                            "'" + std::string(free) + "' is not defined...");
      }
      args.push_back(var);
    }

    auto fn = module_->getFunction(to_StringRef(ident));
    builder_.CreateCall(fn, args);
  }

  void compile_statements(const shared_ptr<AstPL0> ast) {
    for (auto node : ast->nodes) {
      compile_statement(node);
    }
  }

  void compile_if(const shared_ptr<AstPL0> ast) {
    auto cond = compile_condition(ast->nodes[0]);

    auto fn = builder_.GetInsertBlock()->getParent();
    auto ifThen = BasicBlock::Create(context_, "if.then", fn);
    auto ifEnd = BasicBlock::Create(context_, "if.end");

    builder_.CreateCondBr(cond, ifThen, ifEnd);

    builder_.SetInsertPoint(ifThen);
    compile_statement(ast->nodes[1]);
    builder_.CreateBr(ifEnd);

    fn->getBasicBlockList().push_back(ifEnd);
    builder_.SetInsertPoint(ifEnd);
  }

  void compile_while(const shared_ptr<AstPL0> ast) {
    auto whileCond = BasicBlock::Create(context_, "while.cond");
    builder_.CreateBr(whileCond);

    auto fn = builder_.GetInsertBlock()->getParent();
    fn->getBasicBlockList().push_back(whileCond);
    builder_.SetInsertPoint(whileCond);

    auto cond = compile_condition(ast->nodes[0]);

    auto whileBody = BasicBlock::Create(context_, "while.body", fn);
    auto whileEnd = BasicBlock::Create(context_, "while.end");
    builder_.CreateCondBr(cond, whileBody, whileEnd);

    builder_.SetInsertPoint(whileBody);
    compile_statement(ast->nodes[1]);

    builder_.CreateBr(whileCond);

    fn->getBasicBlockList().push_back(whileEnd);
    builder_.SetInsertPoint(whileEnd);
  }

  Value* compile_condition(const shared_ptr<AstPL0> ast) {
    return compile_switch_value(ast->nodes[0]);
  }

  Value* compile_odd(const shared_ptr<AstPL0> ast) {
    auto val = compile_expression(ast->nodes[0]);
    return builder_.CreateICmpNE(val, builder_.getInt32(0), "icmpne");
  }

  Value* compile_compare(const shared_ptr<AstPL0> ast) {
    auto lhs = compile_expression(ast->nodes[0]);
    auto rhs = compile_expression(ast->nodes[2]);

    auto ope = ast->nodes[1]->token;
    switch (ope[0]) {
      case '=':
        return builder_.CreateICmpEQ(lhs, rhs, "icmpeq");
      case '#':
        return builder_.CreateICmpNE(lhs, rhs, "icmpne");
      case '<':
        if (ope.size() == 1) {
          return builder_.CreateICmpSLT(lhs, rhs, "icmpslt");
        }
        // '<='
        return builder_.CreateICmpSLE(lhs, rhs, "icmpsle");
      case '>':
        if (ope.size() == 1) {
          return builder_.CreateICmpSGT(lhs, rhs, "icmpsgt");
        }
        // '>='
        return builder_.CreateICmpSGE(lhs, rhs, "icmpsge");
    }
    return nullptr;
  }

  void compile_out(const shared_ptr<AstPL0> ast) {
    auto val = compile_expression(ast->nodes[0]);
    auto outF = module_->getFunction("out");
    builder_.CreateCall(outF, val);
  }

  Value* compile_expression(const shared_ptr<AstPL0> ast) {
    const auto& nodes = ast->nodes;

    auto sign = nodes[0]->token;
    auto negative = !(sign.empty() || sign == "+");

    auto val = compile_term(nodes[1]);
    if (negative) {
      val = builder_.CreateNeg(val, "negative");
    }

    for (auto i = 2u; i < nodes.size(); i += 2) {
      auto ope = nodes[i + 0]->token[0];
      auto rval = compile_term(nodes[i + 1]);
      switch (ope) {
        case '+':
          val = builder_.CreateAdd(val, rval, "add");
          break;
        case '-':
          val = builder_.CreateSub(val, rval, "sub");
          break;
      }
    }
    return val;
  }

  Value* compile_term(const shared_ptr<AstPL0> ast) {
    const auto& nodes = ast->nodes;
    auto val = compile_factor(nodes[0]);
    for (auto i = 1u; i < nodes.size(); i += 2) {
      auto ope = nodes[i + 0]->token[0];
      auto rval = compile_switch_value(nodes[i + 1]);
      switch (ope) {
        case '*':
          val = builder_.CreateMul(val, rval, "mul");
          break;
        case '/': {
          // TODO: Zero devide error?
          // auto ret = builder_.CreateICmpEQ(rval, builder_.getInt32(0),
          // "icmpeq");
          // if (!ret) {
          //   throw_runtime_error(ast, "divide by 0 error");
          // }
          val = builder_.CreateSDiv(val, rval, "div");
          break;
        }
      }
    }
    return val;
  }

  Value* compile_factor(const shared_ptr<AstPL0> ast) {
    return compile_switch_value(ast->nodes[0]);
  }

  Value* compile_ident(const shared_ptr<AstPL0> ast) {
    auto ident = ast->token;

    auto fn = builder_.GetInsertBlock()->getParent();
    auto tbl = fn->getValueSymbolTable();
    auto var = tbl->lookup(to_StringRef(ident));
    if (!var) {
      throw_runtime_error(ast,
                          "'" + std::string(ident) + "' is not defined...");
    }

    return builder_.CreateLoad(builder_.getInt32Ty(), var);
  }

  Value* compile_number(const shared_ptr<AstPL0> ast) {
    return ConstantInt::getIntegerValue(
        builder_.getInt32Ty(), APInt(32, to_StringRef(ast->token), 10));
  }
};

int main(int argc, const char** argv) {
  if (argc < 2) {
    cout << "usage: pl0 file" << endl;
    return 1;
  }

  // Source file path
  auto path = argv[1];

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
  parser.log = [&](size_t ln, size_t col, const string& msg) {
    cerr << format_error_message(path, ln, col, msg) << endl;
  };

  // Parse the source and make an AST
  shared_ptr<AstPL0> ast;
  if (parser.parse_n(source.data(), source.size(), ast, path)) {
    try {
      // Make a symbol table on the AST
      SymbolTableBuilder::build_on_ast(ast);

      // JIT compile and execute
      JIT::run(ast);
    } catch (const runtime_error& e) {
      cerr << e.what() << endl;
    }
    return 0;
  }

  return -1;
}
