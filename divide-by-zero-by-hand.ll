;------------------------------------------------------------------------------
; Divide by Zero sample
;------------------------------------------------------------------------------

@_ZTIPKc = external constant ptr
declare i32 @puts(ptr) cold
declare i32 @printf(ptr, ...) nounwind

;------------------------------------------------------------------------------
; out
;------------------------------------------------------------------------------

@.printf.fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define void @out(i32 %0) {
entry:
  call i32 (ptr, ...) @printf(ptr @.printf.fmt, i32 %0)
  ret void
}

;------------------------------------------------------------------------------
; divide
;------------------------------------------------------------------------------

declare ptr @__cxa_allocate_exception(i64) nounwind
declare void @__cxa_throw(ptr, ptr, ptr) noreturn nounwind

@.str.zero_divide = private unnamed_addr constant [12 x i8] c"divide by 0\00"

define void @divide(ptr %x) {
entry:
  %0 = load i32, ptr %x, align 4
  ; zero-divide check
  %cmp = icmp eq i32 %0, 0
  br i1 %cmp, label %zdiv.zero, label %zdiv.nonzero, !prof !1

zdiv.zero:
  %eh = call ptr @__cxa_allocate_exception(i64 8)
  store ptr @.str.zero_divide, ptr %eh, align 8
  call void @__cxa_throw(ptr %eh, ptr @_ZTIPKc, ptr null)
  unreachable

zdiv.nonzero:
  %div = sdiv i32 100, %0
  call void @out(i32 %div)
  ret void
}

;------------------------------------------------------------------------------
; start
;------------------------------------------------------------------------------

define void @__pl0_start() {
entry:
  %x = alloca i32, align 4
  store i32 0, ptr %x, align 4
  call void @divide(ptr %x)
  ret void
}

;------------------------------------------------------------------------------
; main
;------------------------------------------------------------------------------

declare i32 @__gxx_personality_v0(...) 
declare i32 @llvm.eh.typeid.for(ptr) nounwind
declare ptr @__cxa_begin_catch(ptr) nounwind
declare void @__cxa_end_catch() nounwind

@.str.unknown = private unnamed_addr constant [17 x i8] c"unknown error...\00"

define void @main() personality ptr @__gxx_personality_v0 {
entry:
  invoke void @__pl0_start() to label %end unwind label %lpad

lpad:
  %exc = landingpad { ptr, i32 }
          catch ptr @_ZTIPKc
  %exc.ptr = extractvalue { ptr, i32 } %exc, 0
  %exc.sel = extractvalue { ptr, i32 } %exc, 1
  %tid.int = call i32 @llvm.eh.typeid.for(ptr @_ZTIPKc)
  %tst.int = icmp eq i32 %exc.sel, %tid.int
  br i1 %tst.int, label %catch_with_message, label %catch_unknown, !prof !2

catch_with_message:
  %str = call ptr @__cxa_begin_catch(ptr %exc.ptr)
  call i32 @puts(ptr %str)
  call void @__cxa_end_catch()
  br label %end

catch_unknown:
  call ptr @__cxa_begin_catch(ptr %exc.ptr)
  call i32 @puts(ptr @.str.unknown)
  call void @__cxa_end_catch()
  br label %end

end:
  ret void
}

;------------------------------------------------------------------------------
; Metadata
;------------------------------------------------------------------------------

!1 = !{!"branch_weights", i32 1, i32 1000}
!2 = !{!"branch_weights", i32 1000, i32 1}
