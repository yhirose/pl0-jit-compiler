;------------------------------------------------------------------------------
; Devide by Zero sample
;------------------------------------------------------------------------------

@_ZTIPKc = external constant i8*
declare i32 @puts(i8*) #3
declare i32 @printf(i8*, ...)

;------------------------------------------------------------------------------
; out
;------------------------------------------------------------------------------

@.printf.fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"

define void @out(i32 %0) {
entry:
  call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.printf.fmt, i32 0, i32 0), i32 %0)
  ret void
}

;------------------------------------------------------------------------------
; divide
;------------------------------------------------------------------------------

declare i8* @__cxa_allocate_exception(i64)
declare void @__cxa_throw(i8*, i8*, i8*)

@.str.zero_divide = private unnamed_addr constant [12 x i8] c"divide by 0\00"

; define void @divide(i32* %x) {
; entry:
;   %0 = load i32, i32* %x, align 4
;   %div = sdiv i32 100, %0
;   call void @out(i32 %div)
;   ret void
; }

define void @divide(i32* %x) {
entry:
  %0 = load i32, i32* %x, align 4
  ; zero-divide check
  %cmp = icmp eq i32 %0, 0
  br i1 %cmp, label %zdiv.zero, label %zdiv.nonzero

zdiv.zero:
  %eh = call i8* @__cxa_allocate_exception(i64 8)
  %payload = bitcast i8* %eh to i8**
  store i8* getelementptr ([12 x i8], [12 x i8]* @.str.zero_divide, i64 0, i64 0), i8** %payload
  %msg = bitcast [12 x i8]* @.str.zero_divide to i8*
  store i8* %msg, i8** %payload
  call void @__cxa_throw(i8* %eh, i8* bitcast (i8** @_ZTIPKc to i8*), i8* null)
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
  store i32 0, i32* %x, align 4
  call void @divide(i32* %x)
  ret void
}

;------------------------------------------------------------------------------
; main
;------------------------------------------------------------------------------

declare i32 @__gxx_personality_v0(...)
declare i32 @llvm.eh.typeid.for(i8*)
declare i8* @__cxa_begin_catch(i8*)
declare void @__cxa_end_catch()

@.str.unknown = private unnamed_addr constant [17 x i8] c"unknown error...\00"

define void @main() personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
entry:
  invoke void @__pl0_start() to label %end unwind label %lpad

lpad:
  %exc = landingpad { i8*, i32 }
          catch i8* bitcast (i8** @_ZTIPKc to i8*)
  %exc.ptr = extractvalue { i8*, i32 } %exc, 0
  %exc.sel = extractvalue { i8*, i32 } %exc, 1
  %tid.int = call i32 @llvm.eh.typeid.for(i8* bitcast (i8** @_ZTIPKc to i8*))
  %tst.int = icmp eq i32 %exc.sel, %tid.int
  br i1 %tst.int, label %catch_with_message, label %catch_unknown

catch_with_message:
  %str = call i8* @__cxa_begin_catch(i8* %exc.ptr)
  call i32 @puts(i8* %str)
  call void @__cxa_end_catch()
  br label %end

catch_unknown:
  call i8* @__cxa_begin_catch(i8* %exc.ptr)
  call i32 @puts(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.unknown, i64 0, i64 0))
  call void @__cxa_end_catch()
  br label %end

end:
  ret void
}
