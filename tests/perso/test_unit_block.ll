; Target
target triple = "x86_64-unknown-linux-gnu"
; External declaration of the printf function
declare i32 @printf(i8* noalias nocapture, ...)
declare i32 @scanf(i8* noalias nocapture, ...)

; Actual code begins

@read1 = global [3 x i8] c"%d\00"

define i32 @main() {
%a = alloca i32
%b = alloca i32

store i32 1, i32* %a
store i32 2, i32* %b
ret i32 0
}

