; Target
target triple = "x86_64-unknown-linux-gnu"
; External declaration of the printf function
declare i32 @printf(i8* noalias nocapture, ...)
declare i32 @scanf(i8* noalias nocapture, ...)

; Actual code begins

@read1 = global [3 x i8] c"%d\00"

define i32 @main() {
%a = alloca i32

store i32 0, i32* %a
br label %While1
While1:
%tmp1 = load i32, i32* %a
%tmp2 = icmp ne i32 %tmp1, 0
br i1 %tmp2, label %Bloc2, label %EndWhile3
Bloc2:
store i32 1, i32* %a
br label %While1
EndWhile3:
ret i32 0
}

