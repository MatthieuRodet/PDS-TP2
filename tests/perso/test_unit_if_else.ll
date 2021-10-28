; Target
target triple = "x86_64-unknown-linux-gnu"
; External declaration of the printf function
declare i32 @printf(i8* noalias nocapture, ...)
declare i32 @scanf(i8* noalias nocapture, ...)

; Actual code begins

@read1 = global [3 x i8] c"%d\00"

define i32 @main() {
%a = alloca i32

%tmp1 = load i32, i32* %a
%tmp2 = icmp ne i32 %tmp1, 0
br i1 %tmp2, label %Then1, label %Else2
Then1:
store i32 2, i32* %a
br label %Endif3
Else2:
store i32 3, i32* %a
br label %Endif3
Endif3:
ret i32 0
}

