; Target
target triple = "x86_64-unknown-linux-gnu"
; External declaration of the printf function
declare i32 @printf(i8* noalias nocapture, ...)
declare i32 @scanf(i8* noalias nocapture, ...)

; Actual code begins

@read1 = global [3 x i8] c"%d\00"
@.fmt2 = global [14 x i8] c"Hello World!\0A\00"

define i32 @main() {
%a = alloca i32

call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.fmt2, i64 0, i64 0) )
ret i32 0
}

