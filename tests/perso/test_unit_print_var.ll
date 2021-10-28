; Target
target triple = "x86_64-unknown-linux-gnu"
; External declaration of the printf function
declare i32 @printf(i8* noalias nocapture, ...)
declare i32 @scanf(i8* noalias nocapture, ...)

; Actual code begins

@read1 = global [3 x i8] c"%d\00"
@.fmt2 = global [3 x i8] c"%d\00"

define i32 @main() {
%a = alloca i32

call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @read1, i64 0, i64 0), i32* %a )
%tmp1 = load i32, i32* %a
call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.fmt2, i64 0, i64 0), i32 %tmp1 )
ret i32 0
}

