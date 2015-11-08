@dnl = internal constant [4 x i8] c"%d\0A\00"
declare i32 @printf(i8*, ...)
define void @printInt(i32 %x) {
entry: %t0 = getelementptr [4 x i8]* @dnl, i32 0, i32 0 call i32 (i8*, ...)* @printf(i8* %t0, i32 %x)
 ret void
}

; Function Attrs: nounwind ssp uwtable
define i32 @main(i32 %argc, i8** %argv) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8**, align 8
  store i32 0, i32* %1
  store i32 %argc, i32* %2, align 4
  store i8** %argv, i8*** %3, align 8
  %a = alloca i32, align 4
  %4 = add nsw i32 1,1
  %5 = add nsw i32 1,%4
  %6 = add nsw i32 1,%5
  %7 = add nsw i32 1,%6
  call void @printInt(i32 %7)
  store i32 44, i32* %a, align 4
  %8 = load i32* %a, align 4
  %9 = add nsw i32 %8,3
  call void @printInt(i32 %9)
  ret i32 0
}
