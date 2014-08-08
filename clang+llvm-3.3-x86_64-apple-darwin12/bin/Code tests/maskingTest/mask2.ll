; ModuleID = 'mask2.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.9.0"

%struct.Masktype = type { %struct.Masktype*, i32 }

@vtable = global %struct.Masktype* null, align 8
@.str = private unnamed_addr constant [7 x i8] c"a: %d\0A\00", align 1
@.str1 = private unnamed_addr constant [7 x i8] c"b: %d\0A\00", align 1
@.str2 = private unnamed_addr constant [7 x i8] c"c: %d\0A\00", align 1
@.str3 = private unnamed_addr constant [7 x i8] c"d: %d\0A\00", align 1

; Function Attrs: nounwind ssp uwtable
define i32 @mask(i32 %val) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %current = alloca %struct.Masktype*, align 8
  %index = alloca i32, align 4
  store i32 %val, i32* %2, align 4
  %3 = load %struct.Masktype** @vtable, align 8
  %4 = icmp eq %struct.Masktype* %3, null
  br i1 %4, label %5, label %8

; <label>:5                                       ; preds = %0
  %6 = call i8* @malloc(i64 16)
  %7 = bitcast i8* %6 to %struct.Masktype*
  store %struct.Masktype* %7, %struct.Masktype** @vtable, align 8
  br label %8

; <label>:8                                       ; preds = %5, %0
  %9 = load %struct.Masktype** @vtable, align 8
  store %struct.Masktype* %9, %struct.Masktype** %current, align 8
  store i32 0, i32* %index, align 4
  br label %10

; <label>:10                                      ; preds = %8, %31
  %11 = load %struct.Masktype** %current, align 8
  %12 = getelementptr inbounds %struct.Masktype* %11, i32 0, i32 1
  %13 = load i32* %12, align 4
  %14 = load i32* %2, align 4
  %15 = icmp eq i32 %13, %14
  br i1 %15, label %16, label %18

; <label>:16                                      ; preds = %10
  %17 = load i32* %index, align 4
  store i32 %17, i32* %1
  br label %43

; <label>:18                                      ; preds = %10
  %19 = load i32* %index, align 4
  %20 = add nsw i32 %19, 1
  store i32 %20, i32* %index, align 4
  %21 = load %struct.Masktype** %current, align 8
  %22 = getelementptr inbounds %struct.Masktype* %21, i32 0, i32 0
  %23 = load %struct.Masktype** %22, align 8
  %24 = icmp eq %struct.Masktype* %23, null
  br i1 %24, label %25, label %26

; <label>:25                                      ; preds = %18
  br label %32

; <label>:26                                      ; preds = %18
  %27 = load %struct.Masktype** %current, align 8
  %28 = getelementptr inbounds %struct.Masktype* %27, i32 0, i32 0
  %29 = load %struct.Masktype** %28, align 8
  store %struct.Masktype* %29, %struct.Masktype** %current, align 8
  br label %30

; <label>:30                                      ; preds = %26
  br label %31

; <label>:31                                      ; preds = %30
  br label %10

; <label>:32                                      ; preds = %25
  %33 = call i8* @malloc(i64 16)
  %34 = bitcast i8* %33 to %struct.Masktype*
  %35 = load %struct.Masktype** %current, align 8
  %36 = getelementptr inbounds %struct.Masktype* %35, i32 0, i32 0
  store %struct.Masktype* %34, %struct.Masktype** %36, align 8
  %37 = load i32* %2, align 4
  %38 = load %struct.Masktype** %current, align 8
  %39 = getelementptr inbounds %struct.Masktype* %38, i32 0, i32 0
  %40 = load %struct.Masktype** %39, align 8
  %41 = getelementptr inbounds %struct.Masktype* %40, i32 0, i32 1
  store i32 %37, i32* %41, align 4
  %42 = load i32* %index, align 4
  store i32 %42, i32* %1
  br label %43

; <label>:43                                      ; preds = %32, %16
  %44 = load i32* %1
  ret i32 %44
}

declare i8* @malloc(i64) #1

; Function Attrs: nounwind ssp uwtable
define i32 @unmask(i32 %index) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %current = alloca %struct.Masktype*, align 8
  store i32 %index, i32* %2, align 4
  %3 = load %struct.Masktype** @vtable, align 8
  store %struct.Masktype* %3, %struct.Masktype** %current, align 8
  br label %4

; <label>:4                                       ; preds = %20, %0
  %5 = load %struct.Masktype** %current, align 8
  %6 = icmp ne %struct.Masktype* %5, null
  br i1 %6, label %7, label %21

; <label>:7                                       ; preds = %4
  %8 = load i32* %2, align 4
  %9 = icmp eq i32 %8, 0
  br i1 %9, label %10, label %14

; <label>:10                                      ; preds = %7
  %11 = load %struct.Masktype** %current, align 8
  %12 = getelementptr inbounds %struct.Masktype* %11, i32 0, i32 1
  %13 = load i32* %12, align 4
  store i32 %13, i32* %1
  br label %22

; <label>:14                                      ; preds = %7
  %15 = load %struct.Masktype** %current, align 8
  %16 = getelementptr inbounds %struct.Masktype* %15, i32 0, i32 0
  %17 = load %struct.Masktype** %16, align 8
  store %struct.Masktype* %17, %struct.Masktype** %current, align 8
  %18 = load i32* %2, align 4
  %19 = add nsw i32 %18, -1
  store i32 %19, i32* %2, align 4
  br label %20

; <label>:20                                      ; preds = %14
  br label %4

; <label>:21                                      ; preds = %4
  store i32 -1, i32* %1
  br label %22

; <label>:22                                      ; preds = %21, %10
  %23 = load i32* %1
  ret i32 %23
}

; Function Attrs: nounwind ssp uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %a = alloca i32, align 4
  %b = alloca i32, align 4
  %c = alloca i32, align 4
  %d = alloca i32, align 4
  store i32 0, i32* %1
  %2 = call i32 @mask(i32 0)
  store i32 %2, i32* %a, align 4
  %3 = call i32 @mask(i32 5)
  store i32 %3, i32* %b, align 4
  %4 = call i32 @mask(i32 3)
  store i32 %4, i32* %c, align 4
  %5 = call i32 @mask(i32 5)
  store i32 %5, i32* %d, align 4
  %6 = load i32* %a, align 4
  %7 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @.str, i32 0, i32 0), i32 %6)
  %8 = load i32* %b, align 4
  %9 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @.str1, i32 0, i32 0), i32 %8)
  %10 = load i32* %c, align 4
  %11 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @.str2, i32 0, i32 0), i32 %10)
  %12 = load i32* %d, align 4
  %13 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @.str3, i32 0, i32 0), i32 %12)
  ret i32 0
}

declare i32 @printf(i8*, ...) #1

attributes #0 = { nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"Apple LLVM version 5.1 (clang-503.0.40) (based on LLVM 3.4svn)"}
