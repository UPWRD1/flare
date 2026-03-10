; ModuleID = 'test.c'
source_filename = "test.c"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx15.0.0"

%struct.Wrapper = type { double, i32 }
%struct.Closure = type { ptr, ptr }

@.str = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define [2 x i64] @new_Wrapper(double noundef %0) #0 {
  %2 = alloca %struct.Wrapper, align 8
  %3 = alloca double, align 8
  store double %0, ptr %3, align 8
  %4 = getelementptr inbounds nuw %struct.Wrapper, ptr %2, i32 0, i32 1
  store i32 0, ptr %4, align 8
  %5 = load double, ptr %3, align 8
  %6 = getelementptr inbounds nuw %struct.Wrapper, ptr %2, i32 0, i32 0
  store double %5, ptr %6, align 8
  %7 = load [2 x i64], ptr %2, align 8
  ret [2 x i64] %7
}

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define [2 x i64] @map_wrapper([2 x ptr] %0, [2 x i64] %1) #0 {
  %3 = alloca %struct.Wrapper, align 8
  %4 = alloca %struct.Closure, align 8
  %5 = alloca %struct.Wrapper, align 8
  %6 = alloca ptr, align 8
  store [2 x ptr] %0, ptr %4, align 8
  store [2 x i64] %1, ptr %5, align 8
  %7 = getelementptr inbounds nuw %struct.Closure, ptr %4, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr inbounds nuw %struct.Wrapper, ptr %5, i32 0, i32 0
  %10 = call ptr %8(ptr noundef %9)
  store ptr %10, ptr %6, align 8
  %11 = load ptr, ptr %6, align 8
  %12 = load double, ptr %11, align 8
  %13 = getelementptr inbounds nuw %struct.Wrapper, ptr %5, i32 0, i32 0
  store double %12, ptr %13, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %5, i64 16, i1 false)
  %14 = load [2 x i64], ptr %3, align 8
  ret [2 x i64] %14
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias writeonly captures(none), ptr noalias readonly captures(none), i64, i1 immarg) #1

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define double @extract([2 x i64] %0) #0 {
  %2 = alloca %struct.Wrapper, align 8
  store [2 x i64] %0, ptr %2, align 8
  %3 = getelementptr inbounds nuw %struct.Wrapper, ptr %2, i32 0, i32 0
  %4 = load double, ptr %3, align 8
  ret double %4
}

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define double @mul_2(double noundef %0) #0 {
  %2 = alloca double, align 8
  store double %0, ptr %2, align 8
  %3 = load double, ptr %2, align 8
  %4 = fmul double %3, 2.000000e+00
  ret double %4
}

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.Closure, align 8
  %3 = alloca %struct.Wrapper, align 8
  %4 = alloca %struct.Wrapper, align 8
  %5 = alloca double, align 8
  store i32 0, ptr %1, align 4
  %6 = getelementptr inbounds nuw %struct.Closure, ptr %2, i32 0, i32 1
  store ptr @mul_2, ptr %6, align 8
  %7 = call [2 x i64] @new_Wrapper(double noundef 2.000000e+01)
  store [2 x i64] %7, ptr %3, align 8
  %8 = load [2 x ptr], ptr %2, align 8
  %9 = load [2 x i64], ptr %3, align 8
  %10 = call [2 x i64] @map_wrapper([2 x ptr] %8, [2 x i64] %9)
  store [2 x i64] %10, ptr %4, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %4, i64 16, i1 false)
  %11 = load [2 x i64], ptr %3, align 8
  %12 = call double @extract([2 x i64] %11)
  %13 = fsub double %12, 2.000000e+01
  store double %13, ptr %5, align 8
  %14 = load double, ptr %5, align 8
  %15 = call i32 (ptr, ...) @printf(ptr noundef @.str, double noundef %14)
  %16 = load double, ptr %5, align 8
  %17 = fptosi double %16 to i32
  ret i32 %17
}

declare i32 @printf(ptr noundef, ...) #2

attributes #0 = { noinline nounwind optnone ssp uwtable(sync) "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #1 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #2 = { "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 15, i32 4]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 8, !"PIC Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 1}
!4 = !{i32 7, !"frame-pointer", i32 1}
!5 = !{!"Homebrew clang version 21.1.8"}
