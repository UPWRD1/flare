; ModuleID = 'examples/ntest.bc'
source_filename = "flare_module"

@"11905662811443449880" = private unnamed_addr constant [14 x i8] c"Hello World\\n\00", align 1
@"13576873079904472461" = private unnamed_addr constant [8 x i8] c"Goodbye\00", align 1

define i32 @flare_f_0() {
entry:
  %closure_indirect = call i32 @puts(ptr @"11905662811443449880")
  %closure_indirect1 = call i32 @puts.1(ptr @"13576873079904472461")
  ret i32 %closure_indirect1
}

declare i32 @puts(ptr)

declare i32 @puts.1(ptr)

define i32 @main() {
entry:
  %calltmp = call i32 @flare_f_0()
  ret i32 %calltmp
}
