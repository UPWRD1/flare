; ModuleID = 'examples/ntest.bc'
source_filename = "flare_module"

@"39623210056537629" = private unnamed_addr constant [14 x i8] c"Hello World\\n\00", align 1

define ptr @flare_f_0() {
entry:
  ret ptr @puts
}

declare i32 @puts(ptr)

define i32 @flare_f_1() {
entry:
  %closure_indirect = call i32 @flare_f_0(ptr @"39623210056537629")
  ret i32 %closure_indirect
}

define i32 @main() {
entry:
  %calltmp = call i32 @flare_f_1()
  ret i32 %calltmp
}
