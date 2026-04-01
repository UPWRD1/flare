; ModuleID = 'examples/ntest.bc'
source_filename = "flare_module"

@"9818343116850033652" = private unnamed_addr constant [14 x i8] c"Hello World\\n\00", align 1

define i32 @flare_f_0() {
entry:
  %closure_indirect = call i32 @puts(ptr @"9818343116850033652")
  ret i32 %closure_indirect
}

declare i32 @puts(ptr)

define i32 @main() {
entry:
  %calltmp = call i32 @flare_f_0()
  ret i32 %calltmp
}
