; ModuleID = 'examples/ntest.bc'
source_filename = "flare_module"

@"3234990066990626947" = private unnamed_addr constant [18 x i8] c"What's your name?\00", align 1

define i32 @flare_f_0() {
entry:
  %closure_indirect = call i32 @puts(ptr @"3234990066990626947")
  %closure_indirect1 = call i32 @puts(ptr @get_line)
  ret i32 %closure_indirect1
}

declare i32 @puts(ptr)

declare ptr @get_line()

define i32 @main() {
entry:
  %calltmp = call i32 @flare_f_0()
  ret i32 %calltmp
}
