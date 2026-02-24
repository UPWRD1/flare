; ModuleID = 'examples/ntest.bc'
source_filename = "flare_module"

define float @flare_f_0({ float, i8 } %0) {
entry:
  %get_field = extractvalue { float, i8 } %0, 0
  ret float %get_field
}

define { float, i8 } @flare_f_1({ float, i8 } %0, ptr %1) {
entry:
  %get_field = extractvalue { float, i8 } %0, 0
  %closure_indirect = call float %1(float %get_field)
  %field_store = insertvalue { float, i8 } undef, float %closure_indirect, 0
  %get_field1 = extractvalue { float, i8 } %0, 1
  %field_store2 = insertvalue { float, i8 } %field_store, i8 %get_field1, 1
  ret { float, i8 } %field_store2
}

define float @flare_f_2(float %0) {
entry:
  ret float %0
}

define { float, i8 } @flare_f_3(float %0) {
entry:
  %closure_indirect = call float @flare_f_2(float %0)
  %field_store = insertvalue { float, i8 } undef, float %closure_indirect, 0
  %field_store1 = insertvalue { float, i8 } %field_store, i8 0, 1
  ret { float, i8 } %field_store1
}

define float @flare_f_5(float %0) {
entry:
  %mul = fmul float %0, 2.000000e+00
  ret float %mul
}

define float @flare_f_4() {
entry:
  %closure_indirect = call { float, i8 } @flare_f_3(float 1.000000e+01)
  %closure_indirect1 = call { float, i8 } @flare_f_1({ float, i8 } %closure_indirect, ptr @flare_f_5)
  %closure_indirect2 = call float @flare_f_0({ float, i8 } %closure_indirect1)
  %sub = fsub float %closure_indirect2, 2.000000e+01
  ret float %sub
}

define i32 @main() {
entry:
  %calltmp = call float @flare_f_4()
  %conversion = fptosi float %calltmp to i32
  ret i32 %conversion
}
