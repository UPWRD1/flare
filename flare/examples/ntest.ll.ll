; ModuleID = 'examples/ntest.ll'
source_filename = "flare_module"

define float @flare_f_0(ptr %0) {
entry:
  %struct_gep = getelementptr inbounds nuw { float, i8 }, ptr %0, i32 0, i32 0
  %load_field = load float, ptr %struct_gep, align 4
  ret float %load_field
}

define ptr @flare_f_2(ptr %0, i8 %1) {
entry:
  %gep_closure_env = getelementptr inbounds nuw { ptr, { float } }, ptr %0, i32 0, i32 1
  %gep_closure_field = getelementptr inbounds nuw { float }, ptr %gep_closure_env, i32 0, i32 0
  %load_closure_capt = load float, ptr %gep_closure_field, align 4
  %struct_alloca = alloca { float, i8 }, align 8
  %field_gep = getelementptr inbounds nuw { float, i8 }, ptr %struct_alloca, i32 0, i32 0
  store float %load_closure_capt, ptr %field_gep, align 4
  %field_gep1 = getelementptr inbounds nuw { float, i8 }, ptr %struct_alloca, i32 0, i32 1
  store i8 %1, ptr %field_gep1, align 1
  ret ptr %struct_alloca
}

define ptr @flare_f_1(ptr %0, ptr %1) {
entry:
  %struct_gep = getelementptr inbounds nuw { float, i8 }, ptr %0, i32 0, i32 0
  %load_field = load float, ptr %struct_gep, align 4
  %call_indirect_res = call float %1(float %load_field)
  %closure_struct = alloca { ptr, { float } }, align 8
  %closure_func_gep = getelementptr inbounds nuw { ptr, { float } }, ptr %closure_struct, i32 0, i32 0
  store ptr @flare_f_2, ptr %closure_func_gep, align 8
  %closure_env_gep = getelementptr inbounds nuw { ptr, { float } }, ptr %closure_struct, i32 0, i32 1
  %closure_field_gep = getelementptr inbounds nuw { float }, ptr %closure_env_gep, i32 0, i32 0
  store float %call_indirect_res, ptr %closure_field_gep, align 4
  %fun-gep = getelementptr inbounds nuw { ptr, { float } }, ptr %closure_struct, i32 0, i32 0
  %fun_pointer = load ptr, ptr %fun-gep, align 8
  %struct_gep1 = getelementptr inbounds nuw { float, i8 }, ptr %0, i32 0, i32 1
  %load_field2 = load i8, ptr %struct_gep1, align 1
  %closure_indirect = call ptr %fun_pointer(ptr %closure_struct, i8 %load_field2)
  ret ptr %closure_indirect
}

define float @flare_f_3(float %0) {
entry:
  ret float %0
}

define ptr @flare_f_5(ptr %0, i8 %1) {
entry:
  %gep_closure_env = getelementptr inbounds nuw { ptr, { float } }, ptr %0, i32 0, i32 1
  %gep_closure_field = getelementptr inbounds nuw { float }, ptr %gep_closure_env, i32 0, i32 0
  %load_closure_capt = load float, ptr %gep_closure_field, align 4
  %struct_alloca = alloca { float, i8 }, align 8
  %field_gep = getelementptr inbounds nuw { float, i8 }, ptr %struct_alloca, i32 0, i32 0
  store float %load_closure_capt, ptr %field_gep, align 4
  %field_gep1 = getelementptr inbounds nuw { float, i8 }, ptr %struct_alloca, i32 0, i32 1
  store i8 %1, ptr %field_gep1, align 1
  ret ptr %struct_alloca
}

define ptr @flare_f_4(float %0) {
entry:
  %call_indirect_res = call float @flare_f_3(float %0)
  %closure_struct = alloca { ptr, { float } }, align 8
  %closure_func_gep = getelementptr inbounds nuw { ptr, { float } }, ptr %closure_struct, i32 0, i32 0
  store ptr @flare_f_5, ptr %closure_func_gep, align 8
  %closure_env_gep = getelementptr inbounds nuw { ptr, { float } }, ptr %closure_struct, i32 0, i32 1
  %closure_field_gep = getelementptr inbounds nuw { float }, ptr %closure_env_gep, i32 0, i32 0
  store float %call_indirect_res, ptr %closure_field_gep, align 4
  %fun-gep = getelementptr inbounds nuw { ptr, { float } }, ptr %closure_struct, i32 0, i32 0
  %fun_pointer = load ptr, ptr %fun-gep, align 8
  %closure_indirect = call ptr %fun_pointer(ptr %closure_struct, i8 0)
  ret ptr %closure_indirect
}

define float @flare_f_7(ptr %0, float %1) {
entry:
  %mul = fmul float %1, 2.000000e+00
  ret float %mul
}

define float @flare_f_6() {
entry:
  %call_indirect_res = call ptr @flare_f_4(float 1.000000e+01)
  %closure_struct = alloca { ptr, {} }, align 8
  %closure_func_gep = getelementptr inbounds nuw { ptr, {} }, ptr %closure_struct, i32 0, i32 0
  store ptr @flare_f_7, ptr %closure_func_gep, align 8
  %call_indirect_res1 = call ptr @flare_f_1(ptr %call_indirect_res, ptr %closure_struct)
  %call_indirect_res2 = call float @flare_f_0(ptr %call_indirect_res1)
  %sub = fsub float %call_indirect_res2, 2.000000e+01
  ret float %sub
}

define i32 @main() {
entry:
  %calltmp = call float @flare_f_6()
  %conversion = fptosi float %calltmp to i32
  ret i32 %conversion
}
