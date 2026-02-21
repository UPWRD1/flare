; ModuleID = 'examples/ntest.bc'
source_filename = "flare_module"

define float @flare_f_0(ptr %0) {
entry:
  %struct_gep = getelementptr inbounds nuw { float, i8 }, ptr %0, i32 0, i32 0
  %load_field = load float, ptr %struct_gep, align 4
  ret float %load_field
}

define void @flare_f_1(ptr sret({ float, i8 }) %0, ptr %1, ptr %2) {
entry:
  %struct_alloca = alloca { ptr, i8 }, align 8
  %field_gep = getelementptr inbounds nuw { ptr, i8 }, ptr %struct_alloca, i32 0, i32 0
  %struct_gep = getelementptr inbounds nuw { float, i8 }, ptr %1, i32 0, i32 0
  %load_field = load float, ptr %struct_gep, align 4
  %closure_indirect = call float %2(float %load_field)
  store float %closure_indirect, ptr %field_gep, align 4
  %field_gep1 = getelementptr inbounds nuw { ptr, i8 }, ptr %struct_alloca, i32 0, i32 1
  %struct_gep2 = getelementptr inbounds nuw { float, i8 }, ptr %1, i32 0, i32 1
  %load_field3 = load i8, ptr %struct_gep2, align 1
  store i8 %load_field3, ptr %field_gep1, align 1
  store ptr %struct_alloca, ptr %0, align 8
  ret void
}

define float @flare_f_2(float %0) {
entry:
  ret float %0
}

define void @flare_f_3(ptr sret({ float, i8 }) %0, float %1) {
entry:
  %struct_alloca = alloca { ptr, i8 }, align 8
  %field_gep = getelementptr inbounds nuw { ptr, i8 }, ptr %struct_alloca, i32 0, i32 0
  %closure_indirect = call float @flare_f_2(float %1)
  store float %closure_indirect, ptr %field_gep, align 4
  %field_gep1 = getelementptr inbounds nuw { ptr, i8 }, ptr %struct_alloca, i32 0, i32 1
  store i8 0, ptr %field_gep1, align 1
  store ptr %struct_alloca, ptr %0, align 8
  ret void
}

define float @flare_f_5(ptr %0, float %1) {
entry:
  %mul = fmul float %1, 2.000000e+00
  ret float %mul
}

define float @flare_f_4() {
entry:
  %sret_slot = alloca { float, i8 }, align 8
  call void @flare_f_3(ptr sret({ float, i8 }) %sret_slot, float 1.000000e+01)
  %closure_struct = alloca { ptr, {} }, align 8
  %closure_func_gep = getelementptr inbounds nuw { ptr, {} }, ptr %closure_struct, i32 0, i32 0
  store ptr @flare_f_5, ptr %closure_func_gep, align 8
  %sret_slot1 = alloca { float, i8 }, align 8
  call void @flare_f_1(ptr sret({ float, i8 }) %sret_slot1, ptr %sret_slot, ptr %closure_struct)
  %closure_indirect = call float @flare_f_0(ptr %sret_slot1)
  %sub = fsub float %closure_indirect, 2.000000e+01
  ret float %sub
}

define i32 @main() {
entry:
  %calltmp = call float @flare_f_4()
  %conversion = fptosi float %calltmp to i32
  ret i32 %conversion
}
