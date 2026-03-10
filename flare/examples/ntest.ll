; ModuleID = 'examples/ntest.bc'
source_filename = "flare_module"

define <{ i8, [4 x i8] }> @flare_f_0(float %0) {
entry:
  %cast_slot = alloca float, align 4
  store float %0, ptr %cast_slot, align 4
  %bytes = load [4 x i8], ptr %cast_slot, align 1
  %body_store = insertvalue <{ i8, [4 x i8] }> <{ i8 0, [4 x i8] undef }>, [4 x i8] %bytes, 1
  ret <{ i8, [4 x i8] }> %body_store
}

define <{ i8, [4 x i8] }> @flare_f_1() {
entry:
  %cast_slot = alloca i8, align 1
  store i8 0, ptr %cast_slot, align 1
  %bytes = load [4 x i8], ptr %cast_slot, align 1
  %body_store = insertvalue <{ i8, [4 x i8] }> <{ i8 1, [4 x i8] undef }>, [4 x i8] %bytes, 1
  ret <{ i8, [4 x i8] }> %body_store
}

define <{ i8, [4 x i8] }> @flare_f_3({ ptr, { ptr } } %0, float %1) {
entry:
  %closure_env = extractvalue { ptr, { ptr } } %0, 1
  %closure_field = extractvalue { ptr } %closure_env, 0
  %closure_indirect = call float %closure_field(float %1)
  %closure_indirect1 = call <{ i8, [4 x i8] }> @flare_f_0(float %closure_indirect)
  ret <{ i8, [4 x i8] }> %closure_indirect1
}

define <{ i8, [4 x i8] }> @flare_f_4(i8 %0) {
entry:
  %closure_indirect = call <{ i8, [4 x i8] }> @flare_f_1()
  ret <{ i8, [4 x i8] }> %closure_indirect
}

define <{ i8, [4 x i8] }> @flare_f_2(<{ i8, [4 x i8] }> %0, ptr %1) {
entry:
  %extract_tag = extractvalue <{ i8, [4 x i8] }> %0, 0
  switch i8 %extract_tag, label %trap_block [
    i8 0, label %case_arm_0
    i8 1, label %case_arm_1
  ]

case_merge:                                       ; preds = %case_arm_1, %case_arm_0
  %case_phi = phi <{ i8, [4 x i8] }> [ %closure_indirect, %case_arm_0 ], [ %closure_indirect4, %case_arm_1 ]
  ret <{ i8, [4 x i8] }> %case_phi

case_arm_0:                                       ; preds = %entry
  %cast_slot = alloca <{ i8, [4 x i8] }>, align 8
  store <{ i8, [4 x i8] }> %0, ptr %cast_slot, align 1
  %bytes = load float, ptr %cast_slot, align 4
  %env_insert = insertvalue { ptr } undef, ptr %1, 0
  %env_insert1 = insertvalue { ptr, { ptr } } { ptr @flare_f_3, { ptr } undef }, { ptr } %env_insert, 1
  %closure_fn = extractvalue { ptr, { ptr } } %env_insert1, 0
  %closure_indirect = call <{ i8, [4 x i8] }> %closure_fn({ ptr, { ptr } } %env_insert1, float %bytes)
  br label %case_merge

case_arm_1:                                       ; preds = %entry
  %cast_slot2 = alloca <{ i8, [4 x i8] }>, align 8
  store <{ i8, [4 x i8] }> %0, ptr %cast_slot2, align 1
  %bytes3 = load i8, ptr %cast_slot2, align 1
  %closure_indirect4 = call <{ i8, [4 x i8] }> @flare_f_4(i8 %bytes3)
  br label %case_merge

trap_block:                                       ; preds = %entry
  unreachable
}

define float @flare_f_6(float %0) {
entry:
  ret float %0
}

define float @flare_f_7({ ptr, { float } } %0, i8 %1) {
entry:
  %closure_env = extractvalue { ptr, { float } } %0, 1
  %closure_field = extractvalue { float } %closure_env, 0
  ret float %closure_field
}

define float @flare_f_5(<{ i8, [4 x i8] }> %0, float %1) {
entry:
  %extract_tag = extractvalue <{ i8, [4 x i8] }> %0, 0
  switch i8 %extract_tag, label %trap_block [
    i8 0, label %case_arm_0
    i8 1, label %case_arm_1
  ]

case_merge:                                       ; preds = %case_arm_1, %case_arm_0
  %case_phi = phi float [ %closure_indirect, %case_arm_0 ], [ %closure_indirect4, %case_arm_1 ]
  ret float %case_phi

case_arm_0:                                       ; preds = %entry
  %cast_slot = alloca <{ i8, [4 x i8] }>, align 8
  store <{ i8, [4 x i8] }> %0, ptr %cast_slot, align 1
  %bytes = load float, ptr %cast_slot, align 4
  %closure_indirect = call float @flare_f_6(float %bytes)
  br label %case_merge

case_arm_1:                                       ; preds = %entry
  %cast_slot1 = alloca <{ i8, [4 x i8] }>, align 8
  store <{ i8, [4 x i8] }> %0, ptr %cast_slot1, align 1
  %bytes2 = load i8, ptr %cast_slot1, align 1
  %env_insert = insertvalue { float } undef, float %1, 0
  %env_insert3 = insertvalue { ptr, { float } } { ptr @flare_f_7, { float } undef }, { float } %env_insert, 1
  %closure_fn = extractvalue { ptr, { float } } %env_insert3, 0
  %closure_indirect4 = call float %closure_fn({ ptr, { float } } %env_insert3, i8 %bytes2)
  br label %case_merge

trap_block:                                       ; preds = %entry
  unreachable
}

define float @flare_f_9(float %0) {
entry:
  %mul = fmul float %0, 2.000000e+00
  ret float %mul
}

define float @flare_f_8() {
entry:
  %closure_indirect = call <{ i8, [4 x i8] }> @flare_f_0(float 1.000000e+01)
  %closure_indirect1 = call <{ i8, [4 x i8] }> @flare_f_2(<{ i8, [4 x i8] }> %closure_indirect, ptr @flare_f_9)
  %closure_indirect2 = call float @flare_f_5(<{ i8, [4 x i8] }> %closure_indirect1, float 7.000000e+00)
  ret float %closure_indirect2
}

define i32 @main() {
entry:
  %calltmp = call float @flare_f_8()
  %conversion = fptosi float %calltmp to i32
  ret i32 %conversion
}
