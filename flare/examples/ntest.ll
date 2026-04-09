; ModuleID = 'examples/ntest.bc'
source_filename = "flare_module"

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define noundef float @flare_f_0() local_unnamed_addr #0 {
entry:
  ret float 3.000000e+00
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define noundef i32 @main() local_unnamed_addr #0 {
entry:
  ret i32 3
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) }
