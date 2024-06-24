define i32 @main(i32 %my_array) {
entry:
  %my_array1 = alloca i32
  store i32 %my_array, i32* %my_array1
  store i32 3, i32* %my_array1
  ret i32 3
}
