let mul_each of x: int for Array =
    self.apply(fn of el: int -> el * x)
    self.apply(fn of el: int -> el * x)


let main =
    my_array = [1, 2, 3]
    new_array = my_array.mul_each(2)
    IO.out(new_array) 
