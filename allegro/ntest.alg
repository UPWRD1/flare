let mul_each of x: ?T for !Array =
    self.apply(fn of el: ?T -> el * x)

let main =
    my_array = [1, 2, 3] -- declare mutable variable
    my_array.mul_each(2) -- [2, 4, 6]



