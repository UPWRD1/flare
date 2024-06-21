let mul_each of x: int for Array =
    self.apply(fn of el -> el * x)

let main =
    my_array = [1, 2, 3]! -- declare mutable variable
    my_array.mul_each(2)

