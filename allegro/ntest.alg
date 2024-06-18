let factorial: int of x = x + factorial(x)

-- function that extends the inbuilt Array type.
let mul_each of x: int for Array! = 
    self.apply(fn of el -> el = el * x)

let main =
    my_array = [1, 2, 3]! -- declare mutable variable
    my_array.mul_each(2) -- [2, 4, 6]