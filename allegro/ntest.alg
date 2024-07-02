-- let mul_each of x: ?T for !Array{?T} =
--     self.apply(fn of el: ?T -> el * x)

let main =
    my_array = [1, 2, 3]
    IO.out(my_array.mul_each(2))
