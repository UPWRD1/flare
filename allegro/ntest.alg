let factorial of x: int = 	
	if x == 0 then 
		1 
	else
		x * factorial(x - 1)

let main =
	a = factorial(3)