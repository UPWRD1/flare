let test of y: int -> int =
	if y > 5 then 
		b = y * 4
	else 
		b = y * 2

let main of x: int =
	b = x + test(3)
