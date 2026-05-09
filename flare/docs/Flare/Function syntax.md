Experiments:


# Functions take tables

Braces
```ruby
let f {x:int  y: int} -> int =
	x + y
	
let main = 
	let res = f {x = 3, y = 4} in
		{res, res * 2}.0
```

Parens
```ruby
let f (x:int  y: int) -> int =
	x + y

let fib : int -> 

let main = 
	let res = f(x = 3, y = 4) in
		(res, res * 2).0
```