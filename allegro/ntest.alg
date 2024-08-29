type File = struct of
	name: str

let open of s: str -> str = 
	return "Lorem ipsum"

let read of f: str -> str =
	return f

let apply of e: T, f: Fn -> T =
	f(e)

let main = 	
	f = open("myfile.txt")
			& read() 
			& apply(fn of el: T -> (el & to_ascii_int()) + 1 ) 
