-- with "examples/container"

type T = struct of 
	f1: int,
	f2: flt

def T =
	let new of x, y =
		T {f1: x, f2: y}
	
	let dump of self = 
		self.f1 * 3
end

let main =
	a = T.new()
	d = call()
	
