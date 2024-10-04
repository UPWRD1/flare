-- with "examples/container"

type A = struct of 
	d: int,
	e: flt

def A =
	let new of d, e =
		A {d: d, e: e}
end

type B = struct of 
	d: str,
	e: uint

let main =
	x = A::new()
	y = B::new()
	b = x.a
	
