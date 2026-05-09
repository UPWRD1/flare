```ruby
package Main = 
	type T[?G] = {
		field1: U,
		field2: ?G,
		field3: V[W]
	}
	let const_func = 1 + 2

	let func x y : ? -> ? -> ?
		where 
			? is Numeric = x + 

	let main = 
		let temp = func const_func 2
		
	let prompt(query): IO[str] -> IO[str] = 
		IO.out query &
		IO.readline
	
	let ask_for_prefs : unit -> IO[unit] =
		let fav_color = IO.query "What's your favorite color?" & 
		let fav_animal = IO.query "What are your favorite animals? List them, separated by comma" &
		let temp_construct = Iter.fold Vec.from Text.split fav_animal ","
							fn a el => Text.concat a (Text.concat el fav_color)) 
							
			
		
```

```ruby
package Logic = 
	prop Eq = 
		eq : ? -> ? -> bool

package Primatives = 
	def Eq for Num = 
		in eq lhs rhs = lhs == rhs
```

```ruby
package Vec = 
	use Core.Mem
	use Core.Mem.Pointer
	
	pub struct Vec[?T] =
		ptr: Pointer.NonNull[?T],
		cap: num,
		len: num,
		
	pub let new = 
		Vec {ptr: Pointer.dangling, cap: 0, num: 0 }
		
	pub let push(v, elem) : Vec[?T] -> ?T -> Vec[?T] = 
		let v = 
			if v.len == v.cap then 
				grow v
			else v 
		in
			do Mem.write(v.ptr.add self.len) elem 
		
		
		
```

```fsharp
package Parser = 
	Type Token = |
		Ident{String},
		Parens{Vec[Self]},
		Arrow,
		Question,
	|
	Type TypeSig = |
		Num,
		Str,
		User{String}
		Lambda(Self, Self),
		Generic(Str),
	|
	let 
	
	let parse_type_sig toks : Vec[Token] -> TypeSig  = 
		let func = 
		Iter.dynamic (toks, fn(i, token) => 
			match token =
				| Ident{s} then match s = 
					| "Num" then TypeSig.Num
					| "Str" then TypeSig.Num
		)
	
```

```ruby
package Main = 
	use IO
	
	let main = 
	#   IO.out : ?T -> IO[Unit] where ?T is Display
		IO.out("Hello world") ;
		x = IO.prompt("What's your name? ")
		& IO.out Text.concat "Hello, " x
	
	let other_main =
		IO.out "Hello world" 
		>> IO.prompt "What's your name? "
		>>= fn x => IO.out Text.concat "Hello, " x
		
		
```

```rb
package LinearAlgebra =
	struct Vector2 =
		x: num,
		y: num,

	def New for Vector 2 =
		let new x y : num -> num -> Self =
			Self {x = x, y = y}
	
	def Add for Vector2 =
		let add self rhs = 
			Self {
				x = self.x + rhs.x, 
				y = self.y + rhs.y 
			}
	
	# other defs ...

	property Numeric from Arith Eq Ord
	
	let render_number n : ?T -> str 
		where ?T is Numeric = 
			# ...
	
package Main = 
	let main = 
		let v1 = Vector2 :: new (4, 5) in
		let v2 = Vector2 :: new (6, 7) in
		let expected = Vector2 {x = 10, y = 12} in
		Testing.assert_all_eq({
			v1 + v2, 
			v1 :: add v2, 
			expected
		})
```


```rb
LinearAlgebra = {
	Vector2 = 
		type {
			x: num
			y: num
		} 
		with {
			new : num -> num -> Self = fn x y =>
				{x = x, y = y}
	
			add self rhs : self -> Self -> Self  = 
				{
					x = self.x + rhs.x, 
					y = self.y + rhs.y 
				}
		}

	Numeric = 
		type all T: Arith + Eq + Ord => T 
		with {
			render_number n : self -> str = # ...
		}

}
Main = {
	v1 = Vector2 :: new (4, 5)
	v2 = Vector2 :: new (6, 7)
	expected = Vector2 {x = 10, y = 12}
	return Testing.assert_eq \
		(v1 + v2) \
		(v1 :: add v2) \
		(expected)
		
}
```