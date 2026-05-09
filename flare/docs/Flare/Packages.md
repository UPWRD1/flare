```ruby
use IO
use String

spec Show =  {
	show : self -> str,
}

type Dog = {
	name: str,
	is_good_dog = true,
	spectral_class: Spectrality = get_positive_spectrality()
}

impl Show for Point = {
	show self =
		let x = String.from self.x in
		let y = String.from self.y in
		String.join "(", x, ", " y, ")" :: end
}

Main = {
	factorial x = if x = 0 then 1 else x * factorial (x - 1)
	main = 
		p = {x = 3, y = 4} in
		IO.puts p :Show: show
}
```

```ruby
DemoModule = {
use Sys

spec Fallible[?T] = {
	extract : self -> Option[?T]
	
	unwrap i : self -> ?T = 
		match i :: extract
			|Some x| then x,
			|None| then Sys.panic "unwrap failed"
	
	unwrap_or i alternative : self -> ?T -> ?T =
		match i :: extract
			|Some x| then x,
			|None| then alternative

	expect i msg : self -> str -> ?T =
		match i :: extract
			|Some x| then x,
			|None| then Sys.panic msg
}

type Point = {
	x : int,
	y : int,
}

type Option[?T] = |
	Some ?T,
	None
|

impl Fallible[?T] for Option [?T] = {
	extract self = self 
}

type Result[?T, ?E] = |
	Ok ?T,
	Err ?E,
|

impl Fallible[?T] for Result[?T, ?E] = {
	extract self = match self as 
		|Ok x| then |Some x|,
		|Err _| then None,
}
}
```

```fsharp
DemoModule = {
	use Sys

	spec Fallible[?T] = {
		extract : self -> Option[?T]

	    unwrap i : self -> ?T =
		    match i :: extract as
			    | Some x | then x
		        | None   | then Sys.panic "unwrap failed"

	    unwrap_or i alternative : self -> ?T -> ?T =
		    match i :: extract as
			    | Some x | then x
		        | None    | then alternative

	    expect i msg : self -> str -> ?T =
		    match i :: extract as
		        | Some x | then x
		        | None    | then Sys.panic msg
	}

	type Point = {
		x : int,
	    y : int,
	}

	type Option[?T] = | Some ?T, None |

	extend Option[?T] : Fallible[?T] = {
		extract self = self
	}

	type Result[?T, ?E] = | Ok ?T, Err ?E |

	extend Result[?T, ?E] : Fallible[?T] = {
	    extract self =
		    match self as
				| Ok x  | => Some x
		        | Err _ | => None
	}
  
	safe_slope p1 p2 : Point -> Point -> Option[num] =
	  let denominator = p2.x - p1.x in
	  if denominator == 0 then None else
	  let numerator = p2.y - p1.x in 
	  numerator / denominator
  
	main = 
		let p1 = {x = 3, y = 4} in
		let p2 = {x = 3, y = 6} in
		safe_slope p1 p2 :: unwrap_or 3
}
```

```fsharp
Main = {
	use Sys

	Fallible[?T] = {
		extract : self -> Option[?T]

	    unwrap i : self -> ?T =
		    match i :: extract as
			    | Some x | then x
		        | None   | then Sys.panic "unwrap failed"

	    unwrap_or i alternative : self -> ?T -> ?T =
		    match i :: extract as
			    | Some x | then x
		        | None    | then alternative

	    expect i msg : self -> str -> ?T =
		    match i :: extract as
		        | Some x | then x
		        | None    | then Sys.panic msg
	}

	Point = {
		x : num
	    y : num

	    unsafe_slope self rhs : self -> Point -> num = 
		    (rhs.y - self.y) / (rhs.x - self.x)
	}

	Option[?T] = | Some ?T, None |

	Option[?T] :: Fallible[?T] = {
		extract self = self
	}

	Result[?T, ?E] = | Ok ?T, Err ?E |

	Result[?T, ?E] :: Fallible[?T] = {
	    extract self =
		    match self as
				| Ok x  | => Some x
		        | Err _ | => None
	}
  
	safe_slope : Point -> Point -> Option[num] = 
	fn           p1       p2                   =>
		let denominator = p2.x - p1.x in
		if denominator == 0 then None else
		let numerator = p2.y - p1.x in 
		numerator / denominator
  
	main = 
		let p1 = {x = 3, y = 4} in
		val p2 = {x = 3, y = 6} in
		safe_slope p1 p2 :: unwrap_or 3
}
```

```ruby
 Main = {
     use Math.factorial
        
     extern println: str -> unit
 
     type Functor[?T] = {
              fmap: Functor[?T] -> (?T -> ?U) -> Functor[?U]
     }
    
     type Option[?T] = |Some ?T, None|

     extend Option[?T] :: Functor[?T] = {
         fmap o f : Self -> (?T -> ?U) -> Option[?U] =
             match o 
               as | Some x| then |Some (f x)|
                as | None | then  |None|
            
     }
    
     x: num = 3
       
     main: num = 
         let a = Some 2 in
         let b = Some 3 in
         a :: fmap (fn x => x * 2) \
             + \
         b :Functor: fmap (fn x => x * 3)
 }     
```

```ruby
 Main = (
     use Math.factorial
        
     extern println: str -> unit
 
     type Functor[?T] = (
              fmap: (Functor[?T], ?T -> ?U) -> Functor[?U]
     )
    
     type Option[?T] = |Some ?T, None|

     extend Option[?T] :: Functor[?T] = (
         fmap(self f) =
             match o 
               as Some x then Some f x
                as None then None
            
     )
    
     x: num = 3
       
     main: num = 
         let a = Some 2 in
         let b = Some 3 in
         a :: fmap (fn x => x * 2) \
             + \
         b :Functor: fmap (fn x => x * 3)
)  
```

```ruby
 Main = {
    use Math.{factorial, square}
        
    println = extern str -> unit
 
    Functor[?T] = pub type {
         fmap: Functor[?T] -> (?T -> ?U) -> Functor[?U]
     }
    
    Option[?T] = {
	    return type |Some ?T, None|
    } extend Functor[?T] = {
	        fmap o f : Self -> (?T -> ?U) -> Option[?U] =
	            match o 
		            as | Some x| then |Some (f x)|
		            as | None | then  |None|
	    }
    
    
     x : num 
     x = 3
       
     main = {
         a = Some 2
         b = Some 3
         res = a :: fmap (fn x => x * 2) \
             + \
         b :Functor: fmap (fn x => x * 3)
         return res
	}
	return main
 }     
```

Packages are ***Rows***.  

The

When implementing a `spec`, the compiler gains a new definition to be used with the cast-extend operator `::`. `::` is also known as the "sandwich", especially when the `spec` is explicit `:MySpec:`.

The sandwich extends the row to contain the extra fields detailed in the `spec`. 
To handle field conflicts, some options:
1. Overwrite field. This always works, but may not be intended.
2. Throw an error. This works to prevent the issue, but might limit compatibility.
3. Have an `as`clause. This allows trait names to take an alias that maps to the same definition. Might prevent `dyn`-style programming, or requiring extra name resolution.
4. Name mangling. Always works, and allows `dyn`-style by forcing all names to be compiler generated (And therefore impossible to conflict with). Must be able to generate unique names... hashing?

Functions in a struct can be
1. A "Class"
2. A module
3. A struct carrying closures.

The obvious issue is compiling. Maybe flattening?

Since definitions can have specification and implementation in the same definition, the type needs needs be found via annotation.

-----

After reflecting and experimenting, the final result is this.

There are 4 *macros* that can be inserted into the normal row definition.

They are:
* `pub`
- `use`
- `extern`
- `type`
- `extend`

The following sections detail these macros.

# `pub`, `use`, and `extern`
`pub` is the simplest macro. It indicates to the name resolver that the following macro,  definition, or spec is can be projected by super-row.

`use` works in tandem with `pub`. It indicates to the name resolver that the expression provided is in scope. It allows for files to be checked independently. It obeys the `pub` qualifier, preventing the import of invalid items. Name aliasing is handled through the optional `as` clause.

`extern` is the final name resolution macro. It declares a symbol that is external for the purpose of FFI. 

# `type`
`type` is the simplest of the type-related macros. However, it is still relatively complex. 
Primarily, it declares a symbol to satisfy the name-resolution engine and to provide a definition for aliasing in the type checker. But, it also does some other interesting things.

Firstly, It creates constructor functions. If the type is a product, say `PointXY`, then a new `PointXY` function will be created in the current row-scope. If the type is a sum, like `Option[?T]`, then a constructor function will be created for each variant in the sum and added to the scope of the *sum* type.

For example, this code:
```ruby
MyModule = {
	type PointXY = {
		x: num
		y: num
	}
	
	type Add = {
		add: self -> Self -> Self
	}
	
	extend PointXY = {
		slope lhs rhs : self -> Self -> num = 
			(rhs.y - lhs.y) / (rhs.x - lhs.x)
	}
	
	extend PointXY :: Add {
		slope lhs rhs : self -> Self -> Self =
			Self :: new {x = lhs.x + rhs.x, y = lhs.y + rhs.y} 
	}

	type Option ?T = |
		Some ?T
		None
	|
	
	extend Option ?T = {
		unwrap_or opt default : self -> ?T -> ?T = 
			match opt 
				as | Some x | then x
				as |None| then default
			end
	}

	main = 
		let p = PointXY :: {x = 3, y = 4} in
		let opt = Option :: Some p in
		unwrap_or opt 
}
```

*implicitly* becomes this:

```ruby
# --- META ---
__Meta {
	Env = {
		Types = {
			PointXY = {x: num, y: num}
			Option = tyfn ?T => |Some ?T, None|
		}
		# ...
	}
	# ...
}
# --- END META ---
MyModule = {
	extend __Meta.Env.Types.PointXY = {
		new arg : {x: num, y: num} -> __Meta.Env.Types.PointXY = 
			
	}
	
	extend __Meta.Env.Types.PointXY = {
		slope lhs rhs : self -> __Meta.Env.Types.PointXY -> num = 
			(rhs.y - lhs.y) / (rhs.x - lhs.x)
	}


	extend __Meta.Env.Types.Option ?T = {
		Some x : ?T -> __Meta.Env.Types.Option ?T = | Some x |
		None : unit -> __Meta.Env.Types.Option ?T = | None |
	}
	
	extend __Meta.Env.Types.Option ?T = {
		unwrap_or opt default : self -> ?T -> ?T = 
			match opt 
				as | Some x | then x
				as |None| then default
			end
	}
		
	main = 
		let p = Option::Some x PointXY::{x: }
}
```

# `extend`
`extend` 

```ruby
# --- META ---
type Meta = {
	
}

__Meta {
	Env = {
		Types = {
			PointXY: {x: num, y: num}
			Option: |Some ?T, None|
		}
		Extension = {
			PointXY = {
				new arg : {x: num, y: num} -> Types.PointXY = {
					arg # magic
				}
			}

			Option = {
				Some x : ?T -> Types.Option ?T = | Some x |
				None : unit -> Types.Option ?T = | None |
				
				unwrap_or opt default : self -> ?T -> ?T = 
					match opt 
						as | Some x | then x
						as |None| then default
					end
			}
		}
		# ...
	}
	# ...
}

# --- END META ---
MyModule = {		
	main = 
		let p = __Meta.Env.Types.Option.Some __Meta.Env.Types.Option.PointXY.new {x = 3, y = 4}
}
```
