Some ideas for the standard library. Some of these items use special compiler intrinsics that let them do unusual or otherwise impossible things.

# Properties

# `Add`

```ruby
property Add =
	add: Self -> Self -> Self 
```

Property for types that can use the arithmetic add operator ( `+` ).

**See Also:** [[#Arith]], [[#Numeric]]

# `Arith`

```ruby
property Arith where Self is Add Subtract Multiply Divide
```

AT LEAST a marker property for types that have definitions for the four arithmetic operators.

**See Also:** [[#Add]], [[#Divide]], [[#Multiply]], [[#Numeric]], [[#Subtract]]

# `Divide`

```ruby
property Divide =
	div: Self -> Self -> Self 
```

Property for types that can use the arithmetic division operator ( `/` ).

**See Also:** [[#Arith]], [[#Numeric]]

# `Eq`

```rb
property Eq =
	eq: Self -> Self -> bool
	let neq self rhs : Self -> Self -> bool = not self :: eq rhs
```

Property for values that can be equal to each other.

**See Also:** [[#Numeric]]

# `Multiply`

```ruby
property Multiply =
	mul: Self -> Self -> Self 
```

Property for types that can use the arithmetic multiplication operator ( `*` ).

**See Also:** [[#Arith]], [[#Numeric]]

# `New`

```ruby
property New =
	new: ?T -> Self where ?T is UnlimitedUniqueGenericVariadic
```

Used as a constructor function. Implemented by users. 

`UnlimitedGenericVariadic` is a compiler intrinsic representing a function type of unknown length and unknown types: `?A -> ?B -> ?C -> ... -> ?Z`. 

```ruby
struct Vector2 =
	x: num,
	y: num,

def New for Vector2 =
	let new x y : num -> num -> Self =
		Self {x = x, y = y}

let main = 
	let v = Vector2 :: new (6, 7) in
		v.x
```

# `Numeric`

```ruby
property Numeric where Self is Arith Eq Ordered
```

Marker property for types that act like numbers.

**See Also:** [[#Arith]], [[#Eq]], [[#Ord]]

# `Ord`

```ruby
@intrinsic
property Ordered where Self is Eq
	less_than: Self -> Self -> bool
	
	greater_than: Self -> Self -> bool
	
	let less_than_eq self rhs : Self -> Self -> bool = 
		self :: eq rhs or self :: less_than rhs

	let greater_than_eq self rhs : Self -> Self -> bool = 
		self :: eq rhs or self :: greater_than rhs
```

Property for types that are Ordered, enabling the usage of the logical comparison operators (`<`, `<=` `>`, `>=`).

# `Subtract`

```ruby
property Subtract =
	sub: Self -> Self -> Self 
```

Property for types that can use the arithmetic subtraction operator ( `-` ).

**See Also:** [[#Arith]], [[#Numeric]]

# Types

# `Seq[?T]`

```rb
package Seq = 
	struct Seq[?T] =
		_phantom: PhantomData[?T],
		len: UnsizedNum,
		ptr: NonNull[?T],

	let from tuple : UnsizedTuple -> Self =
	 # ... 

```