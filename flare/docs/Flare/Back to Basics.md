We're starting over. Full semantic overhaul/redefinition.

Flare is a programming language built on a compositional product row type system. 

```ruby
# A definition containing the empty *row, the unit type. The unit type is a Primitive, it has no subtypes.
a = ()

# Another definition. "b" has the VALUE of "1". 1 is a Primitive, it has no subtypes.
b = 1

# Definitions can be repeated provided they do not conflict. The most specific definition is used at resolution.
b = 1

# Yet another definition. "c" has the VALUE of "Int". "Int" is a typeclass. It's subtypes are all the integers (the ℤ set).
c = Int

# A function. "sum" adds two "Num"s. 
# Functions are (currently) limited in Flare: 
# - They must operate on a single argument
# - That argument must be a subtype of the infinite *row "(..)". Essentially, any *row. 
sum (a = Num, b = Num) = a + b

sum = (a = Num, b = Num): a + b

# The identity function. The "x" parameter is generic for all ?T.
id (x = ?T) = x

# A more complicated definition.
Pair = (
	T = (
		x = Num
		y = Num
	)
	slope (lhs = self.T, rhs = Self) =  (rhs.y - lhs.y) / (rhs.x - lhs.x)
)

main = Int
main (io = IO, ..) = (
	p1 = Pair
	p1 = (x = 10, y = 2)
	p2 = (x = 5, y = 1)
	return = Pair.slope(p1, p2)
).return
 
```

```ruby
# Structures as functions

sum = {
	1 = {
		1 = 2
		2 = 3
		# ...
	}
	2 = {
		1 = 3
		2 = 4
		# ...
	}
}

four = sum(2)(2) # 4


```


Type Hierarchy

1. `?` The generic type. Universal quantifier.
2. `(..)` Anonymous struct type. 
3. `(_ = ?)`
4. `()` Unit type.

