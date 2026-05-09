# General Behavior

All types are First Class.

# Primitives

The following types are considered primitive:

```ruby
num
bool
str # Immutable string literals
char
unit # {}
```

# Functions

Function types are defined using the arrow operator.

```ruby
?T -> ?T
```

# Memory

Heap allocation is indicated with the `ref ` keyword

```ruby
package Box = 
	struct Box[?T] =
		inner: ref ?T

	let new t = 
		Box {inner = Core.Mem.allocate t }
```

# Structs

Structs are a user-defined type. They contain fields
