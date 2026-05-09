In addition to the expression, scope, and type positions, there are potential "Force" positions

OR explicit Forcing

```ruby
Option T = {
	# other defs...
	is_some o : Option T -> bool = #...
	
	return |Some T, None|
}

Main = {
	# Implicit forcing
	maybe_not x : all T => T -> Option T = Option.Some x
	
	# Explicit forcing
	maybe_not x : all T => T -> ~Option T = Option.Some x	
}

```