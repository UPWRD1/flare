How do we permit stateful operations in a purely expression based language?

# What is a list?

The typical functional programmer would implement a naïve linked list like so:

```ruby
enum List[?T] = 
	Cons{?T, Box[Self]},
	End

let main = 
	# {1, 2, 3}
	let mylist = Cons{1, Cons{2, Cons{3, End}}} in #...
```

This structure is one of the first things people learn when introduced to functional programming for the first time. And for good reason! It's intuitive, simple, and importantly, *predictable*. 

What if we stored function pointers in a list?

```ruby
use {IO, Iter}

let main = 
	let execution_sequence = 
		Cons {
			myfunc, 
			Cons {
				IO.out "Hello World", 
				Cons {
					IO.out 2 + 2
				}
			}
		}
	in 
	Iter.foreach execution_sequence fn fp => fp()
```

Ideally, this would iterate through each item in the list, perform the computation, then end. But, this is really verbose and unclear. What if it was easier?

```ruby
use {IO, Iter, }

let main = 
	do {
		IO.out "Hello World",
		set x = IO.prompt "First number: ",
		IO.out format {"2 + 2 = ", value}
	}

```

`do` is a function that takes a tuple `{e1, e2, e3}` of expressions return the Seq[?T] type, and returns the value of the last one. Essentially, it does what our original list-based engine did.

How do we access temporaries?

`do` is essentially a hybrid `foreach` and `fold`?

```ruby
	Iter.foreach fn item => 
		if item.
```
