Flare has several unique idioms.

# Match expressions
Flare has no `if-then-else` expression because match expressions are able to check booleans.

```ruby
do_check = 
	match some_cond
		as true then do_this()
		as false then do_that()
	end
```

