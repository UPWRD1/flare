Products must have all their fields at construction. 

Products and product types are immutable.

```ruby
{
	x: T;
	y: U;
}
```

Fields can self reference if they follow the rules
```ruby
# simple reference
{
	x = 3
	y = x # y = 3
}

# functional reference
{
	x = y(3)
	y(a) = 3
}
```