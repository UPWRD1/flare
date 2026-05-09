Types are essentially a nested structure

```
Any
|- Num 
	|- Int
	|- Float
	|- Double
|- Bool
|- String
	|- char
```

But what if we take this a step further?

Expressions have types. `2+2` is `Num`

but what if it was `Four`, where `Four` is the type of expressions evaluating to `4`?

For instance, you could do something like:

```ruby
struct Dog = 
	name: str,
	sound: "Woof!",

let main = assert (Dog {name = "Cookie"}).sound == "Woof!"

```

Or more productively:

```ruby
struct FoldL[?T] = 
	_impl: fn f z s => 
		if s == [] then z else
			let {x, xs} = Seq.next s in
			_impl f (f z x) xs
```

But if the goal is to encode behavior into the type system, why not just use properties/traits instead?

```ruby
struct Map[?T, ?U] =
	xs: Seq[?T],
	f: Seq[?T] -> Seq[?U])

prop Consume =
	type Output = ?T
	def consume: Self -> Output
	
impl Consume for Map[?T, ?U] = 
	type Output = Seq[?U]
	let consume self = 
		Seq.yield self.xs fn x => self.f x
```

```ruby
# Properties/typeclasses
prop Eq = 
	def eq: self -> Self -> bool

struct Vector2 =
	x: num,
	y: num,

impl Eq for Vector2 =
	let eq l r = 
		l.x == r.x
		and
		l.y == r.y

```