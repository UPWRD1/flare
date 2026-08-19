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

I'm now debating the necessity of both `fn` style and implicit syntax.

```ruby
plus1_v1 = {f x = x + 1}.f
plus1_v2 = fn x => x + 1

usage_v1 = plus1_v1 1 # 2
usage_v2 = plus1_v2 1 # also 2 ...
```

The elaborator essentially already translates

```ruby
plus1_v1 = {f x = x + 1}.f
# into
plus1_v1_elab = {f = fn x => x + 1}.f
# simplified
plus1_v1_simp = fn x => x + 1
```

It might be cool to only have 1 syntax. If we are going to allow arbitrary patterns on the left side of field assignments, then "function" style patterns might be useful.

```ruby
f x y = x + y
```

The downside of getting rid of `fn` style functions, is that there are no true "anonymous" functions. 

```ruby
map s f : Seq A -> (A -> B) -> Seq B = # ...

usage = {
	list = Seq.generators.fibonacci :: take 5 # 1 2 3 5 8
	doubled = map list {double n = n * 2} # 2 4 6 10 16
}
```

You could just use an underscore if you don't want a name.

```ruby
tripled = map list { _ n = n * 2} # 2 4 6 10 16
```

This is interesting, because we now have a "trait", `Func`, which is what all functions are?

All functions are just a singleton row with 1 function (which becomes a lambda)?

```ruby
type Func A B = {
	_ ..
}
```

```ruby
Main (io: Logger) -> Unit =
  fix go => fn t xs =>
    xs as (
      Nil = report caps numShow t,
      Cons c =
        go {{Tree Str Num numOrd}.insert c.head.0 c.head.1 t} c.tail
    )
  + go (Tree Str Num numOrd).empty (list [ ("b", 2), ("a", 1), ("c", 3) ])
```

Regarding patterns in label position, it might be interesting to limit them to argument position, a la:
```ruby
Unit = {}
Mono x = {}
Duo x y = {}
Specific 1 = {}
```

Then, we could have something like
```ruby
operate io n : IO -> num -> {} = 
	x == 3 as {
		True = io.print "is 3"
		False = io.print make.string x
	}
```

```ruby
# A trait: anything Show-able can render to Str
Show a = {
	type { show: a -> Str }
}
```

```ruby
# A generic binary search tree, parameterized over key type k and value type v.
# Demonstrates: self-referential type field, a trait impl living inside the
# same fix as the recursive type it describes, and the fix-splitting problem
# discussed earlier (type-level recursion vs value-level recursion sharing
# one surface `fix`).
Tree k v keyOrd =  {
  type = |Leaf, Node { key: k, val: v, left: self, right: self } |,

  empty: self = |Leaf|,

  insert key val t : k -> v -> self -> self =
    t as {
      Leaf = |Node { key, val, left = empty, right = empty }|
      Node n =
        keyOrd.cmp key n.key as {
          LT = |Node { left  = insert key val n.left, ..n}|
          GT = |Node { right = insert key val n.right, ..n}|
          EQ = |Node { val   = val, ..n }|
        }
    },

  # Recursive trait impl: Show for Tree requires Show for v, and recurses
  # into itself via `self.Show.show` -- the open-recursion case flagged
  # earlier. This field NEEDS the explicit type annotation below, or
  # typeof can't terminate without evaluating the body.
  Show showV : Show v -> Show self = {
    show t : self -> Str =
      t as {
        Leaf = "."
        Node n =
          "(" + 
	          show n.left + 
	          " " + 
	          showV.show n.val + 
	          " " + 
	          self.Show.show n.right + 
          ")"
      }
  }
}

# A capability row combining two effects: logging and a key-value backend.
# Note these are unrelated traits living side by side, composed by
# ordinary row concatenation at the call site, not inheritance.
Logger = { log: Str -> Unit }
KV     = fn k => fn v => { get: k -> v -> v, put: k -> v -> Unit }

# A program requiring BOTH capabilities, plus a Show instance for its value
# type, passed explicitly as a third kind of "dictionary" argument.
report caps showNum t : { io: Logger } -> Show Num -> Tree Str Num -> Unit 
    caps.io.log (t.Show.show t)

# numOrd: an Ord instance for Num, needed by Tree's insert
numOrd: { cmp: Num -> Num -> Ordering } = {
  cmp = fn a b =>
    match (lt a b) {
      True  => LT,
      False => match (eq a b) {
        True  => EQ,
        False => GT
      }
    }
}

numShow: Show Num = { show = numToStr }

Main caps : { io: Logger } -> Unit =
	caps.io.print "hello";
  fix go => fn t => fn xs =>
    xs as {
      Nil = report caps numShow t,
      Cons = fn c =>
        go ((Tree Str Num numOrd).insert c.head.0 c.head.1 t) c.tail
    }
  + go (Tree Str Num numOrd).empty (list [ ("b", 2), ("a", 1), ("c", 3) ])
```


Here we go with function syntax again....

Do we want perfect symmetry, or ease of use?
```ruby
# A program requiring capabilities.

# current
MyModule = {
	report caps t : { io: Logger } -> Tree Str Num -> Unit = 
		caps.io.log(t.Show.show t)
}

# sugar type parens to function
MyModule = {
	report caps  t : ({ io: Logger }, Tree Str Num) -> Unit =
		caps.io.log (t.Show.show t)

# struct args
MyModule = {
	report _ : {caps: { io: Logger}, t: Tree Str Num} -> Unit = 
		_.caps.io.log (_.t.Show.show _.t)
}

# prev but binding type pattern
MyModule = {
	report {caps: { io: Logger}, t: Tree Str Num} -> Unit = 
		caps.io.log (t.Show.show t)
}

# prev but annotation style
MyModule = {
	report {caps, t} : {{io: Logger}, Tree Str Num} -> Unit = 
		caps.io.log (t.Show.show t)
}

# prev but parens and braces swap for construction/application order
MyModule = (
	report (caps: (io: Logger), t: Tree Str Num) -> Unit = 
		caps.io.log {t.Show.show t}
)
```

```ruby
Container = {
	type = {  
		capacity: num,  
		contents: num
	}
  
	new cap : num -> Self =  
		{capacity: cap, contents: 0}  
  
	get_remaining: (self) -> int = self.capacity - self.contents  
  
	in add: (self, wt: int) -> Container =  
		if self.contents + wt < self.capacity then  
			Container {capacity: self.capacity, contents: self.contents + wt}  
		else  
			Container {capacity: self.capacity * 2, contents: self.contents + wt}
}
```