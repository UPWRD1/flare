```ruby
# Universe polymorphic definitions 

# CoC:
# id.{u} (A : Type u) (x : A) : A := x 
id : all A => A -> A
	= fn x => x

# def compose.{u,v,w} (A : Type u) (B : Type v) (C : Type w) (g : B → C) (f : A → B) (x : A) : C := g (f x)
compose : all A B C => (B -> C) -> (A -> B) -> A -> C 
	= fn g f x => g (f x)

compose : all A B C => {f: (B -> C), g: (A -> B), x: A} -> C 
	= fn g f x => g (f x)

```