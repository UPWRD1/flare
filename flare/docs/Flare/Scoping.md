Scoping examples

# RULE 1: Direct Sibling Access

You can reference any sibling (pub or private) in the same scope:

```ruby
A = {
    x = 1           # private
    y = pub 2       # public
    
    _ = x           # ✓ sibling access (private ok in same scope)
    _ = y           # ✓ sibling access (public ok in same scope)
}
```

# RULE 2: Parent & Ancestor Access  

You can reference any ancestor scope by name:

```ruby
A = {
    x = 1
    B = {
        _ = A       # ✓ parent scope reference
        _ = A.x     # ✓ parent scope + field projection (see Rule 4)
    }
}
```

# RULE 3: Public Propagation

`pub` only makes a field visible to the parent's siblings and up. It does NOT make private ancestors visible:

```ruby
A = {
    x = 1
    y = pub 2
}

_ = A       # ✓ A is defined in same scope
_ = A.y     # ✓ A is accessible, y is public
_ = A.x     # ✗ ERROR: x is private, not visible outside A
```

# RULE 4: Field Projection

To access X.field:

1. X must be accessible to you
2. field must be pub (unless you're inside X's immediate scope)

```ruby
A = {
    z = {
        inner = 1
        outer = pub 2
    }
    
    _ = z.outer     # ✓ z is sibling (accessible), outer is pub
    _ = z.inner     # ✗ ERROR: inner is private to z
}
```

# RULE 5: No Transitive Publishing

Publishing a reference doesn't change the visibility of that reference's children:

```ruby
Container = {
	Private = {
		secret = 1
		visible = pub 2
	}

    PublicRef = pub Private  # Publishing the reference itself
    
    Test = {
        _ = PublicRef       # ✗ ERROR: Private is not visible here
                            # PublicRef being pub doesn't 
                            # make Private accessible
    }
    
    _ = PublicRef.visible # ✓ Accessible sibling
}

_ = Container.PublicRef.visible # ✓

```

If you want this to work, you need:

```ruby

Container = {
	Private = pub { # Make the structure itself public
		secret = 1
		visible = pub 2
	}

    PublicRef = pub Private  
    
    Test = {
        _ = PublicRef        # ✓ Private is pub, so accessible
        _ = PublicRef.visible # ✓ visible is pub
        _ = PublicRef.secret  # ✗ ERROR: secret is still private
    }
}
```

# RULE 6: use declarations (if you want this feature)

`use X` brings `X`'s public members into current scope as if they were siblings:

```ruby
Container = {
	Utils = pub {
		helper1 = pub "h1"
		helper2 = pub "h2"
		private_ = "p"
	}

    Client = {
        use Utils

        _ = helper1         # ✓ brought into scope by 'use'
        _ = helper2         # ✓ brought into scope by 'use'
        _ = private_         # ✗ ERROR: not public in Utils
        _ = Utils           # ✓ Utils itself is still accessible
    }
}
```

# RULE 7: No Ancestor injection
Names defined in ancestor scopes are **not automatically available as unqualified identifiers** in descendant scopes.
They must be accessed via: 
- explicit qualification (`A.x`), or 
- `use` import.

