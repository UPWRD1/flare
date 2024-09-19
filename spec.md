# The Allegro Language

## Introduction

> ...Because if it’s boring
> in a different way, that’ll be interesting too.
> That’s what I say.
> -- John Ashbery, The Short Answer

Allegro is a programming language designed to quickly build parallel, multithreaded systems.

## Syntax

### Overview

The syntax of Allegro has been designed to meet the following goals:

* Simplicity (especially for beginners): Simple syntax lets beginners pick up the language quickly, and veterans work efficiently.
* Consistency: Consistent syntax improves DX. Nobody wants to memorize dozens of edge cases, or syntax variations.
* Readability: Readable code is usable code.
* Expressivity: Syntax should not be the limiting factor of a language.

---

### List of Keywords

```rust
def
else
end
for
if
let
mut
of
prop
return
thru
```

### Operators and Comments

``` lua
-- comment

; -- statement terminator

-- Arithmetic operators
+ -- addition
- -- subtraction / numeric negation
* -- multiplication
/ -- division

-- Logical operators
not -- logical not
and -- logical and
or -- logical or

-- Comparison operators
< -- less than
<= -- less than or equal to
> -- greater than
>= -- greater than or equal to
== -- equal to
!= -- not equal to

-- Other operators:

= -- assignment
is -- type comparison
thru -- range operator
& -- Function composition
``` 

---

### Declarations

#### Pair Declarations

``` lua
-- Bind 'x' to 3
x = 3 -- types are inferred
print x is int -- true

y = 3.0
print x == y -- false
-- why? floats and integers are separate types

mut mutable = 3 -- Mutable pair
mutable = 4 -- Reassignment
print mutable -- 4

mut wrong = "asdf"
wrong = 3.0 -- Error: Differing types
```

 In Allegro, types are a placeholder, a *promise*. The programmer is promising that whatever value you supply the pair will be of type `x`.

When you assign a value to a pair, the "intrinsic type" of whatever scalar you use becomes the pair's type.

>Essentially, declaring the value of the pair is the same thing as declaring the pair's type.

#### Function Declarations

```lua
let f of x: int, y: int -> int = 
    return x * y
```

>The syntax of function declarations is inspired by mathematical functions, where "f(x)" is read as "f of x".

Notice how the parameter and return value types are declared similarly to a pair. This is because functions are first-class items in Allegro, which essentially makes them a pair, but with extra steps.

In Allegro, every program starts with a 'main()' function. Here's an example:

```lua
let factorial of x = 
    return x + factorial(x - 1)


let main =
    print factorial(5) -- 120
```

Note how `main()` uses a shorthand. Functions declared this way take no parameters.

## Primative Types

* `int`
* `uint`
* `flt`
* `word`
* `byte`

* `bool`
  
* `chr`
* `str`

* `ptr`

* `tuple`
* `array`

