# The Allegro Language




### Abstract:
Allegro is a programming language designed to quickly build parallel, multithreaded systems.

## Syntax

### Overview

The syntax of Allegro has been designed to meet the following goals:


* Simplicity (especially for beginners): Simple syntax lets beginners pick up the language quickly, and veterans work efficiently.
* Consistency: Consistent syntax improves DX. Nobody wants to memorize dozens of edge cases, or syntax variations.
* Readability: Readable code is usable code.
* Expressivity: Syntax should not be the limiting factor of a language.

---

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

: -- assignment
is -- type comparison
```

---

### Declarations

> Scalar: A concrete "thing", or value; ex integers, floats, strings, etc.
> 
> Pair: A key, and it's associated scalar. Typelocked, immutable by default.

#### Pair Declarations

``` lua
-- Bind 'x' to 3
x: 3 -- types are inferred
print x is int -- true

y: 3.0
print x == y -- false
-- why? floats and integers are separate types

z: str -- delayed initialization
z: "Hello world!"

illegal: str
print illegal -- Error: unbound pair
```

#### Operation Declarations

```lua
--     return type             arrow
--     v                       v
let f: int of (x: int, y: int) -> return x * y;
--  ^         ^                   ^
--  name      parameters          body 
```
