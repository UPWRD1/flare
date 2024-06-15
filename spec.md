# The Allegro Language

## Abstract

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
do
else
end
for
if
in
let
of
return
thru
while
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

:= -- assignment
is -- type comparison
-> -- Definition block start
do -- Control block start
end -- end block
thru -- range operator
=> -- Function composition
```

---

### Declarations

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

mutable: 3! -- Mutable pair
mutable = 4 -- Reassignment
print mutable -- 4

wrong!: "asdf"
wrong = 3.0 -- Error: Differing types
```

For some people, this syntax may seem slightly strange. Why would we use `:` for declaration?

The answer lies in the way Allegro handles types. In Allegro, types are a placeholder, a *promise*. The programmer is promising that whatever value you supply the pair will be of type `x`.

When you assign a value to a pair, the "intrinsic type" of whatever scalar you use becomes the pair's type.

>Essentially, declaring the value of the pair is the same thing as declaring the pair's type.

#### Function Declarations

```lua
let f: int of (x: int, y: int) ->
    return x * y
end
```

>The syntax of function declarations is inspired by mathematical functions, where "f(x)" is read as "f of x".

Notice how the parameter and return value types are declared in the same way as a pair. This is because functions are first-class items in Allegro, which essentially makes them a pair, but with extra steps.

In Allegro, every program starts with a 'main()' function. Here's an example:

```lua
let factorial: int of (x: int) do
    return x + factorial(x - 1)
end

let main do
    print factorial(5) -- 120
end
```

Note how `main()` uses a shorthand. Functions declared this way return the silent type (`..`) and take no parameters.

Functions also support generics:

```lua
let factorial: T? of (x: T?) do
    
    ...
end
```

#### Enum, and Type Declarations

```lua
enum option [T? of
    VARIANT1,
    
end
```

### Control Flow

#### If/Else

```ruby
if condition do 
    ...
end
```

```lua
if condition do 
    ...
else do
    ...
end
```

#### While Loop

```lua
while condition do
    ...
end
```


#### For Loop

```lua
for i in iterable do
    ...
end
```

```lua
for i in 0 thru 10 do
    ...
end
```

#### Switch

```ruby
match item if
    x then ...
    y then ...
    else then ...
end
```
