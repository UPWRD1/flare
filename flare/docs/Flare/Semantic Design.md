# Names

Almost everything in Flare has a name. Names form a tree-like structure of definitions. To navigate this tree, projection paths are used to qualify the names. A projection is an expression resolving to the object it points to, or, to the `return` member of that object.

Examples of paths:

```ruby
Package.Sub.Type.Variant

Package.Sub.Function

Package.Property
```

Paths are only valid if:

1. The object exists
2. The object is Available

# Availability Rules

By default, all definitions are private, making them available only to their local context

`pub` objects are available throughout the module tree, provided their ancestors are all also `pub`.

**Availability Table**

The nature of "Current" and "Ancestor" differs for different objects:

| Object            | Current                                    | Ancestor                                    |
| ----------------- | ------------------------------------------ | ------------------------------------------- |
| Package           | n / a                                      | The superpackage of the package             |
| Func              | The function body                          | Parent Package                              |
| Type              | The type's definition scope                | The package defining the type               |
| Type field        | The type's definition scope                | The package defining the type               |
| Property          | The declaration/definition of the property | The package declaring/defining the property |
| Property Function | The declaration/definition of the property | The package declaring/defining the property |

| Object             | Current | Ancestor | Other | Examples                                                                             |
| ------------------ | ------- | -------- | ----- | ------------------------------------------------------------------------------------ |
| (private) object   | ✅       | ✅        | ❌️    | Hidden Types                                                                         |
| (private) children | ✅       | ❌️<br>   | ❌     | "helper" functions in properties                                                     |
| `pub` children     | ❗       | ❗️       | ❗️    | Invalid (Children cannot exceed parent availability)                                 |
|                    |         |          |       |                                                                                      |
| `pub` object       | ✅       | ✅        | ✅     | User-facing library items                                                            |
| (private) children | ✅       | ❌        | ❌     | Types/properties in libraries with children accessible by the library but not users. |
| `pub` children     | ✅       | ✅        | ✅     | public items in public packages                                                      |

# Expressions

In a typical Flare package, most entities are expressions. Expressions contain the logic and implementation of functions. Expressions produce a value when evaluated. There are many types of expressions.

Literals are "raw" values.

```ruby
-123.456 # number literals
"Hello world" # string literals
true # boolean literals
{} # Unit constructors
{1, 2, 3} # Tuple Constructors
{a: "123", b: 098.765, c: Vec.from {}} # Anonymous record constructors
Person {name: "Philly J.", age: 67 } # Struct constructors

instance.field.0 # field access

fn x y => x + y # Lambdas
```

`let`...`=`...`in` expressions bind expressions to symbols. Scope is enforced by ensuring each declaration recursively contains the next. 

```ruby
let f = 
	let temp = 3 in
	temp * 2
```

Expressions have a type, which is automatically inferred. For instance: 

-  `123` is of type `Num`
- `fn x y => ...` is of type `? -> ? -> ?`

# Functions

Functions are the primary unit of code in Flare. Functions take an *argument*, and return a value. Multi-argument functions are made possible via implicit currying, which enables partial application. 

Lambdas, or anonymous functions, behave like their named counterparts. Internally, all functions are parsed as anonymous, and simply bound to names. This makes `let`-defined functions semantically equal to their `fn`-defined counterparts.

```ruby
let f x y z : ? -> ? -> ? -> ? = # ...

let g x : ? -> ? = fn y => fn z => # ... 

let main = 
	assert! f 1 2 3 == g 1 2 3
```

Functions are called by applying them to their arguments like so:

```ruby
let result : unit -> IO[unit] = 
	IO.out f 4 5 f 6 7 8
	#      f(4(5(f(6(7(8))))))
```

Functions are first-class values. They can be used like any other value, be passed to functions, returned from functions, etc.

```ruby
let f x y = x + y

let main = 
	let the_vec = Vec.from {1, 2, 3, 4, 5} in
	let the_output = Iter.fold the_vec f # using f as the fold function
```

A special keyword, `extern`, is used in the standard library to define unsafe external functions.

```ruby
package IO =
	# ... other definitions
	extern "puts" c_puts : str -> int

	let out value : ?T -> IO[unit]
		where ?T is Show = 
			match c_puts String.from value =
				| 0 then IO.new {}
				else e then IO.exit(e)
```

# Types 

Types are the other fundamental half of functional programming. They are the data that our transformations are applied to.

There are several primitive types:

```ruby
# literals
num  # numbers
str  # string literals
char # character literals
bool # true / false
unit # unit type

?T -> ?T # Function type
{?T, ?T, ?T, ...} # Tuple

```

Since they are omnipresent, Flare makes it easy to define user-created types. 

There are three user-defined types: Structs, Enumerations, and Aliases

Structs contain fields, which can be of differing types:

```ruby
package Vec = 
	struct Vec[?T] =
		field1: ?T
		field2: num,
		# ...
```

Enums contain variants, which take one of three forms:

```ruby
package _ =
	
	enum Forms[?T] = 
		Atomic,
		TupleLike{num, num, ?T},
		StructLike{name: str, age: num},
		# ...
```

Aliases allow users to create simpler names for complex types.

```ruby
alias MyType[?T, ?U, ?V] = Something[{
								Really[?T], 
								Complex[
									{?U, ?V}
								]
							}]
```

Flare is not an object-oriented language, so it doesn't support methods on types. 

You can still pass user types to module functions, though.

```ruby


```

Generic types allow for variations of types, such as containers.

```ruby
enum List[?T] =
	Cons{?T},
	Nil
```

Function types, if no signature is provided, are implicitly generic.

```ruby
let add x y = x + y
#   add : ? -> ? -> ?
```
