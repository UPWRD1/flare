There are some developer choices, that aren't wrong from the compiler's perspective, but go against the natural "flow" of the language. This tries to highlight some of these.

---

# __Code Antipatterns__

# Variable Overuse

## Problem

Variables are useful for storing intermediate values and improving readability. However, using too many can impede the natural "flow" of the code.

## Example

```ruby
let double_every_word s = 
	let split = String.split s in
	let doubled = split.flat_map fn x => 
		let repeated = (Factory.repeater x) in 
			repeated.take(2) 
	String.from doubled

```

## Refactoring

```ruby
let double_every_word s = 
	let split = String.split s in
	String.from split.flat_map fn x => 
			(Factory.repeater x).take 2 
```

By reducing unnecessary variables, we increase readability and highlight the *behavior* of the system over the *actions*.

# If-if-if-if

## Problem

`if` expressions are a simple way to express control flow. However, chained `if` expressions may be better-expressed by a `match` expression.

## Example

```ruby
let inspect n = 
	if n < 0 then
		IO.out "Less than 0"
	else 
		if n == 0 then
			IO.out "Equals zero"
		else 
			if 5 > n >= 1 then 
				IO.out "Small number"
			else 
				if n > 5 
					IO.out "Big Number"
					do_something_not_otherwise()
				else
					IO.out "Biggest Number"

```

## Refactoring

```ruby
let inspect n = 
	IO.out match n
		| ..0  then "Less than 0"
		| 0    then "Equals zero"
		| 1..5 then "Small number"
		| 5=.. then "Big Number"
		else "Weird Number"
```

Not only is the example code overly verbose, it has an error! If `n` equals `5`, it will be marked as a "Weird number". The refactor fixes this, ensures behavioral consistency, and reduces repetition.

---

# Disorganized Types

## Problem

A disorganized datatype structures data in strange ways.

## Example

```ruby
struct DB[?T] =
	users: Map[int, {str, UserPrefs[?T]],
	is_loggedin: Map[int, bool],
	# ...
```

## Refactoring

```ruby
type UserID = int

struct UserData[?T] = 
	username: str,
	prefs: UserPrefs[?T],
	is_loggedin: bool

struct DB[?T] = 
	users: Map[UserID, Userdata[?T]]
```

We give the magic `int` a meaningful name: `UserId`. Then, we move `is_loggedin` from a separate map to the user data itself.

# __Convention Antipatterns__

# Bad Param Placement

## Problem

By convention, the *subject* of the operation goes before any parameters that control the operation. Breaking this convention makes function interfaces confusing, and may cause UFCS to work in unintended ways.

## Example

```ruby
package Ops = 
	let my_operation num l : int -> Vec[int] -> Vec[int] =
		Iter.map l fn x => x + num

package Main = 
	let main =
		let my_list = Vec.from {1, 2, 3} in
		my_operation = Ops.add_ 3 my_list
```

## Refactoring

```ruby
package Ops = 
	let add_each l num : Vec[int] -> int -> Vec[int] =
		Iter.map l fn x => x + num

package Main = 
	let main =
		let my_list = Vec.from {1, 2, 3} in
		my_operation = Ops.add_each my_list 3 # {4, 5, 6}
```

By reducing unnecessary variables, we increase readability and highlight the *behavior* of the system over the *actions*.

# Strange Names

## Problem

Use names that make sense, abide by conventions, and enhance readability.

Modules and types are `UpperCamelCase`, identifiers are `lower_snake_case`

## Example

```ruby
let frobnicate b =
	let mYsuperCool_thing = Lib_01_a.MysteryFunctionASDF {"hello", "world"} in
	SystemIOHandler.out mYsuperCool_thing.Do_it b
```

## Refactoring

```ruby
let print_times times =
	let repeater = Iter.Repeat {"hello", "world"} in
	IO.out repeater.repeat times
```

Don't use the incorrect casing convention, and use good, self-explanatory names.

# Visibility Abuse

## Problem

The ability to control the public visibility of items in packages using `pub` allows developers to develop secure interfaces to libraries and efficiently encapsulate code. However, overusing `pub` creates a potential danger, by exposing code that could be vulnerable to attack.

## Example

```ruby
package Library = 
	pub struct Data[?T] =
		pub a: ?T
		# ...
	
	pub let do_something data = 
		local_code data
		# ...
		
	pub let local_code data =
		# ...
```

## Refactoring

```ruby
package Library = 
	pub struct Data[?T] =
		a: ?T
		# ...
	
	pub let do_something data = 
		local_code data
		# ...
		
	let local_code data =
		# ...
```

By limiting access to `local_code`, and `Data[?T]`'s "`a`" field, we can prevent unauthorized access, and (potentially) mitigate security vulnerabilities.

# Spacing Snags

## Problem

Incorrectly formatted code is hard to read and maintain. It can also introduce ambiguity in the meaning of a statement, which can be hard to debug!

## Example

```ruby
let main = let x = something() in if x.condition() then let temp = part2 fn x => x.frobnicate() in temp else part3 x 
```

## Refactoring

```ruby
let main = 
	let x = something() in 
	if x.condition() then 
		let temp = part2 fn x => x.frobnicate() in temp 
	else 
		part3 x 
```

Correctly formatting code allows it to be easily read.

# Package Overreach

## Problem

Functions defined in a library package should not take arguments with types from another package.

## Example

```ruby
package MyLib = 
	use OtherLib
	
	let frobnicate : SuperCoolThingFromOtherLib[?T] -> int = 
		# ...
```

## Refactoring

```ruby
let main = 
	let x = something() in 
	if x.condition() then 
		let temp = part2 fn x => x.frobnicate() in temp 
	else 
		part3 x 
```

Correctly formatting code allows it to be easily read.

