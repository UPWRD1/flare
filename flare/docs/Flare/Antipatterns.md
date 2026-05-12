There are some developer choices, that aren't wrong from the compiler's perspective, but go against the natural "flow" of the language. This tries to highlight some of these.

---

# __Code Antipatterns__

# Variable Overuse

## Problem

Variables are useful for storing intermediate values and improving readability. However, using too many can impede the natural "flow" of the code.

## Example

```ruby
double_every_word s = {
	split = String.split s
	doubled = split.flat_map fn x => {
		repeated = (Stream.repeat x)
		return repeated.take(2) 
		}
	return String.from doubled
}

```

## Refactoring

```ruby
double_every_word s = {
	split = String.split s
	return String.from split.flat_map fn x => 
			(Stream.repeat x).take 2 
}
```

By reducing unnecessary variables, we increase readability and highlight the *behavior* of the system over the *actions*.

---

# Disorganized Types

## Problem

A disorganized datatype structures data in strange ways.

## Example

```ruby
DB T = type {
	users: Map int {name: str, prefs: (UserPrefs T)}
	is_logged_in: Map int bool
	# ...
```

## Refactoring

```ruby
UserID = type num

UserData T = type {
	name: str
	prefs: UserPrefs T
	is_logged_in: bool
}

DB T = { 
	users: Map UserID (Userdata T)
}
```

We give the magic `int` a meaningful name: `UserId`. Then, we move `is_loggedin` from a separate map to the user data itself.

# __Convention Antipatterns__

# Bad Param Placement

## Problem

By convention, the *subject* of the operation goes before any parameters that control the operation. Breaking this convention makes function interfaces confusing, and may cause UFCS to work in unintended ways.

## Example

```ruby
Ops = {
	add_each val l : num -> Seq num -> Seq num =
		Stream.map l fn x => x + val
}

Main = {
	my_list = Vec.from 1..3
	return Ops.add_each 3 my_list # 4 5 6
}
```

## Refactoring

```ruby
Ops = {
	add_each val l : num -> Seq num -> Seq num =
		Stream.map l fn x => x + val
}

Main = {
	my_list = Vec.from 1..3
	return Ops.add_each my_list 3 # 4 5 6
}
```


By reducing unnecessary variables, we increase readability and highlight the *behavior* of the system over the *actions*.

# Strange Names

## Problem

Use names that make sense, abide by conventions, and enhance readability.

Modules and types are `UpperCamelCase`, identifiers are `lower_snake_case`

## Example

```ruby
frobnicate b = {
	mYsuperCool_thing = Lib_01_a.MysteryFunctionASDF {"hello", "world"}
	return SystemIOHandler.out mYsuperCool_thing.Do_it b
}
```

## Refactoring

```ruby
print_times times = {
	repeater = Stream.Repeater {"hello", "world"}
	IO.out repeater.take times
}
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

