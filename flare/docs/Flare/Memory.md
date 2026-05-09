The problem of memory management is simple:

> How do we prevent programmers from trying to access memory that is no longer available?

Pointers, references, whatever you want to call it, are powerful tools. But, they are dangerous when the point to *invalid* objects.

What makes a pointer *invalid?*

1. The object is uninitialized (wild pointer)
2. The object has been freed, so the pointer references uncontrolled memory (dangling pointer)

Here's how Flare controls these two issues:

# 1) Stack-Allocation

Almost all values are stack-allocated by default. This is Rust's approach, it works quickly and efficiently.

# 2) The `Resource` property

There is a property, `Resource`:

```ruby
prop Resource =
	get : ... -> Self
	release : Self -> unit
```

`Resource` defines an object with a "lifecycle". Th

```ruby
package Fs =
	struct FileHandle = # ...
	
	def Resource for FileHandle =
		let get as open name : str -> Self = # ...
		let release as close self : Self -> Unit = # ...
```

```ruby
let main =
	# BAD
	let f = Fs.open "myfile.txt" in
	let _ = Fs.close f in
	Fs.write f "uh oh"
	
	# BAD
	let f = Fs.open "myfile.txt" in
	let _ = Fs.close f in
	Fs.close f 

```


|                           | Everything (Mutability) | Everywhere (Aliasing) | All at Once (Concurrency) |
| ------------------------- | ----------------------- | --------------------- | ------------------------- |
| Everything (Mutability)   | &mut                    | Not Allowed           | Arc                       |
| Everywhere (Aliasing)     | Not Allowed             | &                     |                           |
| All at Once (Concurrency) |                         |                       | --                        |
