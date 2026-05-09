> [!quote] Code is read more than it is written - Niklaus Wirth

Flare's syntax is takes inspiration from several languages, namely F#, Rust, and Elixir.

When properly styled, it maintains a balance of simplicity, weight, readability, and intention.

Additionally, it was important to design a language that is *unambiguous*. Unambiguous languages are much easier to parse since they eliminate the pitfalls that lead to "quirks". One such "quirk" is the infamous "dangling-else" problem:

```java
public class MyProg {
	public static void main(String[] args) {
		if (some_condition == true)
			if (some_value < 67)
				do_something();
			else 
				do_something_else();
	}
}


```

Which `if` statement does the `else` belong to? Without knowing the language specification, it is hard to decide.

Flare's syntax is designed to minimize these conflicts. This leads to some interesting principles:

1. **Whitespace is insignificant.** While Python, Ruby, and Javascript can be beautiful, significant whitespace leads to headaches over invisible characters.
2. **Related ideas look similar.** To minimize overhead, syntax is consistent across similar ideas. 
3. **One right way.** There is only one way to do everything, unless the alternative is better *without question*.

An example of the these can be found in definitions. It's possible to declare a symbol and assign it a `fn` closure: 

```fsharp
{ f = fn x y => ... }
```

But it makes so much more sense to use the simplified syntax:

```fsharp
{ f x y = ... }
```

# Aesthetics

Functional languages tend to appear drastically different compared to their more imperative counterparts. To minimize "developer shock", Flare is consistent with its syntax:

1. Expressions always follow `=`
2. Types always follow `:`
3. Projections always use `.`
