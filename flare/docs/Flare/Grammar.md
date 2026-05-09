Here's a quick rundown of the grammar syntax:

1. `"terminals"` are enclosed in quotes.
2. `<symbols>` are written within angle brackets.
3. Productions use `::=`
4. Adjacency is concatenation
5. `|` represents alternation (choice)
6. `*` represents 0 or more
7. `+` represents 1 or more
8. Brackets `{` and `}` can be added to repetition to represent a list of items delimitated by the token inside.
9. `?` represents optional syntax
10. Parens `(` and `)` represent grouping

```ebnf
<program>       ::= <package>+
<package>       ::= "package" <ID> "=" (<definition>+ | <package>+)
<definition>    ::= <let_def> 
				| <struct_def> 
				| <enum_def>
				| <use>
				| <extern>

<extern>        ::= "extern" <str_lit> <ID> ":" <type>

<use>           ::= "use" <use_body>
<use_body>      ::= <path> /* ex: use Iter */
				|  "{" (<path>)+{","} "}" /* ex: use {Iter, Collections.Vec} */

<struct_def>    ::= "struct" <ID> "=" (<struct_member>)+{","}
<struct_member> ::= <ID> ":" <type>

<enum_def>      ::= "enum" <ID> "=" (<enum_member>)+{","}
<enum_member>   ::= <enum_member_simple> 
				| <enum_member_tuple_like> 
				| <enum_member_struct_like>
<enum_member_simple> 
				::= <ID>
<enum_member_tuple_like>
				::= <ID> "{" (<type>)+{","} "}"
<enum_member_struct_like>
				::= <ID> "{" (<struct_member>)+{","} "}"


<let_def>       ::= "pub"? "let" <ID> <ID>* (":" <type>)? "=" <expr>

<expr>          ::= <bin_expr>
<bin_expr>      ::= <product>

/* Pratt parsing should be used here, this is written in recursive-descent style
*/
/*product: left(9) */
<product>       ::= <sum>
				| <sum> "*" <sum> 
				| <sum> "/" <sum>

/* sum: left(8) */
<sum>           ::= <apply>
				| <apply> "+" <apply>
				| <apply> "-" <apply>

/* apply: left(10) */
<apply>         ::= <term> <term>

<term>          ::= <atom> 
				| "fn" <ID>* "=" <expr>

<atom>          ::= <num> 
				| <str_lit>
				| <constructor>
				| "true" 
				| "false" 
				| <path>
				| "let" <ID> "=" <expr> "in" <expr>
				| "if" <expr> "then" <expr> "else" <expr>
				| "match" <expr> <match_arm>+

<match_arm>     ::= "|" <pattern> "then" <expr>
				| "else" <ID> "then" <expr>


<constructor>   ::= "{" <expr>*{","} "}"
				| <path> (
					  "{" <expr>*{","} "}" 
					| "{" (<ID> "=" <expr>){","} "}" 
					)
				| <path>


<pattern_cons>  ::= "{" <pattern>*{","} "}"
				| <path> "{" <pattern>*{","} "}"

<pattern>       ::= <pat_constructor> 
				| <atom> /* variable / constant */


<ID>            ::= (
					[a-z] | 
					[A-Z] | 
					"_" | 
					/* valid unicode codepoint */ 
					)+

<path>          ::= <ID> "." <path> | <ID> 
<num>           ::= "-"? [0-9]+ ("." [0-9]+)?
<str_lit>       ::= "\"" (
	/*any well-formed unicode codepoint */
				)* "\""

<ty_list>       ::= <type> "," <ty_list> | <type> 
<type>          ::= <path> ("[" <type> "]")?
				| <type> "->" <type> /* function */
				| "{" <ty_list> "}" /* tuple */
				| "?" <ID> /* generics */
```



```ebnf
<program>       ::= <expression>+ /* A program is a list of expressions */
<package>       ::= "package" <ID> "=" (<definition>+ | <package>+)
<definition>    ::= <let_def> 
				| <struct_def> 
				| <enum_def>
				| <use>
				| <extern>

<extern>        ::= "extern" <str_lit>? <ID> ":" <type>

<use>           ::= "use" <expr>

<let_def>       ::= "pub"? "let" <ID> <ID>* (":" <type>)? "=" <expr>

/* Pratt parsing should be used here, this is written in recursive-descent style
*/
<expr>          ::= <bin_expr>
<bin_expr>      ::= <sandwich>

<sandwich>      ::= <sandwich>
				| <access> ":" <ID> ":" <access>
				| <access> "::" <access>

<access>        ::= <product>
				| <product> "." <product>

/*product: left(9) */
<product>       ::= <sum>
				| <sum> "*" <sum> 
				| <sum> "/" <sum>

/* sum: left(8) */
<sum>           ::= <apply>
				| <apply> "+" <apply>
				| <apply> "-" <apply>

/* apply: left(10) */
<apply>         ::= <term> <term>

<term>          ::= <atom> 
				| "fn" <ID>* "=>" <expr> /*Lambda expression*/

<atom>          ::= <num> 
				| <str_lit>
				| "true" 
				| "false" 
				| <constructor>
				| <path>
				| "let" <ID> "=" <expr> "in" <expr>
				| "if" <expr> "then" <expr> "else" <expr>
				| "match" <expr> "as" <match_arm>+

<match_arm>     ::= <pattern> "then" <expr>

<pattern>       ::= <constructor> 
				| <atom> /* variable / constant */

<constructor>   ::= "{" <expr>*{","} "}" /* tuple */
				| "{" (<ID> "=" <expr> ("," | "\n"))+ "}" /* table */
				| "|" <ID> <expr> "|" /* table */

<ID>            ::= (
					[a-z] | 
					[A-Z] | 
					"_" | 
					/* valid unicode codepoint */ 
					)+

<num>           ::= "-"? [0-9]+ ("." [0-9]+)?
<str_lit>       ::= "\"" (
	/*any well-formed unicode codepoint */
				)* "\""

<ty_list>       ::= <type> ("," | "\n") (<ty_list> | <type>)
<type>          ::= <ID> ("[" <ty_list> "]")?
				| <type> "->" <type> /* function */
				| "{" <ty_list> "}" /* tuple */
				| "{" (<ID> "=" <type> ("," | "\n"))+ "}" /* table */
				| "?" <ID> /* generics */
				| <expr>
```

```ebnf
/* A program is a list of expressions */
<program>       ::= <expr>+

<extern>        ::= "extern" <str_lit>? <ID> ":" <type>

<use>           ::= "use" <expr>

/* Pratt parsing should be used here, this is written in recursive-descent style*/
<expr>          ::= <bin_expr>
<bin_expr>      ::= <sandwich>

<sandwich>      ::= <sandwich>
				| <access> ":" <ID> ":" <access>
				| <access> "::" <access>

<access>        ::= <product>
				| <product> "." <product>

/*product: left(9) */
<product>       ::= <sum>
				| <sum> "*" <sum> 
				| <sum> "/" <sum>

/* sum: left(8) */
<sum>           ::= <apply>
				| <apply> "+" <apply>
				| <apply> "-" <apply>

/* apply: left(10) */
<apply>         ::= <term> <term>

<term>          ::= <atom> 
				| "fn" <ID>* "=>" <expr> 
/*Lambda expression*/

<atom>          ::= <num> 
				| <str_lit>
				| "true" 
				| "false" 
				| <constructor>
				| "let" <ID> "=" <expr> "in" <expr>
				| "if" <expr> "then" <expr> "else" <expr>
				| "match" <expr> "as" <match_arm>+

<match_arm>     ::= <pattern> "then" <expr>

<pattern>       ::= <constructor> 
				| <atom> 
                /* variable / constant */

<constructor>   ::= "{" (<expr> ",")* <expr>  "}" 
				| "{" (<ID> "=" <expr> ("," | "\n"))+ "}" 
				| "|" <ID> <expr> "|" 
         

<ID>            ::= (
					[a-z] | 
					[A-Z] | 
					"_" 
					 
					)+

<num>           ::= "-"? [0-9]+ ("." [0-9]+)?
<str_lit>       ::= "\"" ([a-z] | [A-Z] | [0-9]
				)* "\""

<ty_list>       ::= <type> ("," | "\n") (<ty_list> | <type>)
<type>          ::= <ID> ("[" <ty_list> "]")?
				| <type> "->" <type> 
				| "{" <ty_list> "}" 
				| "{" (<ID> "=" <type> ("," | "\n"))+ "}" 
				| "?" <ID> 
				| <expr>
```