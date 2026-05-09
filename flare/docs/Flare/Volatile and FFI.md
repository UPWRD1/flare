Data and types which are foreign must be annotated as `volatile`. For instance, the C function `puts()`, returns an integer status code. To use `puts()` in flare requires the following import declaration: 

```ruby
extern puts: str -> !int
```