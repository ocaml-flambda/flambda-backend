# The `[@error_message]` attribute

You can put the `[@error_message]` attribute on type or kind constraints
to add custom text to error messages, useful when writing a ppx.

For example, suppose we have a ppx that translates `[%i_have_a x]` to `Some x`.
This makes sense only for `value`s. So the ppx might actually translate

```
let f x = [%i_have_a x]
```

to

```
let f x = Some (x : ((_ : value)[@error_message "only works with values"]))
```

which will say `only works with values` to the user if the type of `x` does not
have kind `value`.

This also works on type constraints like `(x : string)`,
though the implementation on type constraints is more fragile, producing
the custom message only if we already know the type of the expression
before seeing the type constraint. For example, this produces a custom
message:

```
let f (x : bool) = (x : int)[@error_message "custom"]
```

But this does not:

```
let f x = (x : bool)[@error_message "custom"] + 1
```


