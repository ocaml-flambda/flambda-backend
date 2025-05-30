# The `let mutable` extension

The `let mutable` extension provides a new type of `let` statement which
declares a stack-local variable. It can be thought of as an unboxed `ref`.

```ocaml
let triangle n =
  let mutable total = 0 in
  for i = 1 to n do
    total <- total + i
  done;
  total
```

Mutable `let` declarations may not be recursive, and they may not be used at the
structure level or in class definitions. The pattern of a mutable `let`
statement must be a single variable, possibly with a type annotation, e.g. `let
mutable x, y = ..` is not allowed. Mutable `let` statements must also not use
`and`s.

Mutable variables must also not escape their scope. For example, you can't
return a closure that closes over a mutable variable. At the moment, the mode
checker is, sadly, not sophisticated enough to allow some constructions which
are obviously safe. For example, the following code is safe, but rejected by the
mode checker.

```ocaml
let sum xs =
  let mutable total = 0 in
  List.iter xs ~f:(fun x -> total <- total + x);
  total
```
