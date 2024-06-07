# The `immutable_arrays` extension

Immutable arrays are just like regular arrays, but they can't be
mutated: there's no `iarr.(0) <- new_value`, no `Iarray.blit`, etc.
The syntax is the same as existing mutable arrays, but using `:` instead
of `|`:

```ocaml
open Iarray.O

let immutable_array : string iarray =
  [: "zero"; "one"; "two"; "three" :]
;;

let zero : string =
  immutable_array.:(0)
;;
```

Because `iarray`s do not allow mutation, it is possible for the contents of an
`iarray` to be stack allocated. See the `Iarray.Local` module in the stdlib
library.

Another difference is that `iarray` is covariant: if the type `sub` is a subtype of the
type `super`, then `sub iarray` is a subtype of `super iarray`, as though you had `type
+'a iarray = ...`.  (Put another way, this means that you can cast through `iarray`s:
`(iarr : sub iarray :> super iarray)` is always valid when `(x : sub :> super)` is.)

You can also have *immutable array comprehensions*: `[: x, y for x = 1
to 3 and y in [: "some"; "thing" :] :]`.