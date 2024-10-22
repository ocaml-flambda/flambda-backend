# Labeled tuples

The *labeled tuples* extension allows you to label tuple elements.  It is
conceptually dual to labeled function arguments, allowing you to give a helpful
name to constructed values where labeled function arguments permit giving a
helpful name to parameters.

Here is a motivating example
where we want to compute two values from a list and be careful
not to mix them up:

```ocaml
let sum_and_product ints =
  let init = ~sum:0, ~product:1 in
  List.fold_left ints ~init ~f:(fun (~sum, ~product) elem ->
    let sum = elem + sum in
    let product = elem * product in
    ~sum, ~product)
```

This example shows the use of labeled tuples in types and patterns.  They may be
punned like record elements / function arguments.

In types, tuple labels are written similarly to function argument labels.  For
example, the function `f` in the previous example has the type:

```ocaml
(sum:int * product:int) -> int -> sum:int * product:int
```

Labeled tuples are useful anytime you want to use names to explain or
disambiguate the elements of a tuple, but declaring a new record feels too
heavy.  As another example, consider this function from `Core_unix` which
creates a pipe with descriptors for reading and writing:

```ocaml
val pipe : ?close_on_exec:bool -> unit -> File_descr.t * File_descr.t
```

Which is which?  While it's possible declaring a new record might be best in
this case, we can now use labeled tuples:

```ocaml
val pipe : ?close_on_exec:bool -> unit -> read:File_descr.t * write:File_descr.t
```

Tuples may be partially labeled, which can be useful when some elements of the
tuple share a type and need disambiguation, but others don't.  For example:
```ocaml
type min_max_avg = min:int * max:int * float
```

## Reordering and partial patterns

Like records, labeled tuple patterns may be reordered or partial.  The compiler
only supports reordering / partial matching when it knows the type of the
pattern from its context.

So, for example, we can write:
```ocaml
# let lt = ~x:0, ~y:42;;
val lt : x:int * y:int = (~x:0, ~y:42)

# let twice_y = let ~y, .. = lt in y * 2;;
val twice_y : int = 84
```

When the type is not known (in the same sense that we require a type to be known
to disambiguate among constructors), the compiler will reject a partial pattern.  For
example, this program

```ocaml
let get_y t =
  let ~y, .. = t in
  y
```

is rejected with this error:

```
File "foo.ml", line 2, characters 8-14:
2 |     let ~y, .. = t in
            ^^^^^^
Error: Could not determine the type of this partial tuple pattern.
```

This example could be fixed by adding a type annotation to the function's
parameter.

Labels may also be repeated in a tuple, and unlabeled elements can be thought of
as all sharing the same unique label.  When matching on such a tuple, the first
occurence of a label in the pattern is bound to the first corresponding label in
the value, and so on.  As a result, it's also now possible to partially match on
an unlabeled tuple to retrieve the first few elements.

## Limitations

Parentheses are necessary to disambiguate functions types with labeled arguments
from function types with labeled tuple arguments when the first element of the
tuple has a label.  `ocamlformat` will handle this for you.

Unlike records, reordering is not supported in labeled tuple expressions, even
when the type is known. This is like how the function definition for a function
with labeled arguments must bind the arguments in the same order as the type.

Labeled tuples do not support projection (extracting an element of the tuple
by label).

Structure-level let bindings do not allow reordering / partial matching as
flexibly as expression-level let bindings.  For example, this program does not
typecheck:

```ocaml
module M = struct
  let lt = ~x:0, ~y:42
  let ~y, .. = lt
end
```

It results in the error:

```
File "foo.ml", line 3, characters 6-12:
3 |   let ~y, .. = lt
          ^^^^^^
Error: Could not determine the type of this partial tuple pattern.
```