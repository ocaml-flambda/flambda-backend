# Comprehension details

This file covers how comprehensions work in more detail than is
necessary on a day-to-day level; for a higher-level view, see the [introduction to comprehensions](intro.md).

## Syntax

The BNF for comprehensions, in a form suitable for being added to the [grammar of
OCaml](https://v2.ocaml.org/manual/expr.html), is

```
expr +::=
  | `[` comprehension `]`
  | `[|` comprehension `|]`

comprehension ::=
  expr { comprehension_clause }+

comprehension_clause ::=
  | `for` comprehension_iterator { `and` comprehension_iterator }*
  | `when` expr

comprehension_iterator ::=
  | pattern `in` expr
  | value-name `=` expr ( `to` | `downto` ) expr
```

## Evaluation Order

Evaluating a comprehension happens in the following order:

1. Clauses are evaluated from left to right.  Clauses further to the right will
   be evaluated once for each of the values from the surrounding iterators.
    * To evaluate a `for ... and ...` clause, first the sources of values for
      each iterator are evaluated, and then the iterators are iterated over
      from left to right.
        1. Before performing any iteration, each iterator’s source of values is
           evaluated exactly once, in order from left to right.
            * For a `PAT in SEQ` iterator, the expression `SEQ` is evaluated.
            * For a `VAR = START to/downto STOP` iterator, the expression
              `START` is evaluated before the expression `STOP`.
        2. Then, the iterators are iterated over, from left to right; the
           iterators further to the right “vary faster”.  Each time a value is
           drawn from an iterator, the next iterator will be iterated over in
           its entirety before the “outer” (more leftwards) iterator moves onto
           the next value.
    * To evaluate a `when COND` clause, the expression `COND` is evaluated; if
      it evaluates to `false`, the current iteration is terminated and the
      innermost surrounding iterator advances to its next value.  No clauses
      further to the right are evaluated, and nor is the body.
2. At each iteration step, once of all the clauses have been evaluated (and all
   the `when` clauses have evaluated to `true`), the body is evaluated, and the
   result is the next element of the resulting sequence.

## Desugaring

List and array comprehensions are compiled completely differently; the former
are compiled in terms of some internal pre-provided functions, and the latter
are compiled as a series of nested loops.

### Compiling list comprehensions

List comprehensions are compiled in terms of reversed difference lists.  A
difference list in general is a function from lists to lists; by "reversed",
we mean that these lists are stored backwards, and need to be reversed at
the end.  We make both these choices for the usual efficiency reasons:
difference lists allow for efficient concatenation; they can also be viewed
as based on passing around accumulators, which allows us to make our
functions tail-recursive, at the cost of building our lists up backwards.
An additional choice we make is to build all these intermediate data
structures on the stack (i.e., make them `local_`); again, this is for
efficiency, as it means we don't need to get the structure of these
difference lists involved with the garbage collector. Since we can
currently only generate global lists with list comprehensions, we need a
type that is spine-local but element-global; we thus define a custom type of
such snoc[^fn:snoc] lists and define our difference lists in terms of that (in the
internal module `CamlinternalComprehension`):
```ocaml
  type 'a rev_list =
    | Nil
    | Snoc of { init : 'a rev_list; global_ last : 'a }

  type 'a rev_dlist = local_ 'a rev_list -> local_ 'a rev_list
```
We then work exclusively in terms of `local_ 'a rev_dlist` values, reversing
them into a global `list` only at the very end.

[^fn:snoc]: I.e., the reverse of cons (`::`).

We desugar each iterator of a list comprehension into the application of a
tail-recursive higher-order function analogous to `concat_map`, whose type
is of the following form:
```ocaml
  ...iterator arguments... ->
  local_ ('elt -> local_ 'res rev_dlist) ->
  local_ 'res rev_dlist
```
Here, the `...iterator arguments...` define the sequence of values to be
iterated over (the `seq` of a `for pat in seq` iterator, or the `start` and
`end` of a `for x = start to/downto end` iterator); the function argument is
then to be called once for each item.  What goes in the function?  It will be
the next iterator, desugared in the same way.  At any time, a `when` clause
might intervene, which is simply desugared into a conditional that gates
entering the next phase of the translation.

Eventually, we reach the body, which is placed into the body of the innermost
translated function; it produces the single-item reversed difference list
(alternatively, snocs its generated value onto the accumulator).  Because each
function is analogous to `concat_map`, this builds up the correct list in the
end.  The whole thing is then passed into a reversal function, building the
final list.

For example, consider the following list comprehension:
```ocaml
[x+y for x = 1 to 3 when x <> 2 for y in [10*x; 100*x]]
(* = [11; 101; 33; 303] *)
```
This translates to the (Lambda equivalent of the) following:
```ocaml
(* Convert the result to a normal list *)
CamlinternalComprehension.rev_list_to_list (
  (* for x = 1 to 3 *)
  let start = 1 in
  let stop  = 3 in
  CamlinternalComprehension.rev_dlist_concat_iterate_up
    start stop
    (fun x acc_x -> local_
      (* when x <> 2 *)
      if x <> 2
      then
        (* for y in [10*x; 100*x] *)
        let iter_list = [10*x; 100*x] in
        CamlinternalComprehension.rev_dlist_concat_map
          iter_list
          (fun y acc_y -> local_
            (* The body: x+y *)
            Snoc { init = acc_y; last = x*y })
          acc_x
      else
        acc_x)
    Nil)
```

### Compiling array comprehensions

Array comprehensions are compiled completely differently from list
comprehensions: they turn into a nested series of loops that mutably update an
array.  This is simple to say, but slightly tricky to do.  One complexity is
that we want to apply an optimization to certain array comprehensions: if an
array comprehension contains exactly one clause, and it’s a `for ... and ...`
clause, then we can allocate an array of exactly the right size up front
(instead of having to grow the generated array dynamically, as we usually do).
We call this the *fixed-size array comprehension optimization*.  We cannot do
this with nested `for`s, as the sizes of iterators further to the right could
depend on the values generated by those on the left; indeed, this is one of the
reasons we have `for ... and ...` instead of just allowing the user to nest
`for`s.

In the general case, the structure is: we allocate an array and a mutable index
counter that starts at `0`; each iterator becomes a loop; `when` clauses become
an `if` expression, same as with lists; and in the body, every time we generate
an array element, we set it and increment the index counter by one.  If we’re
not in the fixed-size array case, then we also need the array to be growable.
This is the first source of extra complexity: we keep track of the array size,
and if we would ever exceed it, we double the size of the array.  This means
that at the end, we have to use a subarray operation to cut it down to the right
size.

The second source of extra complexity is the fixed-size array case.  In this
case, we have to first compute the size of every iterator and multiply them
together; for both of these operations, we have to check for overflow, in which
case we simply fail.  We also check to see if any of the iterators would be
empty (have size `0`), in which case we can shortcut this whole process and
simply return an empty array.  Once we do that, though, the loop body is simpler
as there’s no need to double the array size, and we don’t need to cut the list
down to size at the end.

To see some examples of what this translation looks like, consider the following
array comprehension, the same as the list comprehension we had before:
```ocaml
[| x+y for x = 1 to 3 when x <> 2 for y in [| 10*x; 100*x |] |]
(* = [| 11; 101; 33; 303 |] *)
```
This translates to (the Lambda equivalent of) the following:
```ocaml
(* Allocate the (resizable) array *)
let array_size = ref 8 in
let array      = ref [|0; 0; 0; 0; 0; 0; 0; 0|] in
(* Next element to be generated *)
let index = ref 0 in
(* for x = 1 to 3 *)
let start = 1 in
let stop  = 3 in
for x = start to stop do
  (* when x <> 2 *)
  if x <> 2 then
    (* for y in [|10*x; 100*x|] *)
    let iter_arr = [|10*x; 100*x|] in
    for iter_ix = 0 to Array.length iter_arr - 1 do
      let y = iter_arr.(iter_ix) in
      (* Resize the array if necessary *)
      if not (!index < !array_size) then begin
        array_size := 2 * !array_size;
        array := Array.append !array !array
      end;
      (* The body: x + y *)
      !array.(!index) <- x + y;
      index := !index + 1
    done
done;
(* Cut the array back down to size *)
Array.sub !array 0 !index
```
On the other hand, consider this array comprehension, which is subject to the
fixed-size array comprehension optimization:
```ocaml
[|x*y for x = 1 to 3 and y = 10 downto 8|]
(* = [|10; 9; 8; 20; 18; 16; 30; 27; 24|] *)
```
This translates to (the Lambda equivalent of) the following rather different OCaml:
```ocaml
(* ... = 1 to 3 *)
let start_x = 1  in
let stop_x  = 3  in
(* ... = 10 downto 8 *)
let start_y = 10 in
let stop_y  = 8  in
(* Check if any iterators are empty *)
if start_x > stop_x || start_y < stop_y
then
  (* If so, return the empty array *)
  [||]
else
  (* Precompute the array size *)
  let array_size =
    (* Compute the size of the range [1 to 3], failing on overflow (the case
       where the range is correctly size 0 is handled by the emptiness check) *)
    let x_size =
      let range_size = (stop_x - start_x) + 1 in
      if range_size > 0
      then range_size
      else raise (Invalid_argument "integer overflow when precomputing \
                                    the size of an array comprehension")
    in
    (* Compute the size of the range [10 downto 8], failing on overflow (the
       case where the range is correctly size 0 is handled by the emptiness
       check) *)
    let y_size =
      let range_size = (start_y - stop_y) + 1 in
      if range_size > 0
      then range_size
      else raise (Invalid_argument "integer overflow when precomputing \
                                    the size of an array comprehension")
    in
    (* Multiplication that checks for overflow ([y_size] can't be [0] because we
       checked that above *)
    let product = x_size * y_size in
    if product / y_size = x_size
    then product
    else raise (Invalid_argument "integer overflow when precomputing \
                                  the size of an array comprehension")
  in
  (* Allocate the (nonresizable) array *)
  let array = Array.make array_size 0 in
  (* Next element to be generated *)
  let index = ref 0 in
  (* for x = 1 to 3 *)
  for x = start_x to stop_x do
    (* for y = 10 downto 8 *)
    for y = start_y downto stop_y do
      (* The body: x*y *)
      array.(!index) <- x*y;
      index := !index + 1
    done
  done;
  array
```
You can see that the loop body is tighter, but there’s more up-front size
checking work to be done.