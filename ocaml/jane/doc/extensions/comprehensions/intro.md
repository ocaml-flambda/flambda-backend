# The `comprehensions` extension

This page introduces comprehensions and gives a high-level, user-oriented
overview.
For more technical details, see [the detailed documentation](details.md).

# List and array comprehensions

List and array comprehensions, as also seen in languages such as Python or
Haskell, are a syntactic form for cleanly building lists and arrays (hereinafter
referred to collectively as sequences), based on mathematical set-builder
notation. Here are some examples:

```ocaml
# open Core;;
# (* Pythagorean triples with components from 1 to 10, no duplicate triples *)
  [ a, b, c for a = 1 to 10 for b = a to 10 for c = b to 10 when a * a + b * b = c * c ];;
- : (int * int * int) list = [(3, 4, 5); (6, 8, 10)]

# (* Let's describe some objects *)
  [| sprintf "a %s %s" adjective noun
     for noun in [| "light"; "pepper" |]
     and adjective in [| "red"; "yellow"; "green" |]
  |];;
- : string array =
[|"a red light"; "a yellow light"; "a green light"; "a red pepper";
  "a yellow pepper"; "a green pepper"|]

# (* Compute a list of reciprocals in increasing order *)
  [ 1. /. Float.of_int x for x = 5 downto 0 when x <> 0 ];;
- : float list = [0.2; 0.25; 0.333333333333333315; 0.5; 1.]

# (* Flatten a nested array *)
  let sentences =
    [| [| "hello"; "world" |]
    ;  [| "how"; "are"; "you"; "doing" |]
    ;  [| "please"; "enjoy"; "these"; "comprehensions" |]
    |]
  in
  [| word for sentence in sentences for word in sentence |];;
- : string array =
[|"hello"; "world"; "how"; "are"; "you"; "doing"; "please"; "enjoy"; "these";
  "comprehensions"|]

# (* We could use comprehensions to reimplement map... *)
  let map' ~f l = [ f x for x in l ];;
val map' : f:('a -> 'b) -> 'a list -> 'b list = <fun>

# (* ...and filter *)
  let filter' ~f l = [| x for x in l when f x |];;
val filter' : f:('a -> bool) -> 'a array -> 'a array = <fun>
```

## Syntax and semantics

The general form of a (list) comprehension is

```ocaml
[ BODY
    for PAT in SEQ and ... and VAR = LOW to HIGH and ... and VAR = HIGH downto LOW and ...
    ...
    when COND
    ...
    for ...
    ...
    when ...
    ... ]
```

(Array comprehensions differ only in being surrounded by `[| ... |]` instead of
`[ ... ]`.)  Breaking this down:

* The body is an expression that computes the values of the resulting sequence.
  Examples above include `a,b,c` and `sprintf "a %s %s" adjective noun`.

* The various things that can come after a `for` or an `and` are called
  *iterators*, and they generate values and bind them to patterns.  Any
  variables bound by these patterns are in scope to the right of the whole `for
  ... and ...` clause, as well as in the body of the comprehension.  Examples
  above include (a) `adjective in [| "red"; "yellow"; "green" |]`, (b) `a = 1 to
  10`, and (c) `x = 5 downto 0`.
    + The `PAT in SEQ` form (example (a)) iterates over a sequence, and matches
      each value in that sequence against the specified pattern.  (Partial
      patterns will currently throw an exception if the match fails; e.g.,
      `Some _ in [ None ]` will throw an exception. This may
      change in future versions.)  List comprehensions iterate over lists and
      array comprehensions iterate over arrays; the two cannot be mixed.

    + The `VAR = LOW to HIGH` and `VAR = HIGH downto LOW` forms (examples (b)
      and (c), respectively) iterate over inclusive ranges of `int`s, just as
      they do in `for` loops; the `to` form counts up from `LOW` to `HIGH`, and
      the `downto` form counts down from `HIGH` to `LOW`.  If `LOW > HIGH`,
      these iterators are empty and will never produce any values.

* The `when` clauses specify conditions on the values being iterated over; the
  body, as well as any clauses to the right of a `when`, are only evaluated on
  iterations when the condition, a `bool`, is `true`.  Examples above include
  `when a * a + b * b = c * c` and `when x <> 0`.

The result of the comprehension is a sequence consisting of the value of the
body expression evaluated with the iterator patterns bound to every possible
combination of values from the iterators (the Cartesian product) such that all
the conditions hold.  The order of the result is given by evaluating the clauses
(`for ... and ...` and `when` alike) in order from left to right; they may be
thought of as nested loops/conditionals.

## A special-case optimization for simple (fixed-size) array comprehensions

One somewhat unusual design choice is the decision to allow multiple iterators
in a single clause, via `for ITERATOR_1 and ITERATOR_2 and ... and ITERATOR_N`,
rather than just having the user write `for ITERATOR_1 for ITERATOR_2 ... for
ITERATOR_N`.  The semantics of the two are almost the same, but not quite.  The
key difference is that `and` does parallel assignment, while nested `for`s do
sequential assignment. This difference has two consequences.  The first
consequence is about variable scoping: nested `for`s allow you to use an earlier
variable in a later iterator, while `and` does not. This is just like the
difference between `let BINDING_1 in let BINDING_2 in BODY` (two `for`s)
vs. `let BINDING_1 and BINDING_2 in BODY` (`for`-`and`). The second consequence
is about evaluation order and frequency: the right-hand side of every iterator
in a single `for`-`and` clause is evaluated all at once before iteration begins,
but nested `for` clauses cause the inner clause's iterator to be repeatedly
re-evaluated on every iteration of the outer clause.

The parallel semantics of `and` offers us an optimization opportunity for array
comprehensions: if an array comprehension contains exactly one clause, and it’s
a `for ... and ...` clause, then we can allocate an array of exactly the right
size up front (instead of having to grow the generated array dynamically, as we
usually do).  We call this the *fixed-size array comprehension optimization*.
We cannot do this with nested `for`s, as the sizes of iterators further to the
right could depend on the values generated by those on the left.  Thus, the
second example above (“Let’s describe some objects”) would preallocate an array
of 6 strings and loop through it, whereas the fourth example (“Flatten a nested
array”) would have to start with a smaller array and grow it as necessary.
(Of course, this optimization does not apply to lists. The only thing we can do with a
list is iterate over it, and the only way we can produce a list is by building it up piece
by piece.)

## Evaluation order

The clauses (`for ... and ...` and `when`) of a comprehension are guaranteed to
be evaluated from left to right, and all the sources of values for iterators in
a single `for ... and ...` clause are guaranteed to be evaluated exactly once
per surrounding iteration before any of them begin to iterate.

## More details

This should be enough to use comprehensions on a day-to-day basis; if you have
more questions, or are simply curious about how things work on the inside, see
the [details page](details.md).