(* TEST
 expect;
*)

[%quote 42];;
[%%expect {|
- : int code = [%quote 42]
|}];;

[%quote 3.14s];;
[%%expect {|
- : float32 code = [%quote 3.14s]
|}];;

[%quote 3.14];;
[%%expect {|
- : float code = [%quote 3.14]
|}];;

[%quote "foo"];;
[%%expect {|
- : string code = [%quote "foo"]
|}];;

[%quote {foo|bar|foo}];;
[%%expect {|
- : string code = [%quote {foo|bar|foo}]
|}];;

[%quote true];;
[%%expect {|
- : bool code = [%quote true]
|}];;

[%quote false];;
[%%expect {|
- : bool code = [%quote false]
|}];;

[%quote ()];;
[%%expect {|
- : unit code = [%quote ()]
|}];;

[%quote (1, 2)];;
[%%expect {|
- : (int * int) code = [%quote (1, 2)]
|}];;

[%quote (1, 2, 3)];;
[%%expect {|
- : (int * int * int) code = [%quote (1, 2, 3)]
|}];;

[%quote (~lab:"val", ~lab2:77, 30)];;
[%%expect {|
- : (lab:string * lab2:int * int) code = [%quote (~lab:"val", ~lab2:77, 30)]
|}];;

[%quote []];;
[%%expect {|
- : 'a list code = [%quote []]
|}];;

[%quote [1; 2; 3]];;
[%%expect {|
- : int list code = [%quote (::) (1, ((::) (2, ((::) (3, [])))))]
|}];;

[%quote [||]];;
[%%expect {|
- : '_weak1 array code = [%quote [||]]
|}];;

[%quote [| 1; 2; 3 |]];;
[%%expect {|
- : int array code = [%quote [|1; 2; 3|]]
|}];;

[%quote None];;
[%%expect {|
- : 'a option code = [%quote None]
|}];;

[%quote Some 111];;
[%%expect {|
- : int option code = [%quote Some 111]
|}];;

[%quote `A 42];;
[%%expect {|
- : [> `A of int ] code = [%quote `A 42]
|}];;

[%quote if true then `A 10 else `B ("foo", 42)];;
[%%expect {|
- : [> `A of int | `B of string * int ] code = [%quote
if true then  `A 10  else  `B ("foo", 42)]
|}];;

[%quote function | `A x -> x | `B (_, foo) -> foo];;
[%%expect {|
- : (([< `A of '_weak3 | `B of '_weak4 * '_weak3 ] as '_weak2) -> '_weak3)
    code
= [%quote function | `A x -> x | `B (_, foo) -> foo]
|}];;

[%quote function | `A x -> x | `B (_, foo) -> foo | _ -> 42];;
[%%expect {|
- : (([> `A of int | `B of '_weak6 * int ] as '_weak5) -> int) code = [%quote
function | `A x -> x | `B (_, foo) -> foo | _ -> 42]
|}];;

[%quote List.map];;
[%%expect {|
- : (('_weak7 -> '_weak8) -> '_weak7 list -> '_weak8 list) code = [%quote
Stdlib.List.map]
|}];;

[%quote fun x -> 42];;
[%%expect {|
- : ('_weak9 -> int) code = [%quote fun x -> 42]
|}];;

[%quote fun _ -> 42];;
[%%expect {|
- : ('_weak10 -> int) code = [%quote fun _ -> 42]
|}];;

[%quote fun x y -> x];;
[%%expect {|
- : ('_weak11 -> '_weak12 -> '_weak11) code = [%quote fun x y -> x]
|}];;

[%quote fun f x y -> f ~a:y ~b:x];;
[%%expect {|
- : ((a:'_weak13 -> b:'_weak14 -> '_weak15) ->
     '_weak14 -> '_weak13 -> '_weak15)
    code
= [%quote fun f x y -> f ~a:y ~b:x]
|}];;

[%quote fun f x y -> f ?a:y ?b:x];;
[%%expect {|
- : ((?a:'_weak16 -> ?b:'_weak17 -> '_weak18) ->
     '_weak17 option -> '_weak16 option -> '_weak18)
    code
= [%quote fun f x y -> f ?a:y ?b:x]
|}];;

[%quote fun (x, y) -> x + y];;
[%%expect {|
- : (int * int -> int) code = [%quote fun (x, y) -> Stdlib.( + ) x y]
|}];;

[%quote function | _ -> 12];;
[%%expect {|
- : ('_weak19 -> int) code = [%quote function | _ -> 12]
|}];;

[%quote function | x -> x];;
[%%expect {|
- : ('_weak20 -> '_weak20) code = [%quote function | x -> x]
|}];;

[%quote function | 42 -> true | _ -> false];;
[%%expect {|
- : (int -> bool) code = [%quote function | 42 -> true | _ -> false]
|}];;

[%quote function | "foo" -> true | _ -> false];;
[%%expect {|
- : (string -> bool) code = [%quote function | "foo" -> true | _ -> false]
|}];;

[%quote function | (x, y) as z -> (x, y, z)];;
[%%expect {|
- : ('_weak21 * '_weak22 -> '_weak21 * '_weak22 * ('_weak21 * '_weak22)) code
= [%quote function | (x, y) as z -> (x, y, z)]
|}];;

[%quote function | (x, y) -> x + y];;
[%%expect {|
- : (int * int -> int) code = [%quote function | (x, y) -> Stdlib.( + ) x y]
|}];;

[%quote function | (x, y, z) -> x + y - z];;
[%%expect {|
- : (int * int * int -> int) code = [%quote
function | (x, y, z) -> Stdlib.( - ) (Stdlib.( + ) x y) z]
|}];;

[%quote function | `A -> true | `B -> false];;
[%%expect {|
- : (([< `A | `B ] as '_weak23) -> bool) code = [%quote
function | `A -> true | `B -> false]
|}];;

[%quote function | `Foo x -> x | `Bar (y, z) -> y + z | `Baz -> 0];;
[%%expect {|
- : (([< `Bar of int * int | `Baz | `Foo of int ] as '_weak24) -> int) code =
[%quote function | `Foo x -> x | `Bar (y, z) -> Stdlib.( + ) y z | `Baz -> 0]
|}];;

[%quote function | lazy x as l -> Lazy.force l];;
[%%expect {|
- : ('_weak25 Lazy.t -> '_weak25) code = [%quote
function | lazy (x) as l -> Stdlib.Lazy.force l]
|}];;

[%quote fun f x d -> match f x with | res -> res | exception e -> d];;
[%%expect {|
- : (('_weak26 -> '_weak27) -> '_weak26 -> '_weak27 -> '_weak27) code =
[%quote fun f x d -> match f x with | res -> res | (exception e) -> d]
|}];;

[%quote function | Some x -> x | None -> 0];;
[%%expect {|
- : (int option -> int) code = [%quote function | Some (x) -> x | None -> 0]
|}];;

[%quote function | [] -> false | x::xs -> true];;
[%%expect {|
- : ('_weak28 list -> bool) code = [%quote
function | [] -> false | (::) (x, xs) -> true]
|}];;

[%quote fun x d -> match x with | Some y -> y | None -> d];;
[%%expect {|
- : ('_weak29 option -> '_weak29 -> '_weak29) code = [%quote
fun x d -> match x with | Some (y) -> y | None -> d]
|}];;

[%quote fun l -> List.map (fun x -> 2 * x) l];;
[%%expect {|
- : (int list -> int list) code = [%quote
fun l -> Stdlib.List.map (fun x -> Stdlib.( * ) 2 x) l]
|}];;

[%quote fun (type a) (f : a -> a) (x : a) -> f (f x)];;
[%%expect {|
- : (('_a -> '_a) -> '_a -> '_a) code = [%quote
fun (type a) (f : a -> a) (x : a) -> f (f x)]
|}];;

[%quote fun x (type a) (f : a -> a * a) (g : int -> a) -> f (g x)];;
[%%expect {|
- : (int -> ('_a -> '_a * '_a) -> (int -> '_a) -> '_a * '_a) code = [%quote
fun x (type a) (f : a -> (a) * (a)) (g : int -> a) -> f (g x)]
|}];;

[%quote fun (f : 'a. 'a -> 'a) -> f f];;
[%%expect {|
- : (('a. 'a -> 'a) -> '_weak30 -> '_weak30) code = [%quote
fun (f : 'a . a -> a) -> f f]
|}];;

[%quote fun x -> fun x -> fun x -> 42];;
[%%expect {|
- : ('_weak31 -> '_weak32 -> '_weak33 -> int) code = [%quote
fun x -> fun x__1 -> fun x__2 -> 42]
|}];;

[%quote fun x -> fun x -> fun x__1 -> 42];;
[%%expect {|
- : ('_weak34 -> '_weak35 -> '_weak36 -> int) code = [%quote
fun x -> fun x__1 -> fun x__2 -> 42]
|}];;

[%quote let z = 10 in z];;
[%%expect {|
- : int code = [%quote let z = 10 in z]
|}];;

[%quote let (x, y) = (42, 100) in x + y];;
[%%expect {|
- : int code = [%quote let (x, y) = (42, 100) in Stdlib.( + ) x y]
|}];;

[%quote let Some x = Some "foo" in x];;
[%%expect {|
Line 1, characters 8-36:
1 | [%quote let Some x = Some "foo" in x];;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
None

- : string code = [%quote match Some "foo" with | Some (x) -> x]
|}];;

[%quote let x::xs = [1; 2; 3] in x];;
[%%expect {|
Line 1, characters 8-34:
1 | [%quote let x::xs = [1; 2; 3] in x];;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[]

- : int code = [%quote
match (::) (1, ((::) (2, ((::) (3, []))))) with | (::) (x, xs) -> x]
|}];;

[%quote let x::xs = [1; 2; 3] in xs];;
[%%expect {|
Line 1, characters 8-35:
1 | [%quote let x::xs = [1; 2; 3] in xs];;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[]

- : int list code = [%quote
match (::) (1, ((::) (2, ((::) (3, []))))) with | (::) (x, xs) -> xs]
|}];;

[%quote let foo x = (x, x) in foo 42];;
[%%expect {|
- : (int * int) code = [%quote let foo = (fun x -> (x, x)) in foo 42]
|}];;

[%quote let foo = 50 and bar = 15 in foo + bar];;
[%%expect {|
- : int code = [%quote let foo = 50 and bar = 15 in Stdlib.( + ) foo bar]
|}];;

[%quote let x = 42 in let x = x in x];;
[%%expect {|
- : int code = [%quote let x = 42 in let x__1 = x in x__1]
|}];;

[%quote
  let (let+) x f = f x in
  let+ a = 42 in a
];;
[%%expect {|
- : int code = [%quote let (let+) = (fun x f -> f x) in (let+) a = 42 in a]
|}];;

[%quote
  let (let+) x f = Option.map f x in
  let+ a = Some 42
  in a * 2
];;
[%%expect {|
- : int option code = [%quote
let (let+) = (fun x f -> Stdlib.Option.map f x) in
(let+) a = Some 42 in Stdlib.( * ) a 2]
|}];;

[%quote
  let (let*) x f = List.map f x and (and*) = List.combine in
  let* a = [1; 2; 3]
  and* b = [10; 20; 30]
  in a + b
];;
[%%expect {|
- : int list code = [%quote
let (let*) = (fun x f -> Stdlib.List.map f x)
and (and*) = Stdlib.List.combine in
(let*) a = (::) (1, ((::) (2, ((::) (3, [])))))
(and*) b = (::) (10, ((::) (20, ((::) (30, []))))) in Stdlib.( + ) a b]
|}];;

[%quote fun (f: int -> int) (x: int) -> f x]
[%%expect {|
- : ((int -> int) -> int -> int) code = [%quote
fun (f : int -> int) (x : int) -> f x]
|}];;

[%quote let module M = Set.Make(Int) in M.singleton 100 |> M.elements];;
[%%expect {|
- : Int.t list code = [%quote
let module M = Stdlib.Set.Make(Stdlib.Int) in M.elements (M.singleton 100)]
|}];;

[%quote ref 42];;
[%%expect {|
- : int ref code = [%quote Stdlib.ref 42]
|}];;

[%quote
  let x = ref 0 in
  for i = 0 to 10 do
    x := !x + i
  done;
  !x
];;
[%%expect {|
- : int code = [%quote
let x = (Stdlib.ref 0) in
for i = 0 to 10 do (Stdlib.( := ) x (Stdlib.( + ) (Stdlib.( ! ) x) i)) done;
Stdlib.( ! ) x]
|}];;

[%quote
  let x = ref 0 in
  for i = 10 downto 0 do
    x := !x + i
  done;
  !x
];;
[%%expect {|
- : int code = [%quote
let x = (Stdlib.ref 0) in
for i = 10 downto 0 do (Stdlib.( := ) x (Stdlib.( + ) (Stdlib.( ! ) x) i))
done; Stdlib.( ! ) x]
|}];;

[%quote while true do () done];;
[%%expect {|
- : 'a code = [%quote while true do () done]
|}];;

[%quote
  let f = ref 1 and i = ref 5 in
  while !i > 0 do
    f := !i * !f;
    i := !i - 1
  done;
  !f
];;
[%%expect {|
- : int code = [%quote
let f = (Stdlib.ref 1) and i = (Stdlib.ref 5) in
while Stdlib.( > ) (Stdlib.( ! ) i) 0 do
(Stdlib.( := ) f (Stdlib.( * ) (Stdlib.( ! ) i) (Stdlib.( ! ) f));
 Stdlib.( := ) i (Stdlib.( - ) (Stdlib.( ! ) i) 1))
done; Stdlib.( ! ) f]
|}];;

[%quote assert true];;
[%%expect {|
- : unit code = [%quote assert true]
|}];;

[%quote assert false];;
[%%expect {|
- : 'a code = [%quote assert false]
|}];;

[%quote lazy 42];;
[%%expect {|
- : int lazy_t code = [%quote lazy 42]
|}];;

[%quote fun () -> #25n];;
[%%expect {|
- : (unit -> nativeint#) code = [%quote fun () -> #25n]
|}];;

[%quote fun () -> #25l];;
[%%expect {|
- : (unit -> int32#) code = [%quote fun () -> #25l]
|}];;

[%quote fun () -> #25L];;
[%%expect {|
- : (unit -> int64#) code = [%quote fun () -> #25L]
|}];;

[%quote fun () -> #6.0];;
[%%expect {|
- : (unit -> float#) code = [%quote fun () -> #6.0]
|}];;

[%quote fun () -> #6.0s];;
[%%expect {|
- : (unit -> float32#) code = [%quote fun () -> #6.0s]
|}];;

[%quote fun () -> #(1, 2, 3)];;
[%%expect {|
- : (unit -> #(int * int * int)) code = [%quote fun () -> #(1, 2, 3)]
|}];;

type rcd = {x: int; y: string};;
[%%expect {|
type rcd = { x : int; y : string; }
|}];;

[%quote {x = 42; y = "foo"}];;
[%%expect {|
- : rcd code = [%quote { x = 42; y = "foo"; }]
|}];;

type rcd_u = #{xu: int; yu: string};;
[%%expect {|
type rcd_u = #{ xu : int; yu : string; }
|}];;

[%quote fun () -> #{xu = 42; yu = "foo"}];;
[%%expect {|
- : (unit -> rcd_u) code = [%quote fun () -> { xu = 42; yu = "foo"; }]
|}];;

[%quote fun r -> r.x];;
[%%expect {|
- : (rcd -> int) code = [%quote fun r -> r.x]
|}];;

[%quote fun {x; y} -> x];;
[%%expect {|
- : (rcd -> int) code = [%quote fun {x=x; y=y; } -> x]
|}];;

[%quote raise (Match_failure ("foo", 42, 100))];;
[%%expect {|
- : 'a code = [%quote Stdlib.raise (Match_failure ("foo", 42, 100))]
|}];;

[%quote raise Out_of_memory];;
[%%expect {|
- : 'a code = [%quote Stdlib.raise Out_of_memory]
|}];;

[%quote raise (Invalid_argument "arg")];;
[%%expect {|
- : 'a code = [%quote Stdlib.raise (Invalid_argument "arg")]
|}];;

[%quote raise (Failure "fail")];;
[%%expect {|
- : 'a code = [%quote Stdlib.raise (Failure "fail")]
|}];;

[%quote raise Not_found];;
[%%expect {|
- : 'a code = [%quote Stdlib.raise Not_found]
|}];;

[%quote raise (Sys_error "err")];;
[%%expect {|
- : 'a code = [%quote Stdlib.raise (Sys_error "err")]
|}];;

[%quote raise End_of_file];;
[%%expect {|
- : 'a code = [%quote Stdlib.raise End_of_file]
|}];;

[%quote raise Division_by_zero];;
[%%expect {|
- : 'a code = [%quote Stdlib.raise Division_by_zero]
|}];;

[%quote raise Stack_overflow];;
[%%expect {|
- : 'a code = [%quote Stdlib.raise Stack_overflow]
|}];;

[%quote raise Sys_blocked_io];;
[%%expect {|
- : 'a code = [%quote Stdlib.raise Sys_blocked_io]
|}];;

[%quote raise (Assert_failure ("assert", 42, 100))];;
[%%expect {|
- : 'a code = [%quote Stdlib.raise (Assert_failure ("assert", 42, 100))]
|}];;

[%quote raise (Undefined_recursive_module ("M", 42, 100))];;
[%%expect {|
- : 'a code = [%quote
Stdlib.raise (Undefined_recursive_module ("M", 42, 100))]
|}];;

[%quote let exception E in ()];;
[%%expect {|
- : unit code = [%quote let exception E in ()]
|}];;

[%quote let exception E in raise E];;
[%%expect {|
- : 'a code = [%quote let exception E in Stdlib.raise E]
|}];;

[%quote let module M = Option in M.map];;
[%%expect {|
- : (('_weak37 -> '_weak38) -> '_weak37 option -> '_weak38 option) code =
[%quote let module M = Stdlib.Option in M.map]
|}];;

[%quote let module M = Option in function | M.None -> false | M.Some x -> x];;
[%%expect {|
- : (bool option -> bool) code = [%quote
let module M = Stdlib.Option in function | None -> false | Some (x) -> x]
|}];;

[%quote fun () -> exclave_ Some 42];;
[%%expect {|
- : (unit -> local_ int option) code = [%quote fun () -> exclave_ Some 42]
|}];;

[%quote fun () -> exclave_ stack_ (Some 42)];;
[%%expect {|
- : (unit -> local_ int option) code = [%quote
fun () -> exclave_ stack_ (Some 42)]
|}];;

module type S = sig
  type t
  type t2
  val a : t
  val b : t -> int -> t
  val c : t -> int
end;;
[%%expect {|
module type S =
  sig type t type t2 val a : t val b : t -> int -> t val c : t -> int end
|}];;

[%quote fun (module _ : S) x -> 42];;
[%%expect {|
- : ((module S) -> '_weak39 -> int) code = [%quote
fun (module _ : S) x -> 42]
|}];;

[%quote fun (module M : S) x -> M.c (M.b M.a x)];;
[%%expect {|
- : ((module S) -> int -> int) code = [%quote
fun (module M : S) x -> M.c (M.b M.a x)]
|}];;

[%quote fun (module M : S with type t = string) x -> M.c (M.b M.a x)];;
[%%expect {|
- : ((module S with type t = string) -> int -> int) code = [%quote
fun (module M : S with type t = string) x -> M.c (M.b M.a x)]
|}];;

[%quote fun (module M : S with type t = string and type t2 = int) x -> M.c (M.b M.a x)];;
[%%expect {|
- : ((module S with type t = string and type t2 = int) -> int -> int) code =
[%quote
fun (module M : S with type t = string and type t2 = int) x ->
M.c (M.b M.a x)]
|}];;

[%quote let module M (N : S) = struct type t = N.t let x = N.a end in ()];;
[%%expect {|
>> Fatal error: Cannot quote struct..end blocks
Uncaught exception: Misc.Fatal_error

|}];;

[%quote let module M = struct type t = int let x = 42 end in M.x];;
[%%expect {|
>> Fatal error: Cannot quote struct..end blocks
Uncaught exception: Misc.Fatal_error

|}];;

let x = 42 in [%quote x];;
[%%expect {|
>> Fatal error: Cannot quote free variable x
Uncaught exception: Misc.Fatal_error

|}];;

let x = [%quote 123] in [%quote !#(x)];;
[%%expect {|
- : int code = [%quote 123]
|}];;

[%quote let o = object method f = 1 end in o#f];;
[%%expect {|
>> Fatal error: Cannot quote object construction.
Uncaught exception: Misc.Fatal_error

|}];;

[%quote fun x -> !#[%quote x]];;
[%%expect {|
- : ('_weak40 -> '_weak40) code = [%quote fun x -> x]
|}];;

[%quote !#[%quote 42]];;
[%%expect {|
- : int code = [%quote 42]
|}];;

[%quote !#[%quote "foo"]];;
[%%expect {|
- : string code = [%quote "foo"]
|}];;

[%quote fun x -> !#((fun y -> [%quote !#y + !#y]) [%quote x])];;
[%%expect {|
- : (int -> int) code = [%quote fun x -> Stdlib.( + ) x x]
|}];;

[%quote !#((fun y -> [%quote !#y + !#y]) [%quote 2])];;
[%%expect {|
- : int code = [%quote Stdlib.( + ) 2 2]
|}];;

[%quote [%quote !#[%quote 123]]];;
[%%expect {|
- : int code code = [%quote [%quote !#[%quote 123]]]
|}];;

let x = [%quote "foo"] and y = [%quote "bar"] in [%quote !#x ^ !#y];;
[%%expect {|
- : string code = [%quote Stdlib.( ^ ) "foo" "bar"]
|}];;

[%quote fun x -> [%quote [%quote !#x]]];;
[%%expect {|
- : ('_weak41 code -> '_weak41 code code) code = [%quote
fun x -> [%quote [%quote !#x]]]
|}];;
