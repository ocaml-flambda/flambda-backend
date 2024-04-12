(* TEST
  expect;
*)

module type Typ = sig type t end

module type Add = sig type t val add : t -> t -> t end

let id {T : Typ} (x : T.t) = x

let id2 : {T : Typ} -> T.t -> T.t =
  fun {A : Typ} (x : A.t) -> x

[%%expect{|
module type Typ = sig type t end
module type Add = sig type t val add : t -> t -> t end
val id : {T : Typ} -> T.t -> T.t = <fun>
val id2 : {T : Typ} -> T.t -> T.t = <fun>
|}]


let f x y = (id {Int} x, id {Bool} y)

[%%expect{|
val f : Int.t -> Bool.t -> Int.t * Bool.t = <fun>
|}]

let merge {T : Typ} x y = (id {T} x, id {T} y)

[%%expect{|
val merge : {T : Typ} -> T.t -> T.t -> T.t * T.t = <fun>
|}]

let test_lambda a = (fun {T : Typ} (x : T.t) -> x) {Int} a

[%%expect{|
val test_lambda : Int.t -> Int.t = <fun>
|}]


let alpha_equiv (f : {A : Add} -> A.t -> A.t) : {T : Add} -> T.t -> T.t = f

[%%expect{|
val alpha_equiv : ({A : Add} -> A.t -> A.t) -> {T : Add} -> T.t -> T.t =
  <fun>
|}]

let apply_weird {M : Typ} (f : {M : Typ} -> _) (x : M.t) : M.t = f {M} x

[%%expect{|
val apply_weird : {M : Typ} -> ({M : Typ} -> M/2.t -> M/2.t) -> M.t -> M.t =
  <fun>
|}]

(* Invalid arguments *)

let f x {M : Typ} (y : M.t) = (x, y)

[%%expect{|
val f : 'a -> {M : Typ} -> M.t -> 'a * M.t = <fun>
|}]

let invalid_arg1 = f {Int}

[%%expect{|
Line 1, characters 19-20:
1 | let invalid_arg1 = f {Int}
                       ^
Error: This expression has type "'a -> {M : Typ} -> M.t -> 'a * M.t"
       But was applied to a module.
|}]

let invalid_arg2 = f 3 4 {Int}

[%%expect{|
Line 1, characters 19-20:
1 | let invalid_arg2 = f 3 4 {Int}
                       ^
Error: This expression has type "'a -> {M : Typ} -> M.t -> 'a * M.t"
       But was applied to an expression.
|}]

let labelled {M : Typ} ~(y:M.t) = y

let apply_labelled = labelled ~y:3 {Int}

[%%expect{|
val labelled : {M : Typ} -> y:M.t -> M.t = <fun>
val apply_labelled : Int.t = 3
|}]

let apply_labelled_fail = labelled ~y:3

[%%expect{|
Line 1, characters 26-34:
1 | let apply_labelled_fail = labelled ~y:3
                              ^^^^^^^^
Error: This expression has type "y:M.t -> M.t"
       Received an expression argument. However, module arguments cannot be omitted.
|}]

let apply_opt (f : ?opt:int -> {M : Typ} -> M.t) = f {Int}

[%%expect{|
val apply_opt : (?opt:int -> {M : Typ} -> M.t) -> Int.t = <fun>
|}]

(* let f_labelled_marg ~{M : Typ} ~{N : Typ} (x : M.t) (y : N.t) = (y, x)

let apply_labelled_m = f_labelled_marg ~N:{ Int } ~M:{ Float} 0.3 1

[%%expect{|
val f_labelled_marg : M:{M : Typ} -> N:{N : Typ} -> M.t -> N.t -> N.t * M.t =
  <fun>
val apply_labelled_m : Int.t * Float.t = (1, 0.3)
|}] *)

(* Typing rules make sense only if module argument are
   a path (module names, projections and applications) *)
let x_from_struct = id {struct type t = int end} 3

[%%expect{|
Line 1, characters 24-47:
1 | let x_from_struct = id {struct type t = int end} 3
                            ^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot infer path of module for functor.
|}]


module type Map = sig
  type _ t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

let map {M : Map} f x = M.map f x

[%%expect{|
module type Map = sig type _ t val map : ('a -> 'b) -> 'a t -> 'b t end
val map : {M : Map} -> ('a -> 'b) -> 'a M.t -> 'b M.t = <fun>
|}]


let s_list = map {List} string_of_int [3; 1; 4]

[%%expect{|
val s_list : string List.t = ["3"; "1"; "4"]
|}]

let s_list : string list = s_list

[%%expect{|
val s_list : string list = ["3"; "1"; "4"]
|}]

module MapCombin (M1 : Map) (M2 : Map) = struct
  type 'a t = 'a M1.t M2.t
  let map f = map {M2} (map {M1} f)
end

let s_list_array = map {MapCombin(List)(Array)} string_of_int [|[3; 2]; [2]; []|]

[%%expect{|
module MapCombin :
  functor (M1 : Map) (M2 : Map) ->
    sig
      type 'a t = 'a M1.t M2.t
      val map : ('a -> 'b) -> 'a M1.t M2.t -> 'b M1.t M2.t
    end
val s_list_array : string MapCombin(List)(Array).t =
  [|["3"; "2"]; ["2"]; []|]
|}]


let s_list_arrayb =
    map {MapCombin(struct type 'a t = 'a list let map = List.map end)(Array)}
    [|[3; 2]; [2]; []|]

[%%expect{|
Line 2, characters 9-76:
2 |     map {MapCombin(struct type 'a t = 'a list let map = List.map end)(Array)}
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot infer path of module for functor.
|}]



(** Various tests on the coercion between functor types. **)
(* Here the sames rules as with first-class modules applies :
   coercion is allowed only if the runtime representation is the same.
*)

module type AddSub = sig
  type t
  val add : t -> t -> t
  val sub : t -> t -> t
end

module type SubAdd = sig
  type t
  val sub : t -> t -> t
  val add : t -> t -> t
end

[%%expect{|
module type AddSub =
  sig type t val add : t -> t -> t val sub : t -> t -> t end
module type SubAdd =
  sig type t val sub : t -> t -> t val add : t -> t -> t end
|}]

module type Typ' = sig
  type t
end

let id3 : {T : Typ'} -> T.t -> T.t = id

[%%expect{|
module type Typ' = sig type t end
val id3 : {T : Typ'} -> T.t -> T.t = <fun>
|}]


let id4 = (id :> {T : Typ} -> T.t -> T.t)

[%%expect{|
val id4 : {T : Typ} -> T.t -> T.t = <fun>
|}]

let id5 = (id :> {T : Typ'} -> T.t -> T.t)

[%%expect{|
val id5 : {T : Typ'} -> T.t -> T.t = <fun>
|}]


(* Fails because this would require computation at runtime *)
let try_coerce (f : {A : Add} -> A.t -> A.t) : {T : Typ} -> T.t -> T.t = f

[%%expect{|
Line 1, characters 73-74:
1 | let try_coerce (f : {A : Add} -> A.t -> A.t) : {T : Typ} -> T.t -> T.t = f
                                                                             ^
Error: This expression has type "{A : Add} -> A.t -> A.t"
       but an expression was expected of type "{T : Typ} -> T.t -> T.t"
       The two module argument types differ by their runtime size.
|}]


(* Here the coercion requires computation and should? be forbidden *)
let try_coerce2 (f : {A : AddSub} -> A.t -> A.t) = (f :> ({T : SubAdd} -> T.t -> T.t))

[%%expect{|
Line 1, characters 51-86:
1 | let try_coerce2 (f : {A : AddSub} -> A.t -> A.t) = (f :> ({T : SubAdd} -> T.t -> T.t))
                                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "{A : AddSub} -> A.t -> A.t" is not a subtype of
         "{T : SubAdd} -> T.t -> T.t"
       The two module argument types do not share
       the same positions for runtime components.
       For example, the value "add" occurs at the expected position of
       the value "sub".
|}]


(* Here the coercion does not require any computation and thus could be allowed *)
let try_coerce3 (f : {A : Add} -> A.t -> A.t) = (f :> {T : Typ} -> T.t -> T.t)

[%%expect{|
Line 1, characters 48-78:
1 | let try_coerce3 (f : {A : Add} -> A.t -> A.t) = (f :> {T : Typ} -> T.t -> T.t)
                                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "{A : Add} -> A.t -> A.t" is not a subtype of
         "{T : Typ} -> T.t -> T.t"
       The two module argument types differ by their runtime size.
|}]

module type Add2 = sig
  type a
  type t
  val add : t -> t -> t
end

module type Add3 = sig
  type t
  type a
  val add : t -> t -> t
end

module type Add4 = sig
  type t
  val add : t -> t -> t
  type a
end

[%%expect{|
module type Add2 = sig type a type t val add : t -> t -> t end
module type Add3 = sig type t type a val add : t -> t -> t end
module type Add4 = sig type t val add : t -> t -> t type a end
|}]

let try_coerce4 (f : {A : Add2} -> A.t -> A.t) : {A : Add} -> A.t -> A.t = f

[%%expect{|
Line 1, characters 75-76:
1 | let try_coerce4 (f : {A : Add2} -> A.t -> A.t) : {A : Add} -> A.t -> A.t = f
                                                                               ^
Error: This expression has type "{A : Add2} -> A.t -> A.t"
       but an expression was expected of type "{A : Add} -> A.t -> A.t"
       Modules do not match: Add is not included in Add2
       The type "a" is required but not provided
|}]

let coerce5 (f : {A : Add2} -> A.t -> A.t) = (f :> {A : Add} -> A.t -> A.t)

let try_coerce6 (f : {A : Add2} -> A.t -> A.t) : {A : Add3} -> A.t -> A.t = f
let try_coerce7 (f : {A : Add2} -> A.t -> A.t) : {A : Add4} -> A.t -> A.t = f

[%%expect{|
val coerce5 : ({A : Add2} -> A.t -> A.t) -> {A : Add} -> A.t -> A.t = <fun>
val try_coerce6 : ({A : Add2} -> A.t -> A.t) -> {A : Add3} -> A.t -> A.t =
  <fun>
val try_coerce7 : ({A : Add2} -> A.t -> A.t) -> {A : Add4} -> A.t -> A.t =
  <fun>
|}]


(** Tests about unannoted applications *)

let apply f {T : Typ} (x : T.t) : T.t = f {T} x

[%%expect{|
Line 3, characters 43-44:
3 | let apply f {T : Typ} (x : T.t) : T.t = f {T} x
                                               ^
Error: Cannot infer signature of functor.
|}]

let apply_with_annot f {T : Typ} (x : T.t) : T.t =
  let _g : {T : Typ} -> T.t -> T.t = f in
  f {T} x

(* (type a) -> a -> a -> a *)
let merge_no_mod (type a) (x : a) (y : a) = x

[%%expect{|
val apply_with_annot : ({T : Typ} -> T.t -> T.t) -> {T : Typ} -> T.t -> T.t =
  <fun>
val merge_no_mod : 'a -> 'a -> 'a = <fun>
|}]


let apply_small_annot1 (f : {T : Typ} -> T.t -> T.t) g {T : Typ} x =
  let r = g {T} x in
  let _ = merge_no_mod f g in
  r

[%%expect{|
Line 2, characters 13-14:
2 |   let r = g {T} x in
                 ^
Error: Cannot infer signature of functor.
|}]


let apply_small_annot2 (f : {T : Typ} -> T.t -> T.t) g {T : Typ} x =
  let _ = merge_no_mod f g in
  g {T} x

[%%expect{|
val apply_small_annot2 :
  ({T : Typ} -> T.t -> T.t) ->
  ({T : Typ} -> T.t -> T.t) -> {T : Typ} -> T.t -> T.t = <fun>
|}]


(* This is a syntax error *)
(* let id_bool_fail {B : module type of Bool} (x : B.t) = x *)

module type TBool = module type of Bool
let id_bool {B : TBool} (x : B.t) = x

[%%expect{|
module type TBool =
  sig
    type t = bool = false | true
    val not : bool -> bool
    external ( && ) : bool -> bool -> bool = "%sequand"
    external ( || ) : bool -> bool -> bool = "%sequor"
    val equal : bool -> bool -> bool
    val compare : bool -> bool -> int
    val to_int : bool -> int
    val to_float : bool -> float
    val to_string : bool -> string
    val seeded_hash : int -> bool -> int
    val hash : bool -> int
  end
val id_bool : {B : TBool} -> B.t -> B.t = <fun>
|}]


(** Escape errors **)

let r = ref None

let set {T : Typ} (x : T.t) =
  r := Some x

[%%expect{|
val r : '_weak1 option ref = {contents = None}
Line 6, characters 12-13:
6 |   r := Some x
                ^
Error: This expression has type "T.t" but an expression was expected of type
         "'weak1"
       The type constructor "T.t" would escape its scope
|}]


let f x {A : Add} (y : A.t) = A.add x y

[%%expect{|
Line 1, characters 36-37:
1 | let f x {A : Add} (y : A.t) = A.add x y
                                        ^
Error: This expression has type "'a" but an expression was expected of type "A.t"
       The type constructor "A.t" would escape its scope
|}]

let f (x : {T : Typ} -> _) : {T : Typ} -> T.t = x

[%%expect{|
Line 1, characters 48-49:
1 | let f (x : {T : Typ} -> _) : {T : Typ} -> T.t = x
                                                    ^
Error: This expression has type "{T : Typ} -> 'a"
       but an expression was expected of type "{T : Typ} -> T.t"
       The module "T" would escape its scope
|}]


(* Testing the `S with type t = _` cases *)

module type Coerce = sig
  type a
  type b
  val coerce : a -> b
end

let coerce {C : Coerce} x = C.coerce x

module IntofBool = struct
  type a = bool
  type b = int
  let coerce b = if b then 1 else 0
end

module BoolofInt = struct
  type a = int
  type b = bool
  let coerce i = i <> 0
end

module IntofString = struct
  type a = string
  type b = int
  let coerce = int_of_string
end

module IntofFloat = struct
  type a = float
  type b = int
  let coerce = int_of_string_opt
end

[%%expect {|
module type Coerce = sig type a type b val coerce : a -> b end
val coerce : {C : Coerce} -> C.a -> C.b = <fun>
module IntofBool :
  sig type a = bool type b = int val coerce : bool -> int end
module BoolofInt :
  sig type a = int type b = bool val coerce : int -> bool end
module IntofString :
  sig type a = string type b = int val coerce : string -> int end
module IntofFloat :
  sig type a = float type b = int val coerce : string -> int option end
|}]

let incr_general
  {Cfrom : Coerce with type b = int}
  {Cto : Coerce with type a = int and type b = Cfrom.a}
  x =
  coerce {Cto} (1 + coerce {Cfrom} x)

[%%expect {|
val incr_general :
  {Cfrom : Coerce with type b = int} ->
  {Cto : Coerce with type a = int and type b = Cfrom.a} -> Cfrom.a -> Cto.b =
  <fun>
|}]

module type CoerceToInt = sig
  type a
  type b = int
  val coerce : a -> int
end

module type CoerceFromInt = sig
  type a = int
  type b
  val coerce : int -> b
end

let incr_general' :
  {C1 : CoerceToInt} -> {C2 : CoerceFromInt with type b = C1.a} -> C1.a -> C1.a =
  incr_general

[%%expect{|
module type CoerceToInt = sig type a type b = int val coerce : a -> int end
module type CoerceFromInt = sig type a = int type b val coerce : int -> b end
val incr_general' :
  {C1 : CoerceToInt} ->
  {C2 : CoerceFromInt with type b = C1.a} -> C1.a -> C1.a = <fun>
|}]

(* Recursive and mutually recursive definitions *)

let rec f : {T : Typ} -> int -> T.t -> T.t -> T.t =
  fun {T : Typ} n (x : T.t) (y : T.t) ->
    if n = 0
    then x
    else f {T} (n - 1) y x

[%%expect{|
val f : {T : Typ} -> int -> T.t -> T.t -> T.t = <fun>
|}]

let rec f {T : Typ} n (x : T.t) (y : T.t) =
  if n = 0
  then x
  else f {T} (n - 1) y x

[%%expect{|
Line 4, characters 10-11:
4 |   else f {T} (n - 1) y x
              ^
Error: Cannot infer signature of functor.
|}]

let rec f {T : Typ} n (x : T.t) (y : T.t) =
  if n = 0
  then x
  else g {T} x y
and g {T : Typ} n (x : T.t) (y : T.t) =
  if n = 0
  then y
  else f {T} x y

[%%expect{|
Line 4, characters 10-11:
4 |   else g {T} x y
              ^
Error: Cannot infer signature of functor.
|}]

let rec m = map {List} (fun x -> x) [3]

let rec m = map {List} (fun x -> x) [3]
and g = 3 :: m

[%%expect{|
val m : int List.t = [3]
val m : int List.t = [3]
val g : int list = [3; 3]
|}]

let rec f {T : Typ} x =
  g x
and g x = f {Int} x

[%%expect{|
val f : {T : Typ} -> 'a -> 'b = <fun>
val g : 'a -> 'b = <fun>
|}]

let rec m = (fun {T : Typ} (x : T.t) -> x) {Int} 3

[%%expect{|
val m : Int.t = 3
|}]
