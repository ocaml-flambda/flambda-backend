(* TEST
   include stdlib_stable;
   include stdlib_upstream_compatible;
   expect;
*)

(* This test demonstrates that the array reinterpret operations are not
   supported on bytecode. We'd like to change this one day. *)

type t = int64# array
let a = [| #0L |]
[%%expect{|
type t = int64# array
val a : int64# array = [|<abstr>|]
|}]

external[@layout_poly] reinterp_get :
  ('a : any). t -> int -> 'a = "%obj_reinterp_array_unsafe_get"

let _ = reinterp_get a 0
[%%expect{|
external reinterp_get : ('a : any). t -> int -> 'a
  = "%obj_reinterp_array_unsafe_get" [@@layout_poly]
>> Fatal error: Array reinterpret operations are not yet supported in bytecode.  Please report this error to the Jane Street compilers team if their absence is causing difficulty.
Uncaught exception: Misc.Fatal_error

|}]

external[@layout_poly] reinterp_set :
  ('a : any). t -> int -> 'a -> unit = "%obj_reinterp_array_unsafe_set"

let _ = reinterp_set a 0 #1L
[%%expect{|
external reinterp_set : ('a : any). t -> int -> 'a -> unit
  = "%obj_reinterp_array_unsafe_set" [@@layout_poly]
>> Fatal error: Array reinterpret operations are not yet supported in bytecode.  Please report this error to the Jane Street compilers team if their absence is causing difficulty.
Uncaught exception: Misc.Fatal_error

|}]
