[@@@zero_alloc all]

let pass1 x = x + 1

exception Exn of (int * int)
let pass2 x = raise (Exn (x,x))

(* can be combined with strict payload *)
let[@zero_alloc strict] pass3 x y = x + y

(* test for ignore  *)
let[@zero_alloc ignore] test1 x = (x, x)

(* assume still works *)
let[@zero_alloc assume][@inline never][@specialise never] fail_loud x = [ x; x+1 ]

let call_loud x = fail_loud (x*2)

(* assume strict still works *)
let[@inline never][@specialise never][@zero_alloc strict assume] fail_loud2 x = raise (Exn (x,x))

let[@zero_alloc strict] call_loud2 x = fail_loud2 (x+1)

(* For duplicate attributes we get a warning and the first one takes effect. *)
let[@inline never][@specialise never][@zero_alloc strict assume][@zero_alloc ignore] fail_loud1 x = raise (Exn (x,x))

let[@zero_alloc strict] call_loud1 x = fail_loud1 (x+1)

module Module : sig
  val add : int -> int -> int [@@zero_alloc]
end = struct
  let add x y = x + y
end
