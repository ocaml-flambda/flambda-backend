(* Both these functions are marked zero_alloc in the mli.  We expect them to get
   checked, even though they aren't marked zero-alloc here, and should get an
   error for the second. *)
let[@inline never] f_zero_alloc x = x

let[@inline never] f_alloc x = (x, x)

(* Here, [M_alloc_var.f_alloc2] allocates, and the mli marks it as zero_alloc,
   so we'll get an error for it.  But we don't get an error for its use in
   f_call_var (because the translation to lambda sees the variable on
   [M_alloc_var.f_alloc2] has been instantiated to show it will be checked, and
   puts an assume on [f_call_var].  This is maybe sad, but mirrors the existing
   behavior if [M_alloc_var.f_alloc2] had been explicitly marked zero_alloc.
*)
module M_alloc_var = struct
  let[@inline never] f_alloc2 x = (x, x)
end

let[@zero_alloc] f_call_var x = M_alloc_var.f_alloc2 x


(* Arity: The typing tests show that if we just give this function the signature
   [val[@zero_alloc] f_arity_one : int -> int -> int] we'll get a type error.
   Here we show that if we give it a signature that passes the typechecker (the
   mli has [val[@zero_alloc (arity 1)] f_arity_one : int -> int -> int]), it
   correctly gets checked and gives a zero_alloc backend error. *)
let f_arity_one x = fun y -> x + y


(* Shadowing: the mli marks [f_shadow] zero_alloc.  That check should only apply
   to the last such function in the file, so we get no error even though there
   is an earlier function with the same name and type that allocates. *)
let f_shadow x = (x, x+1)

let f_shadow _x = (42, 43) (* doesn't allocate because this is static. *)

(* Shadowing: the other way.  This time the function exposed in the signature
   does allocate, and we should get an error for it. *)
let f_shadow_alloc _x = (42, 43)

let f_shadow_alloc x = (x, x)

(* Shadowing: This time both allocate.  We should only get an error the second. *)
let f_shadow_alloc_both x = (x, x)

let f_shadow_alloc_both x = (x, x+1)

(* And now the boring part - just a test that we check each function below with
   the exact parameters implied by the signature (opt, strict). *)

(* these are tagged just [@zero_alloc] in the mli *)
exception E of string
let f_basic_success x =
  if x = 42 then () else
    let s = Printf.sprintf "%d\n" x in
    raise (E s)

let f_basic_fail x = (x, x)

(* These are tagged [@zero_alloc strict] in the mli *)
let f_strict_success x = x + 1

let f_strict_fail x =
  if x = 42 then () else
    let s = Printf.sprintf "%d\n" x in
    raise (E s)

(* These are tagged [@zero_alloc opt] in the mli - the latter should fail just
   when the flag to check opt things is passed. *)
let f_opt_success x =
  if x = 42 then () else
    let s = Printf.sprintf "%d\n" x in
    raise (E s)

let f_opt_fail x = (x, x)

(* These are tagged [@zero_alloc struct opt] in the mli - the latter should fail
   just when the flag to check for opt is passed. *)
let f_strict_opt_success x = x

let f_strict_opt_fail x =
  if x = 42 then () else
    let s = Printf.sprintf "%d\n" x in
    raise (E s)
