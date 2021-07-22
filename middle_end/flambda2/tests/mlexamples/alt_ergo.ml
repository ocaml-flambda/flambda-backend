type borne =
  | Strict of (int * int)
  | Large of (int * int)
  | Pinfty | Minfty

type t = (borne * borne) list * int * int

let any () = Obj.magic (Sys.opaque_identity 0)

let inv_borne_sup b is_int ~other =
  match b with
  | Minfty -> assert false
  | Pinfty -> assert false
    (* if is_int then Large (Q.zero, explain_borne other)
     * else Strict (Q.zero, explain_borne other) *)
  | Strict (c, _) | Large (c, _) when (* Q.sign *) any () c (* = 0 *) -> Minfty
  | Strict (v, e) -> Strict ((* Q.div Q.one v *)any (), e)
  | Large (v, e) -> Large ((* Q.div Q.one v *)any (), e)

let inv_bornes (l, u) is_int =
  inv_borne_sup u is_int ~other:l, u

let inv (l, is_int, _) =
  match l with
  | [] | _::_::_ -> assert false
  | [l,u] -> [inv_bornes (l, u) is_int]

let div i1 i2 =
  let _inv_i2 = inv i2 in
  assert false

