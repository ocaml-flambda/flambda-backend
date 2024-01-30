type valnum = int

(* Classification of operations *)

type op_class =
  | Op_pure           (* pure arithmetic, produce one or several result *)
  | Op_load of Mach.mutable_flag (* memory load *)
  | Op_store of bool  (* memory store, false = init, true = assign *)
  | Op_other   (* anything else that does not allocate nor store in memory *)

module type Operation = sig
  type t
  (* CR-someday xclerc for xclerc: consider asking for a `compare` function. *)
end

module type S = sig

  type op

  type rhs = op * valnum array

  module Equations : sig
    module Rhs_map : Map.S with type key = rhs

    type 'a t =
      { mutable_load_equations : 'a Rhs_map.t;
        other_equations : 'a Rhs_map.t }
  end

  type numbering =
    { num_next: int;                      (* next fresh value number *)
      num_eqs: valnum array Equations.t;  (* mapping rhs -> valnums *)
      num_reg: valnum Reg.Map.t }         (* mapping register -> valnum *)

  val empty_numbering : numbering

  val valnum_regs : numbering -> Reg.t array -> numbering * valnum array

  val find_equation : op_class -> numbering -> rhs -> valnum array option

  val find_regs_containing : numbering -> valnum array -> Reg.t array option

  val set_known_regs : numbering -> Reg.t array -> valnum array -> numbering

  val set_move : numbering -> Reg.t -> Reg.t -> numbering

  val set_fresh_regs : numbering -> Reg.t array -> rhs -> op_class -> numbering

  val set_unknown_regs : numbering -> Reg.t array -> numbering

  val remove_mutable_load_numbering : numbering -> numbering

  val kill_addr_regs : numbering -> numbering

end

let array_fold2 f n a1 a2 =
  let l = Array.length a1 in
  assert (l = Array.length a2);
  let n = ref n in
  for i = 0 to l - 1 do n := f !n a1.(i) a2.(i) done;
  !n


module Make (Op : Operation) : S with type op = Op.t = struct

  type op = Op.t

  (* We maintain sets of equations of the form
       valnums = operation(valnums)
   plus a mapping from registers to valnums (value numbers). *)

type rhs = op * valnum array

module Equations = struct
  module Rhs_map =
    Map.Make(struct type t = rhs let compare = Stdlib.compare end)

  type 'a t =
    { mutable_load_equations : 'a Rhs_map.t;
      other_equations : 'a Rhs_map.t }

  let empty =
    { mutable_load_equations = Rhs_map.empty;
      other_equations = Rhs_map.empty }

  let add op_class op v m =
    match op_class with
    | Op_load Mutable ->
      { m with mutable_load_equations =
                 Rhs_map.add op v m.mutable_load_equations }
    | _ ->
      { m with other_equations = Rhs_map.add op v m.other_equations }

  let find op_class op m =
    match op_class with
    | Op_load Mutable ->
      Rhs_map.find op m.mutable_load_equations
    | _ ->
      Rhs_map.find op m.other_equations

  let remove_mutable_loads m =
    { mutable_load_equations = Rhs_map.empty;
      other_equations = m.other_equations }
end

type numbering =
  { num_next: int;                      (* next fresh value number *)
    num_eqs: valnum array Equations.t;  (* mapping rhs -> valnums *)
    num_reg: valnum Reg.Map.t }         (* mapping register -> valnum *)

let empty_numbering =
  { num_next = 0; num_eqs = Equations.empty; num_reg = Reg.Map.empty }

(** Generate a fresh value number [v] and associate it to register [r].
  Returns a pair [(n',v)] with the updated value numbering [n']. *)

let fresh_valnum_reg n r =
  let v = n.num_next in
  ({n with num_next = v + 1; num_reg = Reg.Map.add r v n.num_reg}, v)

(* Same, for a set of registers [rs]. *)

let array_fold_transf (f: numbering -> 'a -> numbering * 'b) n (a: 'a array)
                      : numbering * 'b array =
  match Array.length a with
  | 0 -> (n, [||])
  | 1 -> let (n', b) = f n a.(0) in (n', [|b|])
  | l -> let b = Array.make l 0 and n = ref n in
         for i = 0 to l - 1 do
           let (n', x) = f !n a.(i) in
           b.(i) <- x; n := n'
         done;
         (!n, b)

let fresh_valnum_regs n rs =
  array_fold_transf fresh_valnum_reg n rs

(** [valnum_reg n r] returns the value number for the contents of
  register [r].  If none exists, a fresh value number is returned
  and associated with register [r].  The possibly updated numbering
  is also returned.  [valnum_regs] is similar, but for an array of
  registers. *)

let valnum_reg n r =
  try
    (n, Reg.Map.find r n.num_reg)
  with Not_found ->
    fresh_valnum_reg n r

let valnum_regs n rs =
  array_fold_transf valnum_reg n rs

(* Look up the set of equations for an equation with the given rhs.
   Return [Some res] if there is one, where [res] is the lhs. *)

let find_equation op_class n rhs =
  try
    Some(Equations.find op_class rhs n.num_eqs)
  with Not_found ->
    None

(* Find a register containing the given value number. *)

let find_reg_containing n v =
  Reg.Map.fold (fun r v' res -> if v' = v then Some r else res)
               n.num_reg None

(* Find a set of registers containing the given value numbers. *)

let find_regs_containing n vs =
  match Array.length vs with
  | 0 -> Some [||]
  | 1 -> begin match find_reg_containing n vs.(0) with
         | None -> None
         | Some r -> Some [|r|]
         end
  | l -> let rs = Array.make l Reg.dummy in
         begin try
           for i = 0 to l - 1 do
             match find_reg_containing n vs.(i) with
             | None -> raise Exit
             | Some r -> rs.(i) <- r
           done;
           Some rs
         with Exit ->
           None
         end

(* Associate the given value number to the given result register,
   without adding new equations. *)

let set_known_reg n r v =
  { n with num_reg = Reg.Map.add r v n.num_reg }

(* Associate the given value numbers to the given result registers,
   without adding new equations. *)

let set_known_regs n rs vs =
  array_fold2 set_known_reg n rs vs

(* Record the effect of a move: no new equations, but the result reg
   maps to the same value number as the argument reg. *)

let set_move n src dst =
  let (n1, v) = valnum_reg n src in
  { n1 with num_reg = Reg.Map.add dst v n1.num_reg }

(* Record the equation [fresh valnums = rhs] and associate the given
   result registers [rs] to [fresh valnums]. *)

let set_fresh_regs n rs rhs op_class =
  let (n1, vs) = fresh_valnum_regs n rs in
  { n1 with num_eqs = Equations.add op_class rhs vs n.num_eqs }

(* Forget everything we know about the given result registers,
   which are receiving unpredictable values at run-time. *)

let set_unknown_regs n rs =
  { n with num_reg = Array.fold_right Reg.Map.remove rs n.num_reg }

(* Keep only the equations satisfying the given predicate. *)

let remove_mutable_load_numbering n =
  { n with num_eqs = Equations.remove_mutable_loads n.num_eqs }

(* Forget everything we know about registers of type [Addr]. *)

let kill_addr_regs n =
  { n with num_reg =
              Reg.Map.filter (fun r _n -> r.Reg.typ <> Cmm.Addr) n.num_reg }

end
