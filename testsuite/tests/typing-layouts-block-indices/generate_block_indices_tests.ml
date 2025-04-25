(* CR rtjoa: write generated tests *)

(* We test the following properties of indices:

   1. for all records [r], indices [idx : (r, a) idx_{imm,mut}],
      r.idx == idx_get r idx

   2. for all records [r], indices [idx : (r, a) idx_mut],
      (let r' = copy r in r'.idx <- a; r')
      ==
      (let r' = copy r in idx_set r' a; r')

   3. for all indices [.ba.#ua1.#ua2 ... .#ua_n], 0 < i < n
      (.idx_{imm,mut}( (.ba.#ua1 ... #ua_i) ).#ua_i+1 ... .#ua_n)
      ==
      [.ba.#ua1.#ua2 ... .#ua_n]

   Example tests for each property:

   1. assert (idx_get c (.b) == c.b);
      assert (idx_get c (.b.#a) == c.b.#a)
   2. let c1 = copy c in
      let c2 = copy c in
      c1.b.#a <- a;
      idx_set c2 (.b.#a) a;
      assert (c1 == c2)
   3. assert ((.idx_imm(.b)) == (.b));
      assert ((.idx_imm(.b).#a) == (.b.#a))

   Note that for 3., to actually test indices for equality, we need to use
   magic (see [Idx_repr]).
*)

module Idx_repr : sig
  type t
  val of_idx_imm : 'a ('b : any). ('a, 'b) idx_imm -> t
  val of_idx_mut : 'a ('b : any). ('a, 'b) idx_mut -> t
  val equal : t -> t -> bool
  val debug_string : t -> string
end = struct
  (* See Note [Representation of block indices] in [lambda/translcore.ml] *)
  type t =
    | Bytecode of { path : int list }
    | Native of { offset : int; gap : int }

  external magic_box_bits64 : ('a : bits64) 'b . 'a -> 'b =
    "%box_int64"
  external lessthan_if_bytecode : int -> int -> bool =
    "caml_lessthan" "caml_greaterthan"

  let of_idx idx =
    let is_bytecode = lessthan_if_bytecode 0 1 in
    if is_bytecode then
      let r = Obj.repr (magic_box_bits64 idx) in
      let nth_idx n : int = Obj.magic (Obj.field r n) in
      let path = List.init (Obj.size r) nth_idx in
      Bytecode { path }
    else
      let i : int64 = magic_box_bits64 idx in
      let offset =
        Int64.(logand (sub (shift_left one 48) one)) i
        |> Int64.to_int
      in
      let gap =
        Int64.shift_right i 48
        |> Int64.to_int
      in
      Native { offset; gap }

  let of_idx_imm = of_idx
  let of_idx_mut = of_idx

  let equal t1 t2 =
    match t1, t2 with
    | Bytecode { path = path1 }, Bytecode { path = path2 } ->
      List.equal Int.equal path1 path2
    | Native { gap = gap1; offset = offset1 },
      Native { gap = gap2; offset = offset2 } ->
      Int.equal gap1 gap2 && Int.equal offset1 offset2
    | Bytecode _, Native _ | Native _, Bytecode _ -> assert false

  let debug_string = function
    | Bytecode { path } ->
      Printf.sprintf "{ %s }"
        (String.concat "; " (List.map Int.to_string path))
    | Native { offset; gap } ->
      Printf.sprintf "offset %d; gap %d" offset gap
end
