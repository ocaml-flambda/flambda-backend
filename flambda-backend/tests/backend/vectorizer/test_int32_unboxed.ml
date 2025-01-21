[@@@ocaml.warnerror "+a-40-41-42"]

module Int32_u = struct
  type t = int32#

  external to_int32 : t -> (int32[@local_opt]) = "%box_int32" [@@warning "-187"]

  external of_int32 : (int32[@local_opt]) -> t = "%unbox_int32" [@@warning "-187"]

  let[@inline always] add x y = of_int32 (Int32.add (to_int32 x) (to_int32 y))

  module Array = struct
    external unsafe_create : ('a : bits32). int -> 'a array =
      "caml_make_unboxed_int32_vect_bytecode" "caml_make_unboxed_int32_vect"
    external unsafe_get: ('a : bits32). 'a array -> int -> 'a = "%array_unsafe_get"
    external unsafe_set: ('a : bits32). 'a array -> int -> 'a -> unit = "%array_unsafe_set"

    module Index = struct
      external unsafe_get
        : ('a : bits32).
            ('a array) -> t -> 'a
        = "%array_unsafe_get_indexed_by_int32#"

      external unsafe_set
        : ('a : bits32).
            'a array -> t -> 'a -> unit
        = "%array_unsafe_set_indexed_by_int32#"
    end
  end

end

type t1 = { mutable d0 : int32# ; mutable d1: int32#; mutable d2: int32#; mutable d3: int32#  }

(* Currently, can't vectorize because not adjacent and have an unnecessary sign extension. *)
let[@inline never] [@local never][@specialize never] add_mutable_record (a : t1) (b: t1) (c : t1) : t1 =
  c.d0 <- Int32_u.add a.d0 b.d0;
  c.d1 <- Int32_u.add a.d1 b.d1;
  c.d2 <- Int32_u.add a.d2 b.d2;
  c.d3 <- Int32_u.add a.d3 b.d3;
  c

let[@inline always] copy_array_one (a : Int32_u.t array)
                      (b : Int32_u.t array) pos =
  let x = Int32_u.Array.unsafe_get a pos in
  Int32_u.Array.unsafe_set b pos x

(* The accesses are adjacent but the use of [int] typed index results in a convoluted
   index computation that is not yet handled by the current heuristics. *)
let[@inline never] [@local never][@specialize never] copy_array_four (a : Int32_u.t array)
                                                       (b : Int32_u.t array) ~pos =
  copy_array_one a b pos;
  copy_array_one a b (pos+1);
  copy_array_one a b (pos+2);
  copy_array_one a b (pos+3);
  ()

(*

114:
(id:3) a:V/61 := R:I/0[%rax]
(id:4) b:V/62 := R:I/1[%rbx]
(id:5) pos:I/63 := R:I/2[%rdi]
(id:6) new_value:I/64 := signed int32  mut[a:V/61 + pos:I/63 * 2 + 6]
(id:7) signed int32[b:V/62 + pos:I/63 * 2 + 6] := new_value:I/64 (assign)
(id:8) Parraysetu:I/65 := 1
(id:9) Paddint:I/66 := pos:I/63
(id:10) Paddint:I/66 := Paddint:I/66 + 2
(id:11) new_value:I/67 := signed int32  mut[a:V/61 + Paddint:I/66 * 2 + 6]
(id:12) signed int32[b:V/62 + Paddint:I/66 * 2 + 6] := new_value:I/67 (assign)
(id:13) Parraysetu:I/68 := 1
(id:14) Paddint:I/69 := pos:I/63
(id:15) Paddint:I/69 := Paddint:I/69 + 4
(id:16) new_value:I/70 := signed int32  mut[a:V/61 + Paddint:I/69 * 2 + 6]
(id:17) signed int32[b:V/62 + Paddint:I/69 * 2 + 6] := new_value:I/70 (assign)
(id:18) Parraysetu:I/71 := 1
(id:19) Paddint:I/72 := pos:I/63
(id:20) Paddint:I/72 := Paddint:I/72 + 6
(id:21) new_value:I/73 := signed int32  mut[a:V/61 + Paddint:I/72 * 2 + 6]
(id:22) signed int32[b:V/62 + Paddint:I/72 * 2 + 6] := new_value:I/73 (assign)
(id:23) Parraysetu:I/74 := 1
(id:24) I/75 := 1
(id:25) R:I/0[%rax] := I/75
(id:26) Return R:I/0[%rax]

*)

let[@inline never] [@local never][@specialize never] copy_array_four_v2 (a : Int32_u.t array)
                                                       (b : Int32_u.t array) ~pos =
  let i0 = pos in
  copy_array_one a b i0;
  let i1 = i0 + 1 in
  copy_array_one a b i1;
  let i2 = i1 + 1 in
  copy_array_one a b i2;
  let i3 = i2 + 1 in
  copy_array_one a b i3;
  ()

let[@inline always] copy_array_index_one (a : Int32_u.t array)
                      (b : Int32_u.t array) (pos : Int32_u.t) =
  let x = Int32_u.Array.Index.unsafe_get a pos in
  Int32_u.Array.Index.unsafe_set b pos x

(* Can't vectorize it! The accesses are adjacent and we use [Int32_u.t] as index,
   but the compiler tags the index before using it! This index computation is not
   yet handled by the vectorizer's heuristics. *)
let[@inline never] [@local never][@specialize never] copy_array_index_four (a : Int32_u.t array)
                                                       (b : Int32_u.t array) ~pos =
  copy_array_index_one a b pos;
  copy_array_index_one a b (Int32_u.add pos #1l);
  copy_array_index_one a b (Int32_u.add pos #2l);
  copy_array_index_one a b (Int32_u.add pos #3l);
  ()

let[@inline never] [@local never][@specialize never] copy_array_index_from_start (a : Int32_u.t array)
                                                       (b : Int32_u.t array) =
  let pos = #0l in
  copy_array_index_one a b pos;
  copy_array_index_one a b (Int32_u.add pos #1l);
  copy_array_index_one a b (Int32_u.add pos #2l);
  copy_array_index_one a b (Int32_u.add pos #3l);
   ()

                            let[@inline never] [@local never][@specialize never] copy_array_from_start (a : Int32_u.t array)
                                                                                   (b : Int32_u.t array) =
  let[@inline always] copy pos =
    let x = Int32_u.Array.unsafe_get a pos in
    Int32_u.Array.unsafe_set b pos x
  in
  let pos = 0 in
  copy pos;
  copy (pos+1);
  copy (pos+2);
  copy (pos+3);
  ()

(* Can't vectorize because of an unnecessary sign extension. The heuristics in the
   vectorizer can be extended to handle this case. *)
let[@inline never] [@local never][@specialize never] add_array_from_start (a : Int32_u.t array) (b : Int32_u.t array) =
  let[@inline always] add pos =
    let x = Int32_u.Array.unsafe_get a pos in
    let y = Int32_u.Array.unsafe_get b pos in
    Int32_u.Array.unsafe_set b pos (Int32_u.add x y)
  in
  let pos = 0 in
  add pos;
  add (pos+1);
  add (pos+2);
  add (pos+3);
  ()

(*
camlTest7__add_array_from_start_7_22_code(R:I/0[%rax] R:I/1[%rbx]) {test7.ml:112,74-379}
  a:V/61 := R:I/0[%rax]
  b:V/62 := R:I/1[%rbx]
  I/63 := signed int32  mut[b:V/62 + 8]{test7.ml:119,2-9;test7.ml:115,12-42}
  I/64 := signed int32  mut[a:V/61 + 8]{test7.ml:119,2-9;test7.ml:114,12-42}
  I/65 := I/64
  I/65 := I/65 + I/63{test7.ml:119,2-9;test7.ml:116,35-52;test7.ml:10,41-78}
  new_value:I/66 := sextend32 I/65{test7.ml:119,2-9;test7.ml:116,35-52;test7.ml:10,41-78}
  signed int32[b:V/62 + 8] := new_value:I/66 (assign){test7.ml:119,2-9;test7.ml:116,4-52}
  Parraysetu:I/67 := 1
  I/68 := signed int32  mut[b:V/62 + 12]{test7.ml:120,2-13;test7.ml:115,12-42}
  I/69 := signed int32  mut[a:V/61 + 12]{test7.ml:120,2-13;test7.ml:114,12-42}
  I/70 := I/69
  I/70 := I/70 + I/68{test7.ml:120,2-13;test7.ml:116,35-52;test7.ml:10,41-78}
  new_value:I/71 := sextend32 I/70{test7.ml:120,2-13;test7.ml:116,35-52;test7.ml:10,41-78}
  signed int32[b:V/62 + 12] := new_value:I/71 (assign){test7.ml:120,2-13;test7.ml:116,4-52}
  Parraysetu:I/72 := 1
  I/73 := signed int32  mut[b:V/62 + 16]{test7.ml:121,2-13;test7.ml:115,12-42}
  I/74 := signed int32  mut[a:V/61 + 16]{test7.ml:121,2-13;test7.ml:114,12-42}
  I/75 := I/74
  I/75 := I/75 + I/73{test7.ml:121,2-13;test7.ml:116,35-52;test7.ml:10,41-78}
  new_value:I/76 := sextend32 I/75{test7.ml:121,2-13;test7.ml:116,35-52;test7.ml:10,41-78}
  signed int32[b:V/62 + 16] := new_value:I/76 (assign){test7.ml:121,2-13;test7.ml:116,4-52}
  Parraysetu:I/77 := 1
  I/78 := signed int32  mut[b:V/62 + 20]{test7.ml:122,2-13;test7.ml:115,12-42}
  I/79 := signed int32  mut[a:V/61 + 20]{test7.ml:122,2-13;test7.ml:114,12-42}
  I/80 := I/79
  I/80 := I/80 + I/78{test7.ml:122,2-13;test7.ml:116,35-52;test7.ml:10,41-78}
  new_value:I/81 := sextend32 I/80{test7.ml:122,2-13;test7.ml:116,35-52;test7.ml:10,41-78}
  signed int32[b:V/62 + 20] := new_value:I/81 (assign){test7.ml:122,2-13;test7.ml:116,4-52}
  Parraysetu:I/82 := 1
  I/83 := 1
  R:I/0[%rax] := I/83
  return R:I/0[%rax]
*)
let print_t1 ppf (t1 : t1) =
  Format.fprintf ppf "{ d0 = %ld ; d1 = %ld; d2 = %ld ; d3 = %ld }"
    (Int32_u.to_int32 t1.d0)
    (Int32_u.to_int32 t1.d1)
    (Int32_u.to_int32 t1.d2)
    (Int32_u.to_int32 t1.d3)

let print_array ~len ppf ( a : Int32_u.t array)=
  for i = 0 to len - 1 do
    let x = Int32_u.Array.unsafe_get a i in
    Format.fprintf ppf "%ld " (x |> Int32_u.to_int32)
  done

let create_array ~len ~init =
  let arr = Int32_u.Array.unsafe_create len in
  for i = 0 to len-1 do
    Int32_u.Array.unsafe_set arr i init
  done;
  arr

let () =
  let a = { d0 = #8l; d1 = #96l; d2 = -#10l; d3 = #0l } in
  let b = { d0 = #80l; d1 = #14l; d2 = -#30l; d3 = -#100l } in
  let c = { d0 = #8l; d1 = #96l; d2 = #0l; d3 = #0l } in
  Format.printf "add_mutable_record %a\n" print_t1
    (add_mutable_record a b c);
  let ar1 = create_array ~len:4 ~init:#30l in
  let ar2 = create_array ~len:4 ~init:#0l in
  copy_array_four ar1 ar2 ~pos:0;
  Format.printf "copy_array_four %a\n" (print_array ~len:4) ar2;
  copy_array_index_four ar2 ar1 ~pos:#0l;
  Format.printf "copy_array_index_four %a\n" (print_array ~len:4) ar1;
  add_array_from_start ar1 ar2;
  Format.printf "add_array_from_start %a\n" (print_array ~len:4) ar2;
  copy_array_index_from_start ar2 ar1;
  Format.printf "copy_array_index_from_start %a\n" (print_array ~len:4) ar1;
  copy_array_from_start ar1 ar2;
  Format.printf "copy_array_from_start %a\n" (print_array ~len:4) ar2;
  copy_array_four_v2 ar1 ar2 ~pos:0;
  Format.printf "copy_array_from_start_v2 %a\n" (print_array ~len:4) ar2;
  ()
