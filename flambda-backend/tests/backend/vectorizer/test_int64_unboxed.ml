[@@@ocaml.warnerror "+a-40-41-42"]

module Int64_u = struct
  type t = int64#

  external to_int64 : t -> (int64[@local_opt]) = "%box_int64" [@@warning "-187"]

  external of_int64 : (int64[@local_opt]) -> t = "%unbox_int64" [@@warning "-187"]

  let[@inline always] add x y = of_int64 (Int64.add (to_int64 x) (to_int64 y))
end

type t1 = { mutable d0 : int64# ; mutable d1: int64# }

let[@opaque][@specialize never] add_mutable_record (a : t1) (b: t1) (c : t1) :
     t1 =
  c.d0 <- Int64_u.add a.d0 b.d0;
  c.d1 <- Int64_u.add a.d1 b.d1;
  c

let[@opaque][@specialize never] copy_mutable_record (a : t1) (b: t1) : unit =
  b.d0 <- a.d0;
  b.d1 <- a.d1;
  ()

type t2 = {
  mutable d0 : int64# ;
  mutable d1: int64# ;
  mutable d2: int64# ;
  mutable d3: int64# }

let[@opaque][@specialize never] add_fours_mutable_record (a : t1) (b: t1)
    (c : t2) : unit =
  c.d0 <- Int64_u.add a.d0 b.d0;
  c.d1 <- Int64_u.add a.d1 b.d1;
  c.d2 <- Int64_u.add a.d0 b.d0;
  c.d3 <- Int64_u.add a.d1 b.d1;
  ()

let print_t1 ppf (t1 : t1) =
  Format.fprintf ppf "{ d0 = %Ld ; d1 = %Ld }" (Int64_u.to_int64 t1.d0)
    (Int64_u.to_int64 t1.d1)

let print_t4 ppf (t2 : t2) =
  Format.fprintf ppf "{ d0 = %Ld ; d1 = %Ld; d2 = %Ld ; d3 = %Ld }"
    (Int64_u.to_int64 t2.d0)
    (Int64_u.to_int64 t2.d1)
    (Int64_u.to_int64 t2.d2)
    (Int64_u.to_int64 t2.d3)

let () =
  let a = { d0 = #8L; d1 = #96L } in
  let b = { d0 = #80L; d1 = #14L } in
  let c = { d0 = #8L; d1 = #96L } in
  let d = { d0 = #0L; d1 = #0L; d2 = #0L; d3 = #0L } in
  let res = { d0 = #0L; d1 = -#10L } in
  Format.printf "add_mutable_record %a\n" print_t1
    (add_mutable_record a b c);
  copy_mutable_record c res;
  Format.printf "copy_mutable_record %a\n" print_t1 res;
  add_fours_mutable_record a b d;
  Format.printf "add_fours_mutable_record %a\n" print_t4 d;
  ()
