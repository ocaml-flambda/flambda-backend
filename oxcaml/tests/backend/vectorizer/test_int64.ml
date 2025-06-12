[@@@ocaml.warnerror "+a-40-41-42"]

type t1 =
  { mutable d0 : int64;
    mutable d1 : int64
  }

(* Can't vectorize because int64 are boxed. *)
let[@opaque] [@specialize never] add_mutable_record (a : t1) (b : t1) (c : t1) :
    t1 =
  c.d0 <- Int64.add a.d0 b.d0;
  c.d1 <- Int64.add a.d1 b.d1;
  c

(* Can't vectorize because memory write requires [caml_modify]. *)
let[@opaque] [@specialize never] copy_mutable_record (a : t1) (b : t1) : t1 =
  b.d0 <- a.d0;
  b.d1 <- a.d1;
  b

(* Can't vectorize because int64 are boxed *)
let[@opaque] [@specialize never] add_mutable_record_fresh (a : t1) (b : t1) : t1
    =
  { d0 = Int64.add a.d0 b.d0; d1 = Int64.add a.d1 b.d1 }

let[@opaque] [@specialize never] copy_mutable_record_fresh (a : t1) : t1 =
  { d0 = a.d0; d1 = a.d1 }

type t4 =
  { mutable d0 : int64;
    mutable d1 : int64;
    mutable d2 : int64;
    mutable d3 : int64
  }

(* Can't vectorize because int64 are boxed. *)
let[@opaque] [@specialize never] add_mutable_record_t4 (a : t1) (b : t1)
    (c : t4) : t4 =
  c.d0 <- Int64.add a.d0 b.d0;
  c.d1 <- Int64.add a.d1 b.d1;
  c.d2 <- Int64.add a.d0 b.d0;
  c.d3 <- Int64.add a.d1 b.d1;
  c

let[@opaque] [@specialize never] copy_mutable_record_t4 (a : t1) (b : t1) : t4 =
  { d0 = a.d0; d1 = a.d1; d2 = b.d0; d3 = b.d1 }

let[@opaque] [@specialize never] dup_mutable_record_t4 (a : t1) : t4 =
  { d0 = a.d0; d1 = a.d1; d2 = a.d0; d3 = a.d1 }

let print_t1 ppf (t1 : t1) =
  Format.fprintf ppf "{ d0 = %Ld ; d1 = %Ld }" t1.d0 t1.d1

let print_t4 ppf (t4 : t4) =
  Format.fprintf ppf "{ d0 = %Ld ; d1 = %Ld; d2 = %Ld ; d3 = %Ld }" t4.d0 t4.d1
    t4.d2 t4.d3

let () =
  let a = { d0 = 8L; d1 = 96L } in
  let b = { d0 = 80L; d1 = 14L } in
  let c = { d0 = 10L; d1 = -10L } in
  let t4 = { d0 = 10L; d1 = -10L; d2 = 199L; d3 = 18L } in
  let res = { d0 = 0L; d1 = -0L } in
  Format.printf "add_mutable_record %a\n" print_t1 (add_mutable_record a b c);
  Format.printf "copy_mutable_record %a\n" print_t1 (copy_mutable_record c res);
  Format.printf "add_mutable_record_fresh %a\n" print_t1
    (add_mutable_record_fresh a b);
  Format.printf "copy_mutable_record_fresh %a\n" print_t1
    (copy_mutable_record_fresh c);
  Format.printf "add_mutable_record_t4 %a\n" print_t4
    (add_mutable_record_t4 a b t4);
  Format.printf "copy_mutable_record_t4 %a\n" print_t4
    (copy_mutable_record_t4 a b);
  Format.printf "dup_mutable_record_t4 %a\n" print_t4 (dup_mutable_record_t4 a);
  ()
