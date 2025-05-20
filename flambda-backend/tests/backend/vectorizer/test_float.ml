[@@@ocaml.warnerror "+a-40-41-42"]

type t1 =
  { mutable d0 : float;
    mutable d1 : float
  }

let[@opaque] [@specialize never] add_mutable_record (a : t1) (b : t1) (c : t1) :
    t1 =
  c.d0 <- Float.add a.d0 b.d0;
  c.d1 <- Float.add a.d1 b.d1;
  c

let[@opaque] [@specialize never] copy_mutable_record (a : t1) (b : t1) : t1 =
  b.d0 <- a.d0;
  b.d1 <- a.d1;
  b

let[@opaque] [@specialize never] add_mutable_record_fresh (a : t1) (b : t1) : t1
    =
  { d0 = Float.add a.d0 b.d0; d1 = Float.add a.d1 b.d1 }

let[@opaque] [@specialize never] copy_mutable_record_fresh (a : t1) : t1 =
  { d0 = a.d0; d1 = a.d1 }

type t4 =
  { mutable d0 : float;
    mutable d1 : float;
    mutable d2 : float;
    mutable d3 : float
  }

let[@opaque] [@specialize never] add_mutable_record_t4 (a : t1) (b : t1)
    (c : t4) : t4 =
  c.d0 <- Float.add a.d0 b.d0;
  c.d1 <- Float.add a.d1 b.d1;
  c.d2 <- Float.add a.d0 b.d0;
  c.d3 <- Float.add a.d1 b.d1;
  c

let[@opaque] [@specialize never] copy_mutable_record_t4 (a : t1) (b : t1) : t4 =
  { d0 = a.d0; d1 = a.d1; d2 = b.d0; d3 = b.d1 }

let[@opaque] [@specialize never] dup_mutable_record_t4 (a : t1) : t4 =
  { d0 = a.d0; d1 = a.d1; d2 = a.d0; d3 = a.d1 }

let print_t1 ppf (t1 : t1) =
  Format.fprintf ppf "{ d0 = %f ; d1 = %f }" t1.d0 t1.d1

let print_t4 ppf (t4 : t4) =
  Format.fprintf ppf "{ d0 = %f ; d1 = %f; d2 = %f ; d3 = %f }" t4.d0 t4.d1
    t4.d2 t4.d3

let () =
  let a = { d0 = 8.; d1 = 96. } in
  let b = { d0 = 80.; d1 = 14. } in
  let c = { d0 = 10.; d1 = -10. } in
  let t4 = { d0 = 10.; d1 = -10.; d2 = 199.; d3 = 18. } in
  let res = { d0 = 0.; d1 = -0. } in
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
