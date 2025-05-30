[@@@ocaml.warnerror "+a-40-41-42"]

(* Record with immutable int fields *)

type t2 =
  { d0 : int;
    d1 : int
  }

let[@opaque] add_pairs_immutable_record (a : t2) (b : t2) : t2 =
  { d0 = a.d0 + b.d0; d1 = a.d1 + b.d1 }

type t4 =
  { d0 : int;
    d1 : int;
    d2 : int;
    d3 : int
  }

let[@opaque] add_fours_immutable_record (a : t4) (b : t4) : t4 =
  { d0 = a.d0 + b.d0; d1 = a.d1 + b.d1; d2 = a.d2 + b.d2; d3 = a.d3 + b.d3 }

(* Tuples *)

let[@opaque] add_int_tuples ((a0, a1) : int * int) ((b0, b1) : int * int) =
  a0 + b0, a1 + b1

let[@opaque] add_t2_to_t4 (a : t2) (b : t2) : t4 =
  { d0 = a.d0 + b.d0; d1 = a.d1 + b.d1; d2 = a.d0 + b.d0; d3 = a.d1 + b.d1 }

(* CR gyorsh: can't vectorize, requires a shuffle because the order of the first
   vector access is not the same as the second. *)
let[@opaque] add_t2_to_t4_reordered (a : t2) (b : t2) : t4 =
  { d0 = a.d0 + b.d0; d1 = a.d1 + b.d1; d3 = a.d0 + b.d0; d2 = a.d1 + b.d1 }

let[@opaque] copy_t2_to_t4_immutable_record (a : t2) : t4 =
  { d0 = a.d0; d1 = a.d1; d2 = a.d0; d3 = a.d1 }

(* CR gyorsh: can't vectorize same load. *)
let[@opaque] same_value_in_both_fields_immutable_record (a : t2) : t2 =
  let x = a.d0 in
  let y = a.d1 in
  let z = x + y in
  { d0 = z; d1 = z }

type s2 =
  { mutable f0 : int;
    mutable f1 : int
  }

let[@opaque] copy_pairs_mutable_record (a : s2) (b : s2) : unit =
  b.f0 <- a.f0;
  b.f1 <- a.f1;
  ()

(* CR gyorsh: dependency outside computation should only look at reg not mem *)
let[@opaque] copy_pairs_mutable_record_return (a : s2) (b : s2) : s2 =
  b.f0 <- a.f0;
  b.f1 <- a.f1;
  b

type s4 =
  { mutable f0 : int;
    mutable f1 : int;
    mutable f2 : int;
    mutable f3 : int
  }

let[@opaque] copy_fours_mutable_record (a : s4) (b : s4) : unit =
  a.f0 <- b.f0;
  a.f1 <- b.f1;
  a.f2 <- b.f2;
  a.f3 <- b.f3;
  ()

let[@opaque] add_fours_mutable_record (a : s4) (b : s4) : unit =
  a.f0 <- b.f0 + a.f0;
  a.f1 <- b.f1 + a.f1;
  a.f2 <- b.f2 + a.f2;
  a.f3 <- b.f3 + a.f3;
  ()

(* CR-someday gyorsh: use expect test *)

let print_t2 ppf (t2 : t2) =
  Format.fprintf ppf "{ d0 = %d ; d1 = %d }" t2.d0 t2.d1

let print_t4 ppf (t4 : t4) =
  Format.fprintf ppf "{ d0 = %d ; d1 = %d; d2 = %d ; d3 = %d }" t4.d0 t4.d1
    t4.d2 t4.d3

let print_s2 ppf (s2 : s2) =
  Format.fprintf ppf "{ f0 = %d ; f1 = %d }" s2.f0 s2.f1

let print_s4 ppf (s4 : s4) =
  Format.fprintf ppf "{ f0 = %d ; f1 = %d; f2 = %d ; f3 = %d }" s4.f0 s4.f1
    s4.f2 s4.f3

let print_pair ppf (x, y) = Format.fprintf ppf "(%d, %d)" x y

let () =
  Format.printf "add_pairs_immutable_record %a\n" print_t2
    (add_pairs_immutable_record { d0 = 8; d1 = 96 } { d0 = 80; d1 = 14 });
  Format.printf "add_fours_immutable_record %a\n" print_t4
    (add_fours_immutable_record
       { d0 = 9; d1 = 12; d2 = 16; d3 = 98 }
       { d0 = 25; d1 = 85; d2 = 72; d3 = 48 });
  Format.printf "add_int_tuples %a\n" print_pair
    (add_int_tuples (48, 31) (4, 71));
  Format.printf "add_t2_to_t4 %a\n" print_t4
    (add_t2_to_t4 { d0 = 8; d1 = 96 } { d0 = 80; d1 = 14 });
  Format.printf "add_t2_to_t4_reordered %a\n" print_t4
    (add_t2_to_t4_reordered { d0 = 8; d1 = 96 } { d0 = 80; d1 = 14 });
  Format.printf "copy_t2_to_t4_immutable_record %a\n" print_t4
    (copy_t2_to_t4_immutable_record { d0 = 8; d1 = 96 });
  Format.printf "same_value_in_both_fields_immutable_record %a\n" print_t2
    (same_value_in_both_fields_immutable_record { d0 = 8; d1 = 96 });
  Format.printf "copy_pairs_mutable_record %a\n" print_s2
    (let s2 = { f0 = 42; f1 = 27 } in
     let s2' = { f0 = 1; f1 = -1 } in
     copy_pairs_mutable_record s2 s2';
     s2);
  Format.printf "copy_pairs_mutable_record_return %a\n" print_s2
    (copy_pairs_mutable_record_return { f0 = 42; f1 = 27 } { f0 = 0; f1 = -100 });
  Format.printf "copy_fours_mutable_record %a\n" print_s4
    (let s4 = { f0 = 42; f1 = 27; f2 = 423; f3 = 239 } in
     let s4' = { f0 = 1; f1 = 2; f2 = 3; f3 = -4 } in
     copy_fours_mutable_record s4 s4';
     s4);
  Format.printf "add_fours_mutable_record %a\n" print_s4
    (let s4 = { f0 = 42; f1 = 27; f2 = 423; f3 = 239 } in
     let s4' = { f0 = 1; f1 = 2; f2 = 3; f3 = -4 } in
     add_fours_mutable_record s4 s4';
     s4)
