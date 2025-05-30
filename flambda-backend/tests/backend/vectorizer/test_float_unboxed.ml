[@@@ocaml.warnerror "+a-40-41-42"]

module Float_u = struct
  type t = float#

  external to_float : t -> (float[@local_opt]) = "%box_float" [@@warning "-187"]

  external of_float : (float[@local_opt]) -> t = "%unbox_float" [@@warning "-187"]

  let[@inline always] add x y = of_float (Float.add (to_float x) (to_float y))
end

type t1 = { mutable d0: float#;
            mutable d1: float#;
            mutable d2: float#;
            mutable d3: float#
          }


let[@opaque][@specialize never] copy_mutable_record (a : t1) (b: t1) : unit =
  b.d0 <- a.d0;
  b.d1 <- a.d1;
  ()

(* Currently, can't vectorize because of the specific floatmem operation (looks like
   it is treated overly conservatively. *)
let[@opaque][@specialize never] add_mutable_record (a : t1) (b: t1) (c : t1) :
    t1 =
  c.d0 <- Float_u.add a.d0 b.d0;
  c.d1 <- Float_u.add a.d1 b.d1;
  c.d2 <- Float_u.add a.d2 b.d2;
  c.d3 <- Float_u.add a.d3 b.d3;
  c

(*
102:
(id:3) a:V/61 := R:I/0[%rax]
(id:4) b:V/62 := R:I/1[%rbx]
(id:5) c:V/63 := R:I/2[%rdi]
(id:6) F/64 := float64  mut[a:V/61]
(id:7) F/65 := F/64
(id:8) F/65 := F/65 +f float64[b:V/62]
(id:9) float64[c:V/63] := F/65 (assign)
(id:10) Psetufloatfield:I/66 := 1
(id:11) F/67 := float64  mut[a:V/61 + 8]
(id:12) F/68 := F/67
(id:13) F/68 := F/68 +f float64[b:V/62 + 8]
(id:14) float64[c:V/63 + 8] := F/68 (assign)
(id:15) Psetufloatfield:I/69 := 1
(id:16) F/70 := float64  mut[a:V/61 + 16]
(id:17) F/71 := F/70
(id:18) F/71 := F/71 +f float64[b:V/62 + 16]
(id:19) float64[c:V/63 + 16] := F/71 (assign)
(id:20) Psetufloatfield:I/72 := 1
(id:21) F/73 := float64  mut[a:V/61 + 24]
(id:22) F/74 := F/73
(id:23) F/74 := F/74 +f float64[b:V/62 + 24]
(id:24) float64[c:V/63 + 24] := F/74 (assign)
(id:25) Psetufloatfield:I/75 := 1
(id:26) R:I/0[%rax] := c:V/63
(id:27) Return R:I/0[%rax]

*)

let print_t1 ppf (t1 : t1) =
  Format.fprintf ppf "{ d0 = %f ; d1 = %f; d2 = %f ; d3 = %f }"
    (Float_u.to_float t1.d0)
    (Float_u.to_float t1.d1)
    (Float_u.to_float t1.d2)
    (Float_u.to_float t1.d3)

let () =
  let a = { d0 = #8.; d1 = #96.; d2 = #0.; d3 = -#0.5 } in
  let b = { d0 = #80.; d1 = #14.; d2 = #0.; d3 = -#0.5 } in
  let c = { d0 = #8.; d1 = #96.; d2 = #0.; d3 = -#0. } in
  let res = { d0 = #0.; d1 = -#10.; d2 = #1.; d3 = -#1. } in
  Format.printf "add_mutable_record %a\n" print_t1
    (add_mutable_record a b c);
  copy_mutable_record c res;
  Format.printf "copy_mutable_record %a\n" print_t1 res;
  ()
