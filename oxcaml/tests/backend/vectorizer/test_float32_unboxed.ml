[@@@ocaml.warnerror "+a-40-41-42"]

module Float32 = struct
  type t = float32

  external add : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
    = "%addfloat32"

  external format : string -> t -> string = "caml_format_float32"

  let to_string f = Stdlib.valid_float_lexem (format "%.9g" f)

  module Bytes = struct
    external get : bytes -> pos:int -> float32 = "%caml_bytes_getf32"
    external unsafe_get : bytes -> pos:int -> float32 = "%caml_bytes_getf32u"
    external set : bytes -> pos:int -> float32 -> unit = "%caml_bytes_setf32"

    external unsafe_set : bytes -> pos:int -> float32 -> unit
      = "%caml_bytes_setf32u"
  end
end

module Float32_u = struct
  type t = float32#

  external to_float32 : t -> (float32[@local_opt]) = "%box_float32" [@@warning "-187"]

  external of_float32 : (float32[@local_opt]) -> t = "%unbox_float32" [@@warning "-187"]

  let[@inline always] add x y = of_float32 (Float32.add (to_float32 x) (to_float32 y))

  module Bytes = struct
    let get bytes ~pos = of_float32 (Float32.Bytes.get bytes ~pos)
    let unsafe_get bytes ~pos = of_float32 (Float32.Bytes.unsafe_get bytes ~pos)
    let set bytes ~pos x = Float32.Bytes.set bytes ~pos (to_float32 x)
    let unsafe_set bytes ~pos x = Float32.Bytes.unsafe_set bytes ~pos (to_float32 x)
  end
end

type t1 = { mutable d0 : float32# ;
            mutable d1: float32#; mutable d2: float32#; mutable d3: float32#  }

(* Not vectorized because float32 fields are not adjacent in a record, they are padded
to 64-bits. *)
let[@inline never] [@local never][@specialize never] copy_mutable_record (a : t1) (b: t1) : unit =
  b.d0 <- a.d0;
  b.d1 <- a.d1;
  b.d2 <- a.d2;
  b.d3 <- a.d3;
  ()

(* Not vectorized because float32 fields are not adjacent in a record, they are padded
to 64-bits. *)
let[@inline never] [@local never][@specialize never] add_mutable_record (a : t1) (b: t1) (c : t1) : t1 =
  c.d0 <- Float32_u.add a.d0 b.d0;
  c.d1 <- Float32_u.add a.d1 b.d1;
  c.d2 <- Float32_u.add a.d2 b.d2;
  c.d3 <- Float32_u.add a.d3 b.d3;
  c

(* [Float32_u.Bytes] contain packed float32_u, can vectorize. *)
let[@inline never] [@local never] [@specialize never] copy_bytes a b =
  let pos = 0 in
  let x = Float32_u.Bytes.unsafe_get a ~pos in
  Float32_u.Bytes.unsafe_set b ~pos x;
  let pos = pos + 4 in
  let x = Float32_u.Bytes.unsafe_get a ~pos in
  Float32_u.Bytes.unsafe_set b ~pos x;
  let pos = pos + 4 in
  let x = Float32_u.Bytes.unsafe_get a ~pos in
  Float32_u.Bytes.unsafe_set b ~pos x;
  let pos = pos + 4 in
  let x = Float32_u.Bytes.unsafe_get a ~pos in
  Float32_u.Bytes.unsafe_set b ~pos x;
  ()

let[@inline never] [@local never] [@specialize never] init_bytes b x =
  let pos = 0 in
  Float32_u.Bytes.unsafe_set b ~pos x;
  let pos = pos + 4 in
  Float32_u.Bytes.unsafe_set b ~pos x;
  let pos = pos + 4 in
  Float32_u.Bytes.unsafe_set b ~pos x;
  let pos = pos + 4 in
  Float32_u.Bytes.unsafe_set b ~pos x;
  ()

let[@inline always] copy_float32_unboxed_pos a b ~pos =
  let x = Float32_u.Bytes.unsafe_get a ~pos in
  Float32_u.Bytes.unsafe_set b ~pos x;
  ()

(* Currently can't vectorize because [pos] untagging is repeated and the current
   heuristic for detecting relations between pointers is not strong enough to
   handle this case. *)
let[@inline never] [@local never] [@specialize never] copy_bytes_pos a b pos =
  copy_float32_unboxed_pos a b ~pos;
  copy_float32_unboxed_pos a b ~pos:(pos+1*4);
  copy_float32_unboxed_pos a b ~pos:(pos+2*4);
  copy_float32_unboxed_pos a b ~pos:(pos+3*4);
  ()

(* 128:
 * (id:3) a:V/61 := R:I/0[%rax]
 * (id:4) b:V/62 := R:I/1[%rbx]
 * (id:5) pos:I/63 := R:I/2[%rdi]
 * (id:6) prim:I/64 := pos:I/63
 * (id:7) prim:I/64 := prim:I/64 >>s 1
 * (id:8) S/65 := float32  mut[a:V/61 + prim:I/64]
 * (id:9) float32[b:V/62 + prim:I/64] := S/65 (assign)
 * (id:10) Pbytes_set_f32:I/66 := 1
 * (id:11) I/67 := pos:I/63
 * (id:12) I/67 := I/67 + 8
 * (id:13) prim:I/68 := I/67
 * (id:14) prim:I/68 := prim:I/68 >>s 1
 * (id:15) S/69 := float32  mut[a:V/61 + prim:I/68]
 * (id:16) float32[b:V/62 + prim:I/68] := S/69 (assign)
 * (id:17) Pbytes_set_f32:I/70 := 1
 * (id:18) I/71 := pos:I/63
 * (id:19) I/71 := I/71 + 16
 * (id:20) prim:I/72 := I/71
 * (id:21) prim:I/72 := prim:I/72 >>s 1
 * (id:22) S/73 := float32  mut[a:V/61 + prim:I/72]
 * (id:23) float32[b:V/62 + prim:I/72] := S/73 (assign)
 * (id:24) Pbytes_set_f32:I/74 := 1
 * (id:25) I/75 := pos:I/63
 * (id:26) I/75 := I/75 + 24
 * (id:27) prim:I/76 := I/75
 * (id:28) prim:I/76 := prim:I/76 >>s 1
 * (id:29) S/77 := float32  mut[a:V/61 + prim:I/76]
 * (id:30) float32[b:V/62 + prim:I/76] := S/77 (assign)
 * (id:31) Pbytes_set_f32:I/78 := 1
 * (id:32) I/79 := 1
 * (id:33) R:I/0[%rax] := I/79
 * (id:34) Return R:I/0[%rax] *)

(* Currently, can't vectorize because the index is untagged before every memory access,
   instead of operating on untagged indexes throughout. *)
let[@inline never] [@local never] [@specialize never] copy_bytes_pos_v2 a b pos =
  let i0 = pos in
  copy_float32_unboxed_pos a b ~pos:i0;
  let i1 = i0 + 4  in
  copy_float32_unboxed_pos a b ~pos:i1;
  let i2 = i1 + 4 in
  copy_float32_unboxed_pos a b ~pos:i2;
  let i3 = i2 + 4 in
  copy_float32_unboxed_pos a b ~pos:i3;
  ()

(* 177:
 * (id:3) a:V/61 := R:I/0[%rax]
 * (id:4) b:V/62 := R:I/1[%rbx]
 * (id:5) pos:I/63 := R:I/2[%rdi]
 * (id:6) prim:I/64 := pos:I/63
 * (id:7) prim:I/64 := prim:I/64 >>s 1
 * (id:8) S/65 := float32  mut[a:V/61 + prim:I/64]
 * (id:9) float32[b:V/62 + prim:I/64] := S/65 (assign)
 * (id:10) Pbytes_set_f32:I/66 := 1
 * (id:11) i1:I/67 := pos:I/63
 * (id:12) i1:I/67 := i1:I/67 + 8
 * (id:13) prim:I/68 := i1:I/67
 * (id:14) prim:I/68 := prim:I/68 >>s 1
 * (id:15) S/69 := float32  mut[a:V/61 + prim:I/68]
 * (id:16) float32[b:V/62 + prim:I/68] := S/69 (assign)
 * (id:17) Pbytes_set_f32:I/70 := 1
 * (id:18) i2:I/71 := i1:I/67
 * (id:19) i2:I/71 := i2:I/71 + 8
 * (id:20) prim:I/72 := i2:I/71
 * (id:21) prim:I/72 := prim:I/72 >>s 1
 * (id:22) S/73 := float32  mut[a:V/61 + prim:I/72]
 * (id:23) float32[b:V/62 + prim:I/72] := S/73 (assign)
 * (id:24) Pbytes_set_f32:I/74 := 1
 * (id:25) I/75 := i2:I/71
 * (id:26) I/75 := I/75 + 8
 * (id:27) prim:I/76 := I/75
 * (id:28) prim:I/76 := prim:I/76 >>s 1
 * (id:29) S/77 := float32  mut[a:V/61 + prim:I/76]
 * (id:30) float32[b:V/62 + prim:I/76] := S/77 (assign)
 * (id:31) Pbytes_set_f32:I/78 := 1
 * (id:32) I/79 := 1
 * (id:33) R:I/0[%rax] := I/79
 * (id:34) Return R:I/0[%rax] *)


let print_t1 ppf (t1 : t1) =
  (* CR gyorsh: how to print Float32? *)
  let to_string f = (Float32_u.to_float32 f |> Float32.to_string) in
  Format.fprintf ppf "{ d0 = %s ; d1 = %s; d2 = %s ; d3 = %s }"
    (to_string t1.d0)
    (to_string t1.d1)
    (to_string t1.d2)
    (to_string t1.d3)

let create_s length =
  String.init length (fun i -> i * 7 mod 256 |> char_of_int)
;;

let create_b length = create_s length |> Bytes.of_string

let print_b ~len ppf b =
  for i = 0 to len-1 do
    Format.fprintf ppf "%s "
      (Float32_u.to_float32 (Float32_u.Bytes.get b ~pos:(i*4)) |> Float32.to_string)
  done

let () =
  let a = { d0 = #8.s; d1 = #96.s; d2 = #0.s; d3 = -#0.5s } in
  let b = { d0 = #80.s; d1 = #14.s; d2 = #0.s; d3 = -#0.5s } in
  let c = { d0 = #8.s; d1 = #96.s; d2 = #0.s; d3 = -#0.s } in
  let res = { d0 = #0.s; d1 = -#10.s; d2 = #1.s; d3 = -#1.s } in
  Format.printf "add_unboxed_pairs_mutable_record %a\n" print_t1
    (add_mutable_record a b c);
  copy_mutable_record c res;
  Format.printf "copy_unboxed_pairs_mutable_record %a\n" print_t1 res;
  let b1 = create_b 16 in
  let b2 = create_b 16 in
  init_bytes b1 #10.s;
  init_bytes b2 #0.s;
  copy_bytes b1 b2;
  Format.printf "copy_bytes %a\n" (print_b ~len:4) b2;
  copy_bytes_pos b2 b1 (Sys.opaque_identity 0);
  Format.printf "copy_bytes_pos %a\n" (print_b ~len:4) b2;
  copy_bytes_pos_v2 b1 b2 (Sys.opaque_identity 0);
  Format.printf "copy_bytes_pos_v2 %a\n" (print_b ~len:4) b2;
  ()
