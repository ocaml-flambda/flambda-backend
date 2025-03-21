open Stdlib

let failmsg = ref (fun () -> ())

let eq lv hv l h =
  if l <> lv then Printf.printf "%016Lx <> %016Lx\n" lv l;
  if h <> hv then Printf.printf "%016Lx <> %016Lx\n" hv h;
  if l <> lv || h <> hv then !failmsg ()

let eqi lv hv l h =
  if l <> lv then Printf.printf "%016x <> %016x\n" lv l;
  if h <> hv then Printf.printf "%016x <> %016x\n" hv h;
  if l <> lv || h <> hv then !failmsg ()

module SSE42_String = struct
  (* These also work with int16x8s, given the 16-bit char encoding immediate
     bit *)

  include Special_builtins.SSE42_String

  let vec_of_string str =
    assert (String.length str = 16);
    let i0 = ref 0L in
    for i = 7 downto 0 do
      i0 := Int64.shift_left !i0 8;
      i0 := Int64.logor !i0 (Char.code str.[i] |> Int64.of_int)
    done;
    let i1 = ref 0L in
    for i = 15 downto 8 do
      i1 := Int64.shift_left !i1 8;
      i1 := Int64.logor !i1 (Char.code str.[i] |> Int64.of_int)
    done;
    int8x16_of_int64s !i0 !i1

  let sbyte = 0b0000_0010

  let cmp_eq_each = 0b0000_1000

  let negate = 0b0001_0000

  let msk_negate = 0b0011_0000

  let lst_sig = 0b0000_0000

  let mst_sig = 0b0100_0000

  let bit_msk = 0b0000_0000

  let byte_mask = 0b0100_0000

  let () =
    let v0 = vec_of_string "abcdefghijklmnop" in
    let v1 = vec_of_string "abcdefgh\000\000\000\000\000\000\000\000" in
    let s0 = cmpistra (sbyte lor cmp_eq_each lor negate) v0 v0 in
    let s1 = cmpistrc (sbyte lor cmp_eq_each) v0 v1 in
    let s2 = cmpistri (sbyte lor cmp_eq_each lor lst_sig) v0 v1 in
    let s3 = cmpistri (sbyte lor cmp_eq_each lor mst_sig) v0 v1 in
    let s4 = cmpistro (sbyte lor cmp_eq_each) v0 v1 in
    let s5 = cmpistrs (sbyte lor cmp_eq_each) v0 v1 in
    let s6 = cmpistrs (sbyte lor cmp_eq_each) v1 v0 in
    let s7 = cmpistrz (sbyte lor cmp_eq_each) v0 v1 in
    let s8 = cmpistrz (sbyte lor cmp_eq_each) v1 v0 in
    eqi s0 s1 1 1;
    eqi s2 s3 0 7;
    eqi s4 s5 1 0;
    eqi s6 s7 1 1;
    eqi s8 0 0 0;
    let m = cmpistrm (sbyte lor cmp_eq_each lor bit_msk) v0 v1 in
    eq (int8x16_low_int64 m) (int8x16_high_int64 m) 0xffL 0L;
    let m = cmpistrm (sbyte lor cmp_eq_each lor bit_msk) v0 v0 in
    eq (int8x16_low_int64 m) (int8x16_high_int64 m) 0xffffL 0L;
    let m = cmpistrm (sbyte lor cmp_eq_each lor byte_mask) v0 v1 in
    eq (int8x16_low_int64 m) (int8x16_high_int64 m) 0xffffffffffffffffL 0L;
    let m = cmpistrm (sbyte lor cmp_eq_each lor byte_mask) v0 v0 in
    eq (int8x16_low_int64 m) (int8x16_high_int64 m) 0xffffffffffffffffL
      0xffffffffffffffffL

  let () =
    let v0 = vec_of_string "abcdefghijklmnop" in
    let v1 = vec_of_string "abcdefgh\000\000\000\000\000\000\000\000" in
    let s0 = cmpestra (sbyte lor cmp_eq_each lor negate) v0 v0 16 16 in
    let s1 = cmpestrc (sbyte lor cmp_eq_each) v0 v1 16 8 in
    let s2 = cmpestri (sbyte lor cmp_eq_each lor lst_sig) v0 v1 16 8 in
    let s3 = cmpestri (sbyte lor cmp_eq_each lor mst_sig) v0 v1 16 8 in
    let s4 = cmpestro (sbyte lor cmp_eq_each) v0 v1 16 8 in
    let s5 = cmpestrs (sbyte lor cmp_eq_each) v0 v1 16 8 in
    let s6 = cmpestrs (sbyte lor cmp_eq_each) v1 v0 8 16 in
    let s7 = cmpestrz (sbyte lor cmp_eq_each) v0 v1 16 8 in
    let s8 = cmpestrz (sbyte lor cmp_eq_each) v1 v0 8 16 in
    eqi s0 s1 1 1;
    eqi s2 s3 0 7;
    eqi s4 s5 1 0;
    eqi s6 s7 1 1;
    eqi s8 0 0 0;
    let m = cmpestrm (sbyte lor cmp_eq_each lor bit_msk) v0 v1 16 8 in
    eq (int8x16_low_int64 m) (int8x16_high_int64 m) 0xffL 0L;
    let m = cmpestrm (sbyte lor cmp_eq_each lor bit_msk) v0 v0 16 16 in
    eq (int8x16_low_int64 m) (int8x16_high_int64 m) 0xffffL 0L;
    let m = cmpestrm (sbyte lor cmp_eq_each lor byte_mask) v0 v1 16 8 in
    eq (int8x16_low_int64 m) (int8x16_high_int64 m) 0xffffffffffffffffL 0L;
    let m = cmpestrm (sbyte lor cmp_eq_each lor byte_mask) v0 v0 16 16 in
    eq (int8x16_low_int64 m) (int8x16_high_int64 m) 0xffffffffffffffffL
      0xffffffffffffffffL
end
