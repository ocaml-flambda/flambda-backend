let size = Bigarray.Array1.dim

type t =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type cursor = {
  buffer: t;
  mutable position: int;
}

let map_binary path =
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  let len = Unix.lseek fd 0 Unix.SEEK_END in
  let t =
    Bigarray.array1_of_genarray
      (Unix.map_file fd Bigarray.int8_unsigned
         Bigarray.c_layout false [|len|]) in
  Unix.close fd;
  t

let map_binary_write path size =
  let fd = Unix.openfile path [Unix.O_RDWR] 0 in
  let t =
    Bigarray.array1_of_genarray
      (Unix.map_file fd Bigarray.int8_unsigned
         Bigarray.c_layout true [|size|]) in
  Unix.close fd;
  t

exception Invalid_format of string
let invalid_format msg = raise (Invalid_format msg)

let assert_format b msg =
  if not b then
    invalid_format msg

let cursor ?(at=0) buffer =
  { buffer; position = at }

let seek t position =
  t.position <- position

let ensure t count msg =
  (* Ensure position does not overflow before checking for buffer overflow. *)
  let new_pos = t.position + count in
  if (new_pos < 0) || (size t.buffer < new_pos) then
    invalid_format msg

let advance t count = t.position <- t.position + count

let at_end t = size t.buffer = t.position

type s8  = int
type u8  = int
type u16 = int
type s32 = int
type u32 = int
type u64 = int64
type s128 = int
type u128 = int

(* All endian and bit-width dependent code starts here *)
module Read = struct
  let u8 t : u8 =
    let result = t.buffer.{t.position} in
    advance t 1;
    result

  let s8 t : s8 =
    let result = t.buffer.{t.position} in
    advance t 1;
    if result > 0x7F
    then result lor ((-1) lsl 8)
    else result

  let u16 t : u16 =
    let result = t.buffer.{t.position} lor t.buffer.{t.position + 1} lsl 8 in
    advance t 2;
    result

  let u32 t : u32 =
    let result = t.buffer.{t.position}
                 lor t.buffer.{t.position + 1} lsl 8
                 lor t.buffer.{t.position + 2} lsl 16
                 lor t.buffer.{t.position + 3} lsl 24
    in
    advance t 4;
    result

  let u32be = u32

  let u64 t : u64 =
    let result = ref 0L in
    for i = 0 to 7 do
      let open Int64 in
      let n = of_int t.buffer.{t.position + i} in
      result := logor !result (shift_left n (i * 8))
    done;
    advance t 8;
    !result

  let uleb128 t : u128 =
    let rec aux t shift acc =
      let x = u8 t in
      let acc = acc lor ((x land 0x7f) lsl shift) in
      if x land 0x80 = 0 then
        acc
      else
        aux t (shift + 7) acc
    in
    aux t 0 0

  let sleb128 t : s128 =
    let rec aux t shift acc =
      let x = u8 t in
      let acc = acc lor ((x land 0x7f) lsl shift) in
      if x land 0x80 = 0 then
        if x land 0x40 = 0
        then acc
        else acc lor -(1 lsl (shift + 7))
      else
        aux t (shift + 7) acc
    in
    aux t 0 0

  let fixed_string t length =
    let {buffer; position} = t in
    let result = Bytes.create length in
    for i = 0 to length - 1 do
      Bytes.set result i (Char.unsafe_chr buffer.{position + i})
    done;
    advance t length;
    Bytes.unsafe_to_string result

  let rec scan_0 (b : t) ofs l i =
    if i >= l then
      None
    else if b.{ofs + i} = 0 then
      Some i
    else
      scan_0 b ofs l (i + 1)

  let zero_string t ?maxlen () =
    let maxlen = match maxlen with
      | None -> size t.buffer - t.position
      | Some maxlen -> maxlen
    in
    match scan_0 t.buffer t.position maxlen 0 with
    | None -> None
    | Some length ->
      let result = fixed_string t length in
      advance t 1;
      Some result

  let buffer t length =
    let result = Bigarray.Array1.sub t.buffer t.position length in
    advance t length;
    result
end

module Write = struct
  let u8 t (w : u8) =
    t.buffer.{t.position} <- w;
    advance t 1

  let s8 t (w : s8) =
    let w =
      if w > 0x7F then
        w lor ((-1) lsl 8)
      else w in
    t.buffer.{t.position} <- w;
    advance t 1

  let u16 t (w : u16) =
    t.buffer.{t.position} <- w land 0xFF;
    t.buffer.{t.position + 1} <- w lsr 8;
    advance t 2

  let u32 t (w : u32) =
    t.buffer.{t.position + 0} <- (w lsr 0) land 0xFF;
    t.buffer.{t.position + 1} <- (w lsr 8) land 0xFF;
    t.buffer.{t.position + 2} <- (w lsr 16) land 0xFF;
    t.buffer.{t.position + 3} <- (w lsr 24) land 0xFF;
    advance t 4

  let u32be = u32

  let u64 t (w : u64) =
    let open Int64 in
    t.buffer.{t.position + 0} <- to_int (shift_right w 0) land 0xFF;
    t.buffer.{t.position + 1} <- to_int (shift_right w 8) land 0xFF;
    t.buffer.{t.position + 2} <- to_int (shift_right w 16) land 0xFF;
    t.buffer.{t.position + 3} <- to_int (shift_right w 24) land 0xFF;
    t.buffer.{t.position + 4} <- to_int (shift_right w 32) land 0xFF;
    t.buffer.{t.position + 5} <- to_int (shift_right w 40) land 0xFF;
    t.buffer.{t.position + 6} <- to_int (shift_right w 48) land 0xFF;
    t.buffer.{t.position + 7} <- to_int (shift_right w 56) land 0xFF;
    advance t 8

  let uleb128 t (w : u128) =
    let rec aux t acc =
      let x = acc land 0x7F in
      let acc = acc lsr 7 in
      if acc = 0 then
        u8 t x
      else
        (u8 t (x lor 0x80);
        aux t acc)
    in
    aux t w

  let sleb128 t (w : s128) =
    let rec aux t acc =
      let x = acc land 0x7F in
      let acc = acc asr 7 in
      if (acc = 0 && (x land 0x40) = 0) || (acc = -1 && (x land 0x40) <> 0) then
        u8 t x
      else
        (u8 t (x lor 0x80);
        aux t acc)
    in
    aux t w

  let fixed_string t length s =
    let {buffer; position} = t in
    for i = 0 to length - 1 do
      buffer.{position + i} <- Char.code s.[i]
    done;
    advance t length

  let zero_string t ?maxlen s =
    let maxlen = match maxlen with
      | None -> size t.buffer - t.position
      | Some maxlen -> maxlen
    in
    let i = ref 0 in
    while !i < maxlen && Char.code s.[!i] <> 0 do
      t.buffer.{t.position + !i} <- Char.code s.[!i];
      i := !i + 1
    done;
    t.buffer.{t.position + !i} <- 0;
    advance t !i

  let buffer t length =
    let result = Bigarray.Array1.sub t.buffer t.position length in
    advance t length;
    result
end

let sub t length =
  cursor (Read.buffer t length)
