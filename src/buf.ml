module Shared = struct
  type t = {
    buf : Bytes.t;
    mutable pos : int;
    pos_end : int;
  }

  let of_bytes buf =
    { buf; pos = 0; pos_end = Bytes.length buf }

  let of_bytes_sub buf ~pos ~pos_end =
    { buf; pos; pos_end }

  let remaining b =
    b.pos_end - b.pos

  external bswap_16 : int -> int = "%bswap16"
  external bswap_32 : int32 -> int32 = "%bswap_int32"
  external bswap_64 : int64 -> int64 = "%bswap_int64"
end

module Write = struct
  include Shared

  let rec write_fully fd buf pos pos_end =
    if pos = pos_end then () else
      let written = Unix.write fd buf pos (pos_end - pos) in
      write_fully fd buf (pos + written) pos_end

  let write_fd fd b =
    write_fully fd b.buf 0 b.pos

  let put_raw_8 b i v = Bytes.unsafe_set b i (Char.unsafe_chr v)
  external put_raw_16 : Bytes.t -> int -> int -> unit = "%caml_bytes_set16u"
  external put_raw_32 : Bytes.t -> int -> int32 -> unit = "%caml_bytes_set32u"
  external put_raw_64 : Bytes.t -> int -> int64 -> unit = "%caml_bytes_set64u"

  exception Overflow of int
  let[@inline never] overflow b = Overflow b.pos

  let[@inline always] put_8 b v =
    let pos = b.pos in
    let pos' = b.pos + 1 in
    if pos' > b.pos_end then raise (overflow b) else
      (put_raw_8 b.buf pos v;
       b.pos <- pos')

  let[@inline always] put_16 b v =
    let pos = b.pos in
    let pos' = b.pos + 2 in
    if pos' > b.pos_end then raise (overflow b) else
      (put_raw_16 b.buf pos (if Sys.big_endian then bswap_16 v else v);
       b.pos <- pos')

  let[@inline always] put_32 b v =
    let pos = b.pos in
    let pos' = b.pos + 4 in
    if pos' > b.pos_end then raise (overflow b) else
      (put_raw_32 b.buf pos (if Sys.big_endian then bswap_32 v else v);
       b.pos <- pos')

  let[@inline always] put_64 b v =
    let pos = b.pos in
    let pos' = b.pos + 8 in
    if pos' > b.pos_end then raise (overflow b) else
      (put_raw_64 b.buf pos (if Sys.big_endian then bswap_64 v else v);
       b.pos <- pos')

  let[@inline always] put_float b f =
    put_64 b (Int64.bits_of_float f)

  let put_string b s =
    let slen =
      match String.index_opt s '\000' with
      | Some i -> i
      | None -> String.length s in
    if b.pos + slen + 1 > b.pos_end then raise (overflow b);
    Bytes.blit_string s 0 b.buf b.pos slen;
    Bytes.unsafe_set b.buf (b.pos + slen) '\000';
    b.pos <- b.pos + slen + 1

  let[@inline never] put_vint_big b v =
    if v = v land 0xffff then
      (put_8 b 253; put_16 b v)
    else if v = Int32.to_int (Int32.of_int v) then
      (put_8 b 254; put_32 b (Int32.of_int v))
    else
      (put_8 b 255; put_64 b (Int64.of_int v))

  let[@inline always] put_vint b v =
    if 0 <= v && v <= 252 then
      put_8 b v
    else
      put_vint_big b v

  type position_8 = int
  type position_16 = int
  type position_32 = int
  type position_64 = int
  type position_float = int

  let[@inline always] skip_8 b =
    let pos = b.pos in
    let pos' = b.pos + 1 in
    if pos' > b.pos_end then raise (overflow b);
    b.pos <- pos';
    pos

  let[@inline always] skip_16 b =
    let pos = b.pos in
    let pos' = b.pos + 2 in
    if pos' > b.pos_end then raise (overflow b);
    b.pos <- pos';
    pos

  let[@inline always] skip_32 b =
    let pos = b.pos in
    let pos' = b.pos + 4 in
    if pos' > b.pos_end then raise (overflow b);
    b.pos <- pos';
    pos

  let[@inline always] skip_64 b =
    let pos = b.pos in
    let pos' = b.pos + 8 in
    if pos' > b.pos_end then raise (overflow b);
    b.pos <- pos';
    pos

  let skip_float = skip_64

  let update_8 b pos v =
    assert (pos + 1 <= b.pos_end);
    put_raw_8 b.buf pos v

  let update_16 b pos v =
    assert (pos + 2 <= b.pos_end);
    put_raw_16 b.buf pos v

  let update_32 b pos v =
    assert (pos + 4 <= b.pos_end);
    put_raw_32 b.buf pos v

  let update_64 b pos v =
    assert (pos + 8 <= b.pos_end);
    put_raw_64 b.buf pos v

  let update_float b pos f =
    update_64 b pos (Int64.bits_of_float f)
end

module Read = struct

  include Shared

  let rec read_into fd buf off =
    if off = Bytes.length buf then
      { buf; pos = 0; pos_end = off }
    else begin
      assert (0 <= off && off <= Bytes.length buf);
      let n = Unix.read fd buf off (Bytes.length buf - off) in
      if n = 0 then
        (* EOF *)
        { buf; pos = 0; pos_end = off }
      else
        (* Short read *)
        read_into fd buf (off + n)
    end
  let read_fd fd buf = read_into fd buf 0

  let refill_fd fd b =
    let len = remaining b in
    Bytes.blit b.buf b.pos b.buf 0 len;
    read_into fd b.buf len

  let split b len =
    let len = min (remaining b) len in
    { b with pos_end = b.pos + len },
    { b with pos = b.pos + len }

  let empty = { buf = Bytes.make 0 '?'; pos = 0; pos_end = 0 }

  external get_raw_16 : Bytes.t -> int -> int = "%caml_bytes_get16u"
  external get_raw_32 : Bytes.t -> int -> int32 = "%caml_bytes_get32u"
  external get_raw_64 : Bytes.t -> int -> int64 = "%caml_bytes_get64u"

  exception Underflow of int
  let[@inline never] underflow b = Underflow b.pos

  let[@inline always] get_8 b =
    let pos = b.pos in
    let pos' = b.pos + 1 in
    if pos' > b.pos_end then raise (underflow b);
    b.pos <- pos';
    Char.code (Bytes.unsafe_get b.buf pos)

  let[@inline always] get_16 b =
    let pos = b.pos in
    let pos' = b.pos + 2 in
    if pos' > b.pos_end then raise (underflow b);
    b.pos <- pos';
    if Sys.big_endian then bswap_16 (get_raw_16 b.buf pos) else get_raw_16 b.buf pos

  let[@inline always] get_32 b =
    let pos = b.pos in
    let pos' = b.pos + 4 in
    if pos' > b.pos_end then raise (underflow b);
    b.pos <- pos';
    if Sys.big_endian then bswap_32 (get_raw_32 b.buf pos) else get_raw_32 b.buf pos

  let[@inline always] get_64 b =
    let pos = b.pos in
    let pos' = b.pos + 8 in
    if pos' > b.pos_end then raise (underflow b);
    b.pos <- pos';
    if Sys.big_endian then bswap_64 (get_raw_64 b.buf pos) else get_raw_64 b.buf pos

  let[@inline always] get_float b =
    Int64.float_of_bits (get_64 b)

  let get_string b =
    let start = b.pos in
    while get_8 b <> 0 do () done;
    let len = b.pos - 1 - start in
    Bytes.sub_string b.buf start len

  let[@inline never] get_vint_big b c =
    match c with
    | 253 -> get_16 b
    | 254 -> Int32.to_int (get_32 b)
    | 255 -> Int64.to_int (get_64 b)
    | _ -> assert false

  let[@inline always] get_vint b =
    match get_8 b with
    | c when c < 253 -> c
    | c -> get_vint_big b c
end

let () =
  Printexc.register_printer (function
    | Write.Overflow n ->
       Some ("Buffer overflow at position " ^ string_of_int n)
    | Read.Underflow n ->
       Some ("Buffer underflow at position " ^ string_of_int n)
    | _ -> None)
