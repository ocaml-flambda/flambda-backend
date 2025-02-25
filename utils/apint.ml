(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Jacob Van Buren, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let no_overflow_add a b = a lxor b lor (a lxor lnot (a + b)) < 0

let no_overflow_sub a b = a lxor lnot b lor (b lxor (a - b)) < 0

(* Taken from Hacker's Delight, chapter "Overflow Detection" *)
let no_overflow_mul a b =
  not ((a = min_int && b < 0) || (b <> 0 && a * b / b <> a))

let no_overflow_lsl a k =
  0 <= k && k < Sys.word_size - 1 && min_int asr k <= a && a <= max_int asr k

let ( + ) a b =
  if no_overflow_add a b then Int.add a b else invalid_arg "overflow"

let ( - ) a b =
  if no_overflow_sub a b then Int.sub a b else invalid_arg "overflow"

let ( * ) a b =
  if no_overflow_mul a b then Int.mul a b else invalid_arg "overflow"

let ( lsl ) a b =
  if no_overflow_lsl a b then Int.shift_left a b else invalid_arg "overflow"

let ( ~- ) x = if x = min_int then invalid_arg "overflow" else Int.neg x

module Nativeint = struct
  include Nativeint

  let size_in_bytes = size / 8

  let trailing_mask ~bits =
    assert (0 <= bits && bits <= size);
    if bits = size then -1n else pred (shift_left 1n bits)

  let count_leading_zeros =
    let rec go n ~acc ~num_bits =
      if num_bits = 1
      then acc + 1 - to_int n
      else
        let right = num_bits lsr 1 in
        let left = num_bits - right in
        let left_mask = shift_left (-1n) right in
        if logand n left_mask = 0n
        then go n ~acc:(acc + left) ~num_bits:right
        else
          let n = shift_right_logical n right in
          go n ~acc ~num_bits:left
    in
    fun n -> go (logand n (trailing_mask ~bits:size)) ~acc:0 ~num_bits:size

  let count_trailing_zeros =
    let rec go n ~acc ~num_bits =
      if n = 0n
      then acc + num_bits
      else if num_bits = 1
      then acc
      else
        let right = num_bits lsr 1 in
        let left = num_bits - right in
        let right_mask = pred (shift_left 1n right) in
        if logand n right_mask <> 0n
        then go n ~acc ~num_bits:right
        else
          let n = shift_right_logical n right in
          go n ~acc:(acc + right) ~num_bits:left
    in
    fun n -> go n ~acc:0 ~num_bits:size

  let count_trailing_ones x = count_trailing_zeros (lognot x)

  let count_leading_ones x = count_leading_zeros (lognot x)

  let sign_extend t ~bits =
    assert (0 < bits && bits < size);
    let unused = size - bits in
    let t = shift_left t unused in
    shift_right t unused

  let zero_extend t ~bits =
    assert (0 < bits && bits < size);
    let mask = pred (shift_left 1n bits) in
    logand t mask
end

let num_words ~num_bits =
  assert (0 <= num_bits && num_bits + Nativeint.size - 1 > 0);
  (num_bits + Nativeint.size - 1) / Nativeint.size

(** The bits of the number, stored little-endian, meaning the byte at index 0 is the
    least-significant byte. [t] is always stored sign-extended such that its length is an
    even multiple of [Nativeint.size_in_bytes].

    For example, to store the UNSIGNED integer 0xffff_ffff on a 32-bit system, the
    representation would be 4 \xff bytes, followed by 4 \x00 bytes, since that makes the
    sign bit (i.e., the most-significant bit 0). *)
type t = { words : bytes } [@@unboxed]

let zero = { words = Bytes.empty }

let create_uninitialized num_words =
  assert (num_words >= 0);
  { words = Bytes.create (num_words * Nativeint.size_in_bytes) }

(** The length in words *)
let length { words } = Bytes.length words / Nativeint.size_in_bytes

let sign_bit { words } =
  let len = Bytes.length words in
  if len = 0 then false else Bytes.get_uint8 words (len - 1) land 0x80 <> 0

let compare lhs rhs =
  let unsigned_compare lhs rhs =
    let cmp = Int.compare (Bytes.length lhs) (Bytes.length rhs) in
    if cmp <> 0 then cmp else Bytes.compare lhs rhs
  in
  match sign_bit lhs, sign_bit rhs with
  | true, false -> -1
  | false, true -> 1
  | false, false -> unsigned_compare lhs.words rhs.words
  | true, true -> ~-(unsigned_compare lhs.words rhs.words)

let get { words } i =
  if i < 0 then invalid_arg "Apint.get";
  let i = i * Nativeint.size_in_bytes in
  match Nativeint.size with
  | 32 -> Nativeint.of_int32 (Bytes.get_int32_le words i)
  | 64 -> Int64.to_nativeint (Bytes.get_int64_le words i)
  | _ -> assert false

let set { words } i x =
  if i < 0 then invalid_arg "Apint.set";
  let i = i * Nativeint.size_in_bytes in
  match Nativeint.size with
  | 32 -> Bytes.set_int32_le words i (Nativeint.to_int32 x)
  | 64 -> Bytes.set_int64_le words i (Int64.of_nativeint x)
  | _ -> assert false

let get_extended { words } i =
  let len = Bytes.length words in
  if 0 <= i && i < len / Nativeint.size_in_bytes
  then get { words } i
  else if i > 0 && sign_bit { words }
  then -1n
  else 0n

let bit t i =
  if i < 0 then invalid_arg "Apint.bit";
  let word = get_extended t (i / Nativeint.size) in
  let mask = Nativeint.shift_left 1n (i mod Nativeint.size) in
  Nativeint.logand word mask <> 0n

let blit ~src ~dst ?(src_pos = 0) ?(dst_pos = 0) ~len () =
  assert (src_pos >= 0);
  assert (dst_pos >= 0);
  assert (len >= 0);
  assert (src_pos <= length src);
  assert (dst_pos <= length dst - len);
  let src_pos = src_pos * Nativeint.size_in_bytes in
  let dst_pos = dst_pos * Nativeint.size_in_bytes in
  let len = len * Nativeint.size_in_bytes in
  let blit_len = min len (Bytes.length src.words - src_pos) in
  BytesLabels.blit ~src:src.words ~src_pos ~dst:dst.words ~dst_pos ~len:blit_len;
  let fill_len = len - blit_len in
  BytesLabels.fill dst.words ~pos:(dst_pos + blit_len) ~len:fill_len
    (match sign_bit src with true -> '\xff' | false -> '\x00')

(** remove redundant leading words *)
let trim t =
  let is_most_significant_word_redundant t ~len =
    (* The most significant word is redundant if it is the same as the sign bit
       of previous word *)
    if len = 1
    then get t 0 = 0n
    else
      let word = get t (len - 1) in
      let prev = get t (len - 2) in
      word = Nativeint.shift_right prev (Nativeint.size - 1)
  in
  let rec go t ~len =
    if len = 0
    then zero
    else if is_most_significant_word_redundant t ~len
    then go t ~len:(len - 1)
    else if len = length t
    then t
    else
      let dst = create_uninitialized len in
      blit ~src:t ~dst ~len ();
      dst
  in
  go t ~len:(length t)

let bit_length t =
  match length t with
  | 0 -> 0
  | len ->
    let most_significant_word = get t (len - 1) in
    let ignored_bits =
      if sign_bit t
      then Nativeint.count_leading_ones most_significant_word - 1
      else Nativeint.count_leading_zeros most_significant_word
    in
    (len * Nativeint.size) - ignored_bits

let sign_extend t ~bits =
  if bits < 0 then invalid_arg "Apint.sign_extend";
  if bits >= length t * Nativeint.size
  then t
  else
    let len = num_words ~num_bits:bits in
    let dst = create_uninitialized len in
    blit ~src:t ~dst ~len ();
    if bits mod Nativeint.size <> 0
    then
      get t (len - 1)
      |> Nativeint.sign_extend ~bits:(bits mod Nativeint.size)
      |> set dst (len - 1);
    trim dst

let zero_extend t ~bits =
  if bits < 0 then invalid_arg "Apint.zero_extend";
  if bits = 0
  then zero
  else if bits >= length t * Nativeint.size && compare t zero >= 0
  then t
  else if bit t (bits - 1) && bits mod Nativeint.size = 0
  then (
    (* we need an extra word to store a leading zero *)
    let len = 1 + num_words ~num_bits:bits in
    let dst = create_uninitialized len in
    blit ~src:t ~dst ~len:(len - 1) ();
    set dst (len - 1) 0n;
    dst)
  else
    let len = num_words ~num_bits:bits in
    let dst = create_uninitialized len in
    blit ~src:t ~dst ~len:(len - 1) ();
    if bits mod Nativeint.size <> 0
    then
      get t (len - 1)
      |> Nativeint.zero_extend ~bits:(bits mod Nativeint.size)
      |> set dst (len - 1);
    dst

let byteswap t ~bytes =
  assert (bytes >= 0);
  if length t = 0
  then zero
  else
    let get_byte i =
      if i < Bytes.length t.words
      then Bytes.get t.words i
      else match sign_bit t with true -> '\xff' | false -> '\x00'
    in
    let len = num_words ~num_bits:(8 * bytes) in
    let dst = create_uninitialized len in
    for i = 0 to len - 1 do
      Bytes.set dst.words i (get_byte (len - 1 - i))
    done;
    zero_extend dst ~bits:(bytes * 8)

let print ppf t =
  let rec go ppf t ~i ~len =
    if i < len
    then (
      let word = get t i in
      (if i = 0
      then Format.fprintf ppf "%nx" word
      else
        match Nativeint.size with
        | 32 -> Format.fprintf ppf "%08nx" word
        | 64 -> Format.fprintf ppf "%016nx" word
        | _ -> assert false);
      go ppf t ~i:(i + 1) ~len)
  in
  match length t with
  | 0 -> Format.pp_print_string ppf "0"
  | len -> go ppf t ~i:0 ~len

let of_constant ?(unsigned = false) (const : nativeint) =
  let t =
    if const = 0n
    then zero
    else
      let t = create_uninitialized 1 in
      set t 0 const;
      t
  in
  if unsigned then zero_extend t ~bits:Nativeint.size else t

let of_int ?(unsigned = false) i =
  let n = Nativeint.of_int i in
  let n =
    if unsigned
    then Nativeint.logand n (Nativeint.trailing_mask ~bits:Sys.int_size)
    else n
  in
  of_constant ~unsigned n

let not t =
  let len = length t + 1 in
  let dst = create_uninitialized len in
  for i = 0 to len - 1 do
    set dst i (Nativeint.lognot (get_extended t i))
  done;
  trim dst

let shift_left t amount =
  if amount < 0 then invalid_arg "Apint.shift_left";
  let len = num_words ~num_bits:(bit_length t + 1 + amount) in
  let dst = create_uninitialized len in
  for i = 0 to len - 1 do
    set dst i
      (let i = i - (amount / Nativeint.size) in
       let offset = amount mod Nativeint.size in
       if offset = 0
       then get_extended t i
       else
         let upper = get_extended t i in
         let lower = get_extended t (i - 1) in
         let upper = Nativeint.shift_left upper offset in
         let lower =
           Nativeint.shift_right_logical lower (Nativeint.size - offset)
         in
         Nativeint.logor upper lower)
  done;
  trim dst

let shift_right t amount =
  if amount < 0 then invalid_arg "Apint.shift_right";
  let num_bits = bit_length t + 1 - amount in
  if num_bits <= 0
  then zero
  else
    let len = num_words ~num_bits:(bit_length t + 1 - amount) in
    let dst = create_uninitialized len in
    for i = 0 to len - 1 do
      set dst i
        (let i = i + (amount / Nativeint.size) in
         let offset = amount mod Nativeint.size in
         if offset = 0
         then get_extended t i
         else
           let upper = get_extended t (i + 1) in
           let lower = get_extended t i in
           let upper = Nativeint.shift_left upper (Nativeint.size - offset) in
           let lower = Nativeint.shift_right_logical lower offset in
           Nativeint.logor upper lower)
    done;
    trim dst
