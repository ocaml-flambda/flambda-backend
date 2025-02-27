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

(* CR jvanburen: use immediates for small ints *)

let ( + ) a b =
  if no_overflow_add a b then Int.add a b else invalid_arg "overflow"

let ( - ) a b =
  if no_overflow_sub a b then Int.sub a b else invalid_arg "overflow"

let ( * ) a b =
  if no_overflow_mul a b then Int.mul a b else invalid_arg "overflow"

let ( ~- ) x = if x = min_int then invalid_arg "overflow" else Int.neg x

module Nativeint = struct
  include Nativeint

  let size_in_bytes = size / 8

  let trailing_mask ~bits =
    assert (0 <= bits && bits <= size);
    if bits = size then minus_one else pred (shift_left one bits)

  let count_leading_zeros =
    let rec go n ~acc ~num_bits =
      if num_bits = 1
      then acc + 1 - to_int n
      else
        let right = num_bits lsr 1 in
        let left = num_bits - right in
        let left_mask = shift_left minus_one right in
        if logand n left_mask = zero
        then go n ~acc:(acc + left) ~num_bits:right
        else
          let n = shift_right_logical n right in
          go n ~acc ~num_bits:left
    in
    fun n -> go (logand n (trailing_mask ~bits:size)) ~acc:0 ~num_bits:size

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

(* CR-someday jvanburen: use an abstract block or add this to the runtime since
   polymorphic compare doesn't work correctly. Also it's slow lol. *)

let zero = { words = Bytes.empty }

let create_uninitialized num_words =
  assert (num_words >= 0);
  { words = Bytes.create (num_words * Nativeint.size_in_bytes) }

(** The length in words *)
let length { words } = Bytes.length words / Nativeint.size_in_bytes

let sign_bit { words } =
  let len = Bytes.length words in
  if len = 0 then false else Bytes.get_uint8 words (len - 1) land 0x80 <> 0

let equal { words = x } { words = y } = Bytes.equal x y

let get { words } i =
  if i < 0 || i >= length { words } then invalid_arg "Apint.get";
  let i = i * Nativeint.size_in_bytes in
  match Nativeint.size with
  | 32 -> Nativeint.of_int32 (Bytes.get_int32_le words i)
  | 64 -> Int64.to_nativeint (Bytes.get_int64_le words i)
  | _ -> assert false

let compare lhs rhs =
  let unsigned_compare lhs rhs =
    let rec go lhs rhs ~pos =
      if pos < 0
      then 0
      else
        let cmp = Nativeint.unsigned_compare (get lhs pos) (get rhs pos) in
        if cmp <> 0 then cmp else go lhs rhs ~pos:(pos - 1)
    in
    let len = length lhs in
    let cmp = Int.compare len (length rhs) in
    if cmp <> 0 then cmp else go lhs rhs ~pos:(len - 1)
  in
  match sign_bit lhs, sign_bit rhs with
  | true, false -> -1
  | false, true -> 1
  | false, false -> unsigned_compare lhs rhs
  | true, true -> ~-(unsigned_compare lhs rhs)

let sign t = compare t zero

let set { words } i x =
  if i < 0 || i >= length { words } then invalid_arg "Apint.set";
  let i = i * Nativeint.size_in_bytes in
  match Nativeint.size with
  | 32 -> Bytes.set_int32_le words i (Nativeint.to_int32 x)
  | 64 -> Bytes.set_int64_le words i (Int64.of_nativeint x)
  | _ -> assert false

(* let to_string_hex t =
 *   match length t with
 *   | 0 -> "0"
 *   | len ->
 *     (if sign_bit t then "-" else "")
 *     ^ "0x"
 *     ^ (List.init len (fun i ->
 *            if i = 0
 *            then Printf.sprintf "%nx" (get t (len - 1 - i))
 *            else Printf.sprintf "%016nx" (get t (len - 1 - i)))
 *       |> String.concat "") *)

let init num_words ~f =
  let t = create_uninitialized num_words in
  for i = 0 to num_words - 1 do
    set t i (f i)
  done;
  t

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
    else init len ~f:(get t)
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

let of_nativeint (const : nativeint) =
  let t =
    if const = 0n
    then zero
    else
      let t = create_uninitialized 1 in
      set t 0 const;
      t
  in
  t

let of_int ?(unsigned = false) i =
  let n = Nativeint.of_int i in
  let n =
    if unsigned
    then Nativeint.logand n (Nativeint.trailing_mask ~bits:Sys.int_size)
    else n
  in
  of_nativeint n

let one = of_int 1

let minus_one = of_int (-1)

let make_bitwise_op operator x y =
  init
    (Int.max (length x) (length y) + 1)
    ~f:(fun i -> operator (get_extended x i) (get_extended y i))
  |> trim

let logand x y = make_bitwise_op Nativeint.logand x y

let logor x y = make_bitwise_op Nativeint.logor x y

let logxor x y = make_bitwise_op Nativeint.logxor x y

let lognot x = logxor x minus_one

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
  let dst = trim dst in
  (* Printf.printf "assert ( %s << %d == %s )\n%!" (to_string_hex t) amount
   *   (to_string_hex dst); *)
  dst

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

let add x y =
  if x = zero
  then y
  else if y = zero
  then x
  else
    let len = Int.max (length x) (length y) + 1 in
    let dst = create_uninitialized len in
    let carry = ref 0n in
    for i = 0 to len - 1 do
      let x = get_extended x i in
      let y = get_extended y i in
      set dst i (Nativeint.add (Nativeint.add x y) !carry);
      carry
        := let low =
             Nativeint.add (Nativeint.logand 1n x) (Nativeint.logand 1n y)
           in
           let low =
             Nativeint.shift_right_logical (Nativeint.add low !carry) 1
           in
           let high =
             Nativeint.add
               (Nativeint.shift_right_logical x 1)
               (Nativeint.shift_right_logical y 1)
           in
           Nativeint.shift_right_logical (Nativeint.add high low)
             (Nativeint.size - 1)
    done;
    let dst = trim dst in
    (* Printf.printf "assert ( %s + %s == %s )\n%!" (to_string_hex x)
     *   (to_string_hex y) (to_string_hex dst); *)
    dst

let neg t = add (lognot t) (of_int 1)

let sub x y = add x (neg y)

let succ t = add t one

let pred t = add t one

let zero_extend t ~bits =
  if bits < 0 then invalid_arg "Apint.zero_extend";
  let dst =
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
      trim dst)
    else
      let len = num_words ~num_bits:bits in
      let dst = create_uninitialized len in
      blit ~src:t ~dst ~len ();
      if bits mod Nativeint.size <> 0
      then
        get_extended t (len - 1)
        |> Nativeint.zero_extend ~bits:(bits mod Nativeint.size)
        |> set dst (len - 1);
      trim dst
  in
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

let of_nativeint ?(unsigned = false) x =
  let x = of_nativeint x in
  if unsigned then zero_extend x ~bits:Nativeint.size else x

let of_int64 ?(unsigned = false) i =
  match Nativeint.size with
  | 64 -> of_nativeint ~unsigned (Int64.to_nativeint i)
  | 32 ->
    let lo = Int64.to_nativeint i in
    let hi = Int64.to_nativeint (Int64.shift_right_logical i 32) in
    let lo = of_nativeint lo ~unsigned:true in
    let hi = of_nativeint hi ~unsigned in
    logor (shift_left hi 32) lo
  | _ -> assert false

let rec unsigned_mul x y =
  if x = zero || y = zero
  then zero
  else
    let result =
      let bits = Int.max (bit_length x) (bit_length y) in
      if bits <= Nativeint.size / 2
      then of_nativeint (Nativeint.mul (get x 0) (get y 0)) ~unsigned:true
      else
        let bits = bits / 2 in
        let x_hi = shift_right x bits in
        let x_lo = zero_extend x ~bits (* sub x (shift_left x_hi bits) *) in
        let y_hi = shift_right y bits in
        let y_lo = zero_extend y ~bits (* sub y (shift_left y_hi bits) *) in
        add
          (add
             (shift_left (unsigned_mul x_hi y_hi) (2 * bits))
             (unsigned_mul x_lo y_lo))
          (add
             (shift_left (unsigned_mul x_hi y_lo) bits)
             (shift_left (unsigned_mul x_lo y_hi) bits))
    in
    (* Printf.printf "assert ( %s * %s == %s )\n%!" (to_string_hex x)
     *   (to_string_hex y) (to_string_hex result); *)
    result

let mul x y =
  match compare x zero, compare y zero with
  | 0, _ | _, 0 -> zero
  | 1, 1 -> unsigned_mul x y
  | 1, -1 -> neg (unsigned_mul x (neg y))
  | -1, 1 -> neg (unsigned_mul (neg x) y)
  | -1, -1 -> unsigned_mul (neg x) (neg y)
  | _, _ -> assert false

let of_string =
  let fail () = failwith "Apint.of_string" in
  let parse_digit = function
    | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
    | 'a' .. 'f' as c -> Some (10 + Char.code c - Char.code 'a')
    | 'A' .. 'F' as c -> Some (10 + Char.code c - Char.code 'A')
    | _ -> None
  in
  let rec parse_body s ~pos ~len ~acc ~base =
    if pos = len
    then acc
    else
      match s.[pos] with
      | '_' -> parse_body s ~pos:(pos + 1) ~len ~acc ~base
      | c -> (
        match parse_digit c with
        | Some d when d < base ->
          let acc = add (unsigned_mul acc (of_int base)) (of_int d) in
          parse_body s ~pos:(pos + 1) ~len ~base ~acc
        | _ -> fail ())
  in
  let parse_body s ~pos ~len ~base =
    if pos = len
    then fail ()
    else
      match parse_digit s.[pos] with
      | Some d when d < base ->
        parse_body s ~pos:(pos + 1) ~len ~base ~acc:(of_int d)
      | _ -> fail ()
  in
  let parse_nonnegative s ~pos ~len =
    if pos = len
    then fail ()
    else
      match s.[pos] with
      | '0' when pos + 1 < len -> (
        match s.[pos + 1] with
        | 'x' | 'X' -> parse_body s ~pos:(pos + 2) ~len ~base:16
        | 'u' | 'U' -> parse_body s ~pos:(pos + 2) ~len ~base:10
        | 'o' | 'O' -> parse_body s ~pos:(pos + 2) ~len ~base:8
        | 'b' | 'B' -> parse_body s ~pos:(pos + 2) ~len ~base:2
        | _ -> parse_body s ~pos ~len ~base:10)
      | _ -> parse_body s ~pos ~len ~base:10
  in
  fun s ->
    match String.length s with
    | 0 -> fail ()
    | len -> (
      match s.[0] with
      | '-' -> neg (parse_nonnegative s ~pos:1 ~len)
      | '+' -> parse_nonnegative s ~pos:1 ~len
      | _ -> parse_nonnegative s ~pos:0 ~len)

let unsigned_div dividend divisor =
  if divisor = zero
  then raise Division_by_zero
  else if divisor = one
  then dividend
  else if dividend = divisor
  then one
  else
    (* Binary long division algorithm *)
    let rec go ~quotient ~remainder =
      (* Printf.printf "quotient= %s\nremainder= %s\ndivisor= %s\n%!"
       *   (to_string_hex quotient) (to_string_hex remainder)
       *   (to_string_hex divisor); *)
      if compare remainder divisor < 0
      then
        (* Printf.printf "compare %s %s < 0\n%!" (to_string_hex remainder)
         *   (to_string_hex divisor); *)
        quotient
      else
        let shift, divisor =
          (* Find the largest shift such that (divisor << shift) <= remainder *)
          let ceil_lg2_remainder = bit_length (pred remainder) in
          let ceil_lg2_divisor = bit_length (pred divisor) in
          let rec go ~shift =
            assert (shift >= ceil_lg2_remainder - ceil_lg2_divisor - 1);
            let divisor = shift_left divisor shift in
            if compare divisor remainder <= 0
            then shift, divisor
            else go ~shift:(shift - 1)
          in
          let shift = ceil_lg2_remainder - ceil_lg2_divisor + 1 in
          assert (compare (shift_left divisor (shift + 1)) remainder > 0);
          go ~shift
        in
        let bit = shift_left one shift in
        (* Printf.printf "> 1 << %d = %s\n>divisor << %d = %s\n\n%!" shift
         *   (to_string_hex bit) shift (to_string_hex divisor); *)
        go ~quotient:(add quotient bit) ~remainder:(sub remainder divisor)
    in
    go ~quotient:zero ~remainder:dividend

let div x y =
  match compare x zero, compare y zero with
  | _, 0 -> raise Division_by_zero
  | 0, _ -> zero
  | 1, 1 -> unsigned_div x y
  | 1, -1 -> neg (unsigned_div x (neg y))
  | -1, 1 -> neg (unsigned_div (neg x) y)
  | -1, -1 -> unsigned_div (neg x) (neg y)
  | _, _ -> assert false

let rem x y = sub x (mul (div x y) y)

let min_int ?(unsigned = false) () ~bits =
  if bits < 0 then invalid_arg "APInt.min_int";
  if unsigned || bits = 0 then zero else neg (shift_left one (bits - 1))

let max_int ?(unsigned = false) () ~bits =
  if bits < 0 then invalid_arg "APInt.min_int";
  if unsigned || bits = 0
  then pred (shift_left one bits)
  else pred (shift_left one (bits - 1))

let to_nativeint t = get_extended t 0

let to_int t = Nativeint.to_int (to_nativeint t)

let to_int_exn ?(unsigned = false) t =
  let lo = min_int ~unsigned () ~bits:Sys.int_size in
  let hi = max_int ~unsigned () ~bits:Sys.int_size in
  if compare t lo < 0 || compare hi t < 0
  then failwith "APInt.to_int_exn"
  else to_int t

let to_nativeint_exn ?(unsigned = false) t =
  let lo = min_int ~unsigned () ~bits:Nativeint.size in
  let hi = max_int ~unsigned () ~bits:Nativeint.size in
  if compare t lo < 0 || compare hi t < 0
  then failwith "APInt.to_nativeint_exn"
  else to_nativeint t

let rec to_string t =
  let sign = compare t zero in
  if sign = 0
  then "0"
  else if sign < 0
  then "-" ^ to_string (neg t)
  else
    let ten = of_int 10 in
    let rec go t ~digits =
      if equal t zero
      then String.concat "" digits
      else
        let digit = rem t ten in
        (* Printf.printf "%s / 10 = %s\n%s %% 10 = %s\n\n%!" (to_string_hex t)
         *   (to_string_hex (div t ten))
         *   (to_string_hex t)
         *   (to_string_hex (rem t ten)); *)
        let digit = string_of_int (to_int_exn digit) in
        go (div t ten) ~digits:(digit :: digits)
    in
    go t ~digits:[]

let of_float f =
  if Float.is_infinite f || Float.abs f < 1.
  then zero
  else
    let bits = Int64.bits_of_float f in
    let sign = Int64.logand bits Int64.min_int <> 0L in
    let exponent = Int64.to_int (Int64.shift_right bits 52) land 0x7ff in
    let mantissa = Int64.logand bits 0xfffffffffffffL in
    let bias = 1023 in
    let t = of_int64 (Int64.logor 0x10000000000000L mantissa) in
    let t = shift_left t exponent in
    shift_right t (bias + 52)

(* let rec to_float t =
 *   if t = zero
 *   then 0.
 *   else if sign t < 0
 *   then -.to_float (neg t)
 *   else (
 *     (* positive integers only *)
 *     let bits = bit_length t in
 *     let bias = 1023 in
 *     if bits + bias > 0x7ff
 *     then Float.infinity
 *     (* align the number so that the bit length is exactly 53 *)
 *     else
 *       let mantissa, exponent =
 *         if bits = 53 then
 *           to_int
 *
 *
 *   ) *)

(* let big = of_string "2000000000000000000000000000000000000001" *)
