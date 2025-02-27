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

let[@inline] no_overflow_add a b = a lxor b lor (a lxor lnot (a + b)) < 0

let[@inline] no_overflow_sub a b = a lxor lnot b lor (b lxor (a - b)) < 0

(* Taken from Hacker's Delight, chapter "Overflow Detection" *)
let[@inline] no_overflow_mul a b =
  not ((a = min_int && b < 0) || (b <> 0 && a * b / b <> a))

let[@inline] no_overflow_div a b = b <> -1 || a <> min_int

let no_overflow_rem = no_overflow_div

let ( + ) a b =
  if no_overflow_add a b then Int.add a b else invalid_arg "overflow"

let ( - ) a b =
  if no_overflow_sub a b then Int.sub a b else invalid_arg "overflow"

let ( * ) a b =
  if no_overflow_mul a b then Int.mul a b else invalid_arg "overflow"

let ( ~- ) x = if x = min_int then invalid_arg "overflow" else Int.neg x

module Int64 = struct
  include Int64

  let size = 64

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
    let mask = pred (shift_left 1L bits) in
    logand t mask
end

let num_words ~num_bits =
  assert (0 <= num_bits && num_bits + Int64.size - 1 > 0);
  (num_bits + Int64.size - 1) / Int64.size

module Big = struct
  (** The bits of the number, stored little-endian, meaning the byte at index 0 is the
    least-significant byte. [t] is always stored sign-extended such that its length is an
    even multiple of 8.

    For example, to store the positive integer 2^64-1, the representation would be 8 \xff
    bytes, followed by 8 \x00 bytes, since that makes the sign bit (i.e., the
    most-significant bit) 0. *)
  type t = { words : bytes } [@@unboxed]

  (* CR-someday jvanburen: use an abstract block or add this to the runtime
     since polymorphic compare doesn't work correctly. Also it's slow lol. *)

  let zero = { words = Bytes.empty }

  let create_uninitialized num_words =
    assert (num_words >= 0);
    { words = Bytes.create (num_words * Int64.size_in_bytes) }

  (** The length in words *)
  let length { words } =
    let len = Bytes.length words / Int64.size_in_bytes in
    assert (len > 0);
    len

  let equal { words = x } { words = y } = Bytes.equal x y

  let get { words } i =
    if i < 0 || i >= length { words } then invalid_arg "Z.get";
    let i = i * Int64.size_in_bytes in
    Bytes.get_int64_le words i

  let sign_bit t = get t (length t - 1) < 0L

  let compare lhs rhs =
    let unsigned_compare lhs rhs =
      let rec go lhs rhs ~pos =
        if pos < 0
        then 0
        else
          let cmp = Int64.unsigned_compare (get lhs pos) (get rhs pos) in
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
    | true, true -> -unsigned_compare lhs rhs

  let sign t = compare t zero

  let set { words } i x =
    if i < 0 || i >= length { words } then invalid_arg "Z.set";
    let i = i * Int64.size_in_bytes in
    Bytes.set_int64_le words i x

  (* let to_string_hex t =
   *   match length t with
   *   | 0 -> "0"
   *   | len ->
   *     (if sign_bit t then "-" else "")
   *     ^ "0x"
   *     ^ (List.init len (fun i ->
   *            if i = 0
   *            then Printf.sprintf "%nx" (get t (len - 1 - i))
   *            else Printf.sprintf "%016Lx" (get t (len - 1 - i)))
   *       |> String.concat "") *)

  let init num_words ~f =
    let t = create_uninitialized num_words in
    for i = 0 to num_words - 1 do
      set t i (f i)
    done;
    t

  let get_extended { words } i =
    let len = Bytes.length words in
    if 0 <= i && i < len / Int64.size_in_bytes
    then get { words } i
    else if i > 0 && sign_bit { words }
    then -1L
    else 0L

  let bit t i =
    if i < 0 then invalid_arg "Z.bit";
    let word = get_extended t (i / Int64.size) in
    let mask = Int64.shift_left 1L (i mod Int64.size) in
    Int64.logand word mask <> 0L

  let[@inline] bit_length =
    (* sign_word is either 0L or -1L, depending on the sign of the argument. *)
    let of_last_word word ~pos ~sign_word =
      let ignored_bits =
        let word = Int64.logxor word sign_word in
        let bits = Int64.count_leading_zeros word in
        bits + Int64.to_int sign_word
      in
      (pos * Int64.size) - ignored_bits
    in
    let rec go t ~pos ~sign_word =
      if pos < 0
      then 0
      else
        let word = get t pos in
        if word = sign_word
        then go t ~pos:(pos - 1) ~sign_word
        else of_last_word word ~pos ~sign_word
    in
    let[@local] go t ~pos ~bits ~unsigned =
      let last_word = get t pos in
      let last_word =
        if bits = 0
        then last_word
        else if unsigned
        then Int64.zero_extend last_word ~bits
        else Int64.sign_extend last_word ~bits
      in
      let sign_word =
        if unsigned then 0L else Int64.shift_right last_word (Int64.size - 1)
      in
      if last_word <> sign_word
      then of_last_word last_word ~pos ~sign_word
      else go t ~pos:(pos - 1) ~sign_word
    in
    fun ?(unsigned = false) ?bits t ->
      match bits with
      | None -> go ~unsigned t ~pos:(length t - 1) ~bits:0
      | Some 0 -> 0
      | Some bits ->
        assert (bits > 0);
        let len = length t in
        let pos = bits / Int64.size in
        let bits = bits mod Int64.size in
        if pos < len
        then go ~unsigned t ~pos ~bits
        else go ~unsigned t ~pos:(len - 1) ~bits:0

  let of_int64 ?(unsigned = false) i =
    assert (i <> 0L);
    if unsigned && i < 0L
    then init 2 ~f:(function 0 -> i | _ -> 0L)
    else init 1 ~f:(fun _ -> i)

  let to_int64 t = get t 0
end

(** Either an immediate integer, or a [Big.t].
    Invariant: small numbers are always represented as immediate integers. *)
type t

type repr =
  | Small of int
  | Big of Big.t

external of_int : int -> t = "%identity"

external of_big_unchecked : Big.t -> t = "%identity"

let zero = of_int 0

let one = of_int 1

let minus_one = of_int (-1)

let[@inline] repr (t : t) =
  let t = Obj.repr t in
  if Obj.is_int t then Small (Obj.obj t) else Big (Obj.obj t)

(** pack a big number into a [t] *)
let of_big big =
  let bit_length = Big.bit_length big + 1 (* don't forget the sign bit *) in
  if bit_length <= Sys.int_size
  then of_int (Int64.to_int (Big.to_int64 big))
  else
    let len = num_words ~num_bits:bit_length in
    if Big.length big = len
    then of_big_unchecked big
    else (* trim the excess *)
      of_big_unchecked (Big.init len ~f:(Big.get big))

let of_big_unchecked big =
  (* CR jvanburen: remove this *)
  let t = of_big big in
  match repr t with Big _ -> t | Small _ -> assert false

let[@inline] of_int64 ?(unsigned = false) i =
  let top_bits = Int64.shift_right i (Sys.int_size - 1) in
  let fits = top_bits = 0L || ((not unsigned) && top_bits = -1L) in
  if fits
  then of_int (Int64.to_int i)
  else of_big_unchecked (Big.of_int64 ~unsigned i)

let bit_length t =
  match repr t with
  | Big big -> Big.bit_length big
  | Small x ->
    let word = Int64.of_int x in
    let ignored_bits =
      if x < 0
      then Int64.count_leading_ones word - 1
      else Int64.count_leading_zeros word
    in
    Int64.size - ignored_bits

let sign_extend t ~bits =
  if bits < 0
  then invalid_arg "Z.sign_extend"
  else if bits = 0
  then zero
  else
    match repr t with
    | Small x ->
      if bits >= Sys.int_size
      then t
      else Int64.of_int x |> Int64.sign_extend ~bits |> Int64.to_int |> of_int
    | Big b ->
      if bits >= Big.bit_length b
      then t
      else
        (* optimization for sign-extending to a small int *)
        let num_bits = 1 + Big.bit_length b ~bits in
        if bits <= Sys.int_size
        then Big.to_int64 b |> Int64.sign_extend ~bits |> Int64.to_int |> of_int
        else
          let len = num_words ~num_bits:bits in
          Big.init len ~f:(fun i ->
              let word = Big.get b i in
              if i < len - 1
              then word
              else Int64.sign_extend word ~bits:(bits - 1))
          |> of_big_unchecked

let one = of_int 1

let minus_one = of_int (-1)

let[@inline] make_bitwise_op operator x y =
  match repr x, repr y with
  | Small x, Small y ->
    of_int (Int64.to_int (operator (Int64.of_int x) (Int64.of_int y)))
  | Big x, Big y ->
    Big.init
      (Int.max (Big.length x) (Big.length y))
      ~f:(fun i -> operator (Big.get_extended x i) (Big.get_extended y i))
    |> of_big
  | Big b, Small s | Small s, Big b ->
    let s = Int64.of_int s in
    let sign_word = Int64.shift_right s (Int64.size - 1) in
    Big.init (Big.length b) ~f:(fun i ->
        operator (Big.get_extended x i) (if i = 0 then s else sign_word))
    |> of_big

let logand x y =
  match repr x, repr y with
  | Small 0, _ | _, Small 0 -> zero
  | Small -1, _ -> y
  | _, Small -1 -> x
  | _, _ -> make_bitwise_op Int64.logand x y

let logor x y =
  match repr x, repr y with
  | Small -1, _ | _, Small -1 -> minus_one
  | Small 0, _ -> y
  | _, Small 0 -> x
  | _, _ -> make_bitwise_op Int64.logor x y

let lognot x =
  match repr x with
  | Small x -> of_int (Int.lognot x)
  | Big b ->
    Big.init (Big.length b) ~f:(fun i -> Int64.lognot (Big.get_extended x i))
    |> of_big_unchecked

let logxor x y =
  let logor x y =
    match repr x, repr y with
    | Small -1, _ | _, Small -1 -> minus_one
    | Small 0, _ -> y
    | _, Small 0 -> x
    | _, _ -> make_bitwise_op ~int:Int.logor ~int64:Int64.logxor x y

let shift_left t amount =
  if amount < 0 then invalid_arg "Z.shift_left";
  let len = num_words ~num_bits:(bit_length t + 1 + amount) in
  let dst = create_uninitialized len in
  for i = 0 to len - 1 do
    set dst i
      (let i = i - (amount / Int64.size) in
       let offset = amount mod Int64.size in
       if offset = 0
       then get_extended t i
       else
         let upper = get_extended t i in
         let lower = get_extended t (i - 1) in
         let upper = Int64.shift_left upper offset in
         let lower = Int64.shift_right_logical lower (Int64.size - offset) in
         Int64.logor upper lower)
  done;
  let dst = trim dst in
  (* Printf.printf "assert ( %s << %d == %s )\n%!" (to_string_hex t) amount
   *   (to_string_hex dst); *)
  dst

let shift_right t amount =
  if amount < 0 then invalid_arg "Z.shift_right";
  let num_bits = bit_length t + 1 - amount in
  if num_bits <= 0
  then zero
  else
    let len = num_words ~num_bits:(bit_length t + 1 - amount) in
    let dst = create_uninitialized len in
    for i = 0 to len - 1 do
      set dst i
        (let i = i + (amount / Int64.size) in
         let offset = amount mod Int64.size in
         if offset = 0
         then get_extended t i
         else
           let upper = get_extended t (i + 1) in
           let lower = get_extended t i in
           let upper = Int64.shift_left upper (Int64.size - offset) in
           let lower = Int64.shift_right_logical lower offset in
           Int64.logor upper lower)
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
    let carry = ref 0L in
    for i = 0 to len - 1 do
      let x = get_extended x i in
      let y = get_extended y i in
      set dst i (Int64.add (Int64.add x y) !carry);
      carry
        := let low = Int64.add (Int64.logand 1L x) (Int64.logand 1L y) in
           let low = Int64.shift_right_logical (Int64.add low !carry) 1 in
           let high =
             Int64.add
               (Int64.shift_right_logical x 1)
               (Int64.shift_right_logical y 1)
           in
           Int64.shift_right_logical (Int64.add high low) (Int64.size - 1)
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
  if bits < 0 then invalid_arg "Z.zero_extend";
  let dst =
    if bits = 0
    then zero
    else if bits >= length t * Int64.size && compare t zero >= 0
    then t
    else if bit t (bits - 1) && bits mod Int64.size = 0
    then (
      (* we need an extra word to store a leading zero *)
      let len = 1 + num_words ~num_bits:bits in
      let dst = create_uninitialized len in
      blit ~src:t ~dst ~len:(len - 1) ();
      set dst (len - 1) 0L;
      trim dst)
    else
      let len = num_words ~num_bits:bits in
      let dst = create_uninitialized len in
      blit ~src:t ~dst ~len ();
      if bits mod Int64.size <> 0
      then
        get_extended t (len - 1)
        |> Int64.zero_extend ~bits:(bits mod Int64.size)
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

let rec unsigned_mul x y =
  if x = zero || y = zero
  then zero
  else
    let result =
      let bits = Int.max (bit_length x) (bit_length y) in
      if bits <= Int64.size / 2
      then of_int64 (Int64.mul (get x 0) (get y 0)) ~unsigned:true
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
  let fail () = failwith "Z.of_string" in
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
  if bits < 0 then invalid_arg "Z.min_int";
  if unsigned || bits = 0 then zero else neg (shift_left one (bits - 1))

let max_int ?(unsigned = false) () ~bits =
  if bits < 0 then invalid_arg "Z.min_int";
  if unsigned || bits = 0
  then pred (shift_left one bits)
  else pred (shift_left one (bits - 1))

let to_int64 t = get_extended t 0

let to_nativeint t = Int64.to_nativeint (to_int64 t)

let to_int t = Int64.to_int (to_int64 t)

let to_int_exn ?(unsigned = false) t =
  match length t with
  | 0 -> 0
  | 1 ->
    let i = get t 0 in
    let lo = if unsigned then 0L else Int64.of_int Int.min_int in
    let hi =
      if unsigned
      then Int64.pred (Int64.shift_left 1L Sys.int_size)
      else Int64.of_int Int.max_int
    in
    if lo <= i && i <= hi then Int64.to_int i else failwith "Z.to_int_exn"
  | _ -> failwith "Z.to_int_exn"

let to_nativeint_exn ?(unsigned = false) t =
  let lo = min_int ~unsigned () ~bits:Nativeint.size in
  let hi = max_int ~unsigned () ~bits:Nativeint.size in
  if compare t lo < 0 || compare hi t < 0
  then failwith "Z.to_nativeint_exn"
  else to_nativeint t

let to_int64_exn ?(unsigned = false) t =
  let lo = min_int ~unsigned () ~bits:Int64.size in
  let hi = max_int ~unsigned () ~bits:Int64.size in
  if compare t lo < 0 || compare hi t < 0
  then failwith "Z.to_int64_exn"
  else to_int64 t

let rec to_string t =
  let sign = sign t in
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

let mantissa_mask = 0xf_ffff_ffff_ffffL

let of_float f =
  if Float.is_infinite f || Float.abs f < 1.
  then zero
  else
    let bits = Int64.bits_of_float f in
    let sign = Int64.logand bits Int64.min_int <> 0L in
    let exponent = Int64.to_int (Int64.shift_right bits 52) land 0x7ff in
    let mantissa = Int64.logand bits mantissa_mask in
    let bias = 1023 in
    let t = of_int64 (Int64.logor mantissa mantissa_mask) in
    let t = shift_left t exponent in
    let t = shift_right t (bias + 52) in
    if sign then neg t else t

let to_float t =
  if t = zero
  then 0.
  else
    let sign_bit, t = if sign t > 0 then 0L, t else Int64.min_int, neg t in
    let msb = bit_length t - 1 in
    let exponent =
      let bias = 1023 in
      msb + bias
    in
    if exponent > 0x7ff
    then if sign t < 0 then Float.neg_infinity else Float.infinity
    else
      let exponent_bits = Int64.shift_left (Int64.of_int exponent) 52 in
      let mantissa_bits =
        let mantissa =
          if msb <= 52 then shift_left t (52 - msb) else shift_right t (msb - 52)
        in
        Int64.logand (to_int64 mantissa) mantissa_mask
      in
      sign_bit |> Int64.logor exponent_bits |> Int64.logor mantissa_bits
      |> Int64.float_of_bits

let of_int ?(unsigned = false) i =
  if (not unsigned) || i >= 0
  then small_obj i
  else big_obj (Big.of_int i ~unsigned)

let equal x y =
  match repr x, repr y with
  | Small x, Small y -> Int.equal x y
  | Small _, Big _ | Big _, Small _ -> false
  | Big x, Big y -> Big.equal x y

let compare x y =
  match repr x, repr y with
  | Small x, Small y -> Int.compare x y
  | x, y -> Big.compare (to_big x) (to_big y)

let neg x =
  match repr x with
  | Small x when x <> Int.min_int -> small_obj (-x)
  | x -> obj (Big.neg (to_big x))

let add x y =
  match repr x, repr y with
  | Small x, Small y when no_overflow_add x y -> small_obj (Int.add x y)
  | x, y -> obj (Big.add (to_big x) (to_big y))

let sub x y =
  match repr x, repr y with
  | Small x, Small y when no_overflow_sub x y -> small_obj (Int.sub x y)
  | x, y -> obj (Big.sub (to_big x) (to_big y))

let mul x y =
  match repr x, repr y with
  | Small 0, _ | _, Small 0 -> zero
  | Small 1, _ -> y
  | _, Small 1 -> x
  | Small x, Small y when no_overflow_mul x y -> small_obj (Int.mul x y)
  | x, y -> obj (Big.mul (to_big x) (to_big y))

let div x y =
  match repr x, repr y with
  | _, Small 0 -> raise Division_by_zero
  | _, Small 1 -> x
  | Small 0, _ -> zero
  | Small x, Small y when no_overflow_div x y -> small_obj (Int.div x y)
  | x, y -> obj (Big.div (to_big x) (to_big y))

let rem x y =
  match repr x, repr y with
  | _, Small 0 -> raise Division_by_zero
  | _, Small 1 -> zero
  | Small 0, _ -> zero
  | Small x, Small y when no_overflow_div x y -> small_obj (Int.rem x y)
  | x, y -> obj (Big.rem (to_big x) (to_big y))

let succ x = add x one

let pred x = sub x one

let sign_extend x ~bits =
  if bits < 0
  then invalid_arg "Z.sign_extend"
  else if bits = 0
  then zero
  else
    match repr x with
    | Big x -> obj (Big.sign_extend x ~bits)
    | Small x ->
      if bits >= Sys.int_size
      then small_obj x
      else
        Int64.of_int x |> Int64.sign_extend ~bits |> Int64.to_int |> small_obj

let of_int ?(unsigned = false) i =
  let i = Int64.of_int i in
  of_int64
    (if unsigned
    then Int64.logand i (Int64.trailing_mask ~bits:Sys.int_size)
    else i)

let of_nativeint ?(unsigned = false) i =
  let i = Int64.of_nativeint i in
  of_int64
    (if unsigned
    then Int64.logand i (Int64.trailing_mask ~bits:Nativeint.size)
    else i)
