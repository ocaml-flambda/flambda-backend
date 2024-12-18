(* CR jvanburen: copyright header *)

let ( land ) = Nativeint.logand

let ( lor ) = Nativeint.logor

let ( lxor ) = Nativeint.logxor

let lnot = Nativeint.lognot

let num_bits = 8 * Arch.size_int

let max_shift = num_bits - 1

let trailing_mask ~bits =
  assert (0 <= bits && bits <= num_bits);
  if bits = Sys.word_size
  then -1n
  else Nativeint.pred (Nativeint.shift_left 1n bits)

let leading_mask ~bits =
  if bits = 0
  then 0n
  else Nativeint.shift_left (trailing_mask ~bits) (num_bits - bits)

let count_leading_zeros =
  let rec go n ~acc ~num_bits =
    if num_bits = 1
    then acc + (1 - Nativeint.to_int n)
    else
      let right = num_bits / 2 in
      let left = num_bits - right in
      let left_mask = Nativeint.shift_left (-1n) right in
      if n land left_mask = 0n
      then go n ~acc:(acc + left) ~num_bits:right
      else
        let n = Nativeint.shift_right_logical n right in
        go n ~acc ~num_bits:left
  in
  fun n -> go (n land trailing_mask ~bits:num_bits) ~acc:0 ~num_bits

let count_trailing_zeros =
  let rec go n ~acc ~num_bits =
    if n = 0n
    then acc + num_bits
    else if num_bits = 1
    then acc
    else
      let right = num_bits / 2 in
      let left = num_bits - right in
      let right_mask = Nativeint.pred (Nativeint.shift_left 1n right) in
      if n land right_mask <> 0n
      then go n ~acc ~num_bits:right
      else
        let n = Nativeint.shift_right_logical n right in
        go n ~acc:(acc + right) ~num_bits:left
  in
  fun n -> go n ~acc:0 ~num_bits

let count_leading_ones x = count_leading_zeros (lnot x)

let sign_bit bits ~known =
  let sign_bit = Nativeint.shift_left 1n num_bits in
  if known land sign_bit = 0n then None else Some (bits land sign_bit <> 0n)

type t =
  { bits : Nativeint.t;  (** known bits of the number (see [mask]) *)
    known : Nativeint.t;  (** a mask of which bits are known *)
    sign_bits : int
        (** how many leading bits are known to be the same, even if it's not known what they
          are. *)
  }

let invariant { bits; known; sign_bits } =
  let unknown_bits_unset = bits land known in
  let unknown_bits_set = bits lor lnot known in
  let min_possible_sign_bits =
    match sign_bit bits ~known with
    | None -> 1
    | Some true -> count_leading_ones unknown_bits_unset
    | Some false -> count_leading_zeros unknown_bits_set
  in
  let max_possible_sign_bits =
    let max_sign_bits_if_sign_is_unset () =
      count_leading_zeros unknown_bits_unset
    in
    let max_sign_bits_if_sign_is_set () = count_leading_ones unknown_bits_set in
    match sign_bit bits ~known with
    | Some true -> max_sign_bits_if_sign_is_set ()
    | Some false -> max_sign_bits_if_sign_is_unset ()
    | None ->
      Int.max
        (max_sign_bits_if_sign_is_set ())
        (max_sign_bits_if_sign_is_unset ())
  in
  assert (bits = unknown_bits_unset);
  if min_possible_sign_bits > sign_bits
  then
    Misc.fatal_errorf
      "sign_bits = %d, min_possible_sign_bits = %d, bits = %nx, known = %nx"
      sign_bits min_possible_sign_bits bits known;
  if max_possible_sign_bits < sign_bits
  then
    Misc.fatal_errorf
      "sign_bits = %d, max_possible_sign_bits = %d, bits = %nx, known = %nx"
      sign_bits max_possible_sign_bits bits known;
  assert (
    min_possible_sign_bits <= sign_bits && sign_bits <= max_possible_sign_bits)

let create ?(sign_bits = 1) bits ~known =
  let min_sign_bits =
    match sign_bit bits ~known with
    | None -> 1
    | Some false -> count_leading_zeros (bits lor lnot known)
    | Some true -> count_leading_ones (bits land known)
  in
  let sign_bits = Int.max sign_bits min_sign_bits in
  let t = { sign_bits; bits; known } in
  invariant t;
  t

let sign_bits t = t.sign_bits

let unknown_bits_set t = t.bits lor lnot t.known

let is_power_of_2 const = const land Nativeint.pred const = 0n

let leading_zeros t = count_leading_zeros (unknown_bits_set t)

let leading_ones t = count_leading_zeros (lnot (unknown_bits_set t))

let join t1 t2 =
  let known = t1.known land t2.known land lnot (t1.bits lxor t2.bits) in
  let t =
    { bits = t1.bits lor t2.bits land known;
      known;
      sign_bits = Int.min t1.sign_bits t2.sign_bits
    }
  in
  invariant t;
  t

let top = { sign_bits = 1; bits = 0n; known = 0n }

let is_top t = t.known = 0n

let constant n = create n ~known:(-1n)

let range ~min ~max =
  assert (min <= max);
  join (constant (Nativeint.of_int min)) (constant (Nativeint.of_int max))

let to_constant t = if t.known = -1n then Some t.bits else None

let to_small_int ~min ~max n =
  if Nativeint.of_int min <= n && n <= Nativeint.of_int max
  then Some (Nativeint.to_int n)
  else None

let to_small_int t ~min ~max =
  Option.bind (to_constant t) (to_small_int ~min ~max)

let can_weaken_add_to_or t1 t2 =
  (* we can weaken an [add] to an [or] if we know there's no overflow from
     unknown bits *)
  Nativeint.add (unknown_bits_set t1) (unknown_bits_set t2)
  = unknown_bits_set t1 lor unknown_bits_set t2

let can_weaken_mul t1 t2 =
  match to_constant t1, to_constant t2 with
  | Some t1, Some t2 -> `Constant (Nativeint.mul t1 t2)
  | None, None -> `Unknown
  | Some 0n, None | None, Some 0n -> `Constant 0n
  | None, Some t2 ->
    if is_power_of_2 t2
    then `Shift_LHS_left_by (count_trailing_zeros t2)
    else `Unknown
  | Some t1, None ->
    if is_power_of_2 t1
    then `Shift_RHS_left_by (count_trailing_zeros t1)
    else `Unknown

let logor t1 t2 =
  let bits = t1.bits lor t2.bits in
  let sign_bits = Int.min t1.sign_bits t2.sign_bits in
  create ~sign_bits bits ~known:(bits lor (t1.known land t2.known))

let logand t1 t2 =
  let bits = t1.bits land t2.bits in
  let known_to_be_zero =
    lnot t1.bits land t1.known lor (lnot t2.bits land t2.known)
  in
  let sign_bits = Int.min t1.sign_bits t2.sign_bits in
  create ~sign_bits bits ~known:(known_to_be_zero lor (t1.known land t2.known))

let logxor t1 t2 =
  let bits = t1.bits lxor t2.bits in
  let sign_bits = Int.min t1.sign_bits t2.sign_bits in
  create ~sign_bits bits ~known:(t1.known land t2.known)

let shift_right_logical t1 t2 =
  match to_small_int t2 ~min:0 ~max:max_shift with
  | None -> top
  | Some 0 -> t1
  | Some shift ->
    create
      (Nativeint.shift_right_logical t1.bits shift)
      ~known:
        (leading_mask ~bits:shift
        lor Nativeint.shift_right_logical t1.known shift)

let shift_left t1 t2 =
  match to_small_int t2 ~min:0 ~max:max_shift with
  | None -> top
  | Some 0 -> t1
  | Some shift ->
    create
      (Nativeint.shift_left t1.bits shift)
      ~known:(Nativeint.shift_left t1.known shift lor trailing_mask ~bits:shift)

let shift_right t1 t2 =
  match to_small_int t2 ~min:0 ~max:max_shift with
  | None -> top
  | Some 0 -> t1
  | Some shift ->
    let ( asr ) x y =
      let unused_bits = Sys.word_size - num_bits in
      let x = Nativeint.shift_left x unused_bits in
      let x = Nativeint.shift_right x y in
      let x = Nativeint.shift_right_logical x unused_bits in
      x
    in
    create (t1.bits asr shift) ~known:(t1.known asr shift)

let add t1 t2 =
  match to_constant t1, to_constant t2 with
  | Some t1, Some t2 -> constant (Nativeint.add t1 t2)
  | Some 0n, None -> t2
  | None, Some 0n -> t1
  | _, _ ->
    if can_weaken_add_to_or t1 t2
    then logor t1 t2
    else
      { top with sign_bits = Int.max 1 (Int.min t1.sign_bits t2.sign_bits - 1) }

let sign_bit t = sign_bit t.bits ~known:t.known
