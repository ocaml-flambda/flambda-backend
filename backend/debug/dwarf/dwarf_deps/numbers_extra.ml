module Int = Numbers.Int
module Float = Numbers.Float

module Int8 = struct
  type t = int

  let zero = 0

  let one = 1

  let of_int_exn i =
    if i < -(1 lsl 7) || i > (1 lsl 7) - 1
    then Misc.fatal_errorf "Int8.of_int_exn: %d is out of range" i
    else i

  let to_int i = i

  let print ppf t = Format.pp_print_int ppf t
end

module Int16 = struct
  type t = int

  let zero = 0

  let one = 1

  let of_int_exn i =
    if i < -(1 lsl 15) || i > (1 lsl 15) - 1
    then Misc.fatal_errorf "Int16.of_int_exn: %d is out of range" i
    else i

  let lower_int64 = Int64.neg (Int64.shift_left Int64.one 15)

  let upper_int64 = Int64.sub (Int64.shift_left Int64.one 15) Int64.one

  let of_int64_exn i =
    if Int64.compare i lower_int64 < 0 || Int64.compare i upper_int64 > 0
    then Misc.fatal_errorf "Int16.of_int64_exn: %Ld is out of range" i
    else Int64.to_int i

  let to_int t = t

  let print ppf t = Format.pp_print_int ppf t
end

module Uint8 = struct
  type t = int

  let print ppf t = Format.pp_print_int ppf t

  let zero = 0

  let one = 1

  let of_nonnegative_int_exn i =
    if i < 0 || i > (1 lsl 8) - 1
    then Misc.fatal_errorf "Uint8.of_nonnegative_int_exn: %d is out of range" i
    else i

  let to_int i = i
end

module Uint16 = struct
  type t = int

  let print ppf t = Format.pp_print_int ppf t

  let of_nonnegative_int_exn i =
    if i < 0 || i > (1 lsl 16) - 1
    then Misc.fatal_errorf "Uint16.of_nonnegative_int_exn: %d is out of range" i
    else i

  let upper_int64 = Int64.sub (Int64.shift_left Int64.one 16) Int64.one

  let of_nonnegative_int64_exn i =
    if Int64.compare i 0L < 0 || Int64.compare i upper_int64 > 0
    then
      Misc.fatal_errorf "Uint16.of_nonnegative_int64_exn: %Ld is out of range" i
    else Int64.to_int i

  let to_int t = t
end

module Uint32 = struct
  type t = Int64.t

  let zero = 0L

  let print ppf t = Format.fprintf ppf "0x%Lx" t

  let upper_int64 = Int64.sub (Int64.shift_left Int64.one 32) Int64.one

  let of_nonnegative_int_exn i =
    if i < 0
    then Misc.fatal_errorf "Uint32.of_nonnegative_int_exn: %d is out of range" i
    else
      let i64 = Int64.of_int i in
      if Int64.compare i64 upper_int64 > 0
      then
        Misc.fatal_errorf "Uint32.of_nonnegative_int_exn: %d is out of range" i
      else i64

  let of_nonnegative_int64_exn i =
    if Int64.compare i 0L < 0 || Int64.compare i upper_int64 > 0
    then
      Misc.fatal_errorf "Uint32.of_nonnegative_int64_exn: %Ld is out of range" i
    else i

  let of_nonnegative_int32_exn i =
    if Int32.compare i 0l < 0
    then
      Misc.fatal_errorf "Uint32.of_nonnegative_int64_exn: %ld is out of range" i
    else Int64.of_int32 i

  let to_int64 t = t
end

module Uint64 = struct
  type t = Int64.t

  let zero = 0L

  let succ t = Int64.add 1L t

  let of_nonnegative_int_exn i =
    if i < 0
    then Misc.fatal_errorf "Uint64.of_nonnegative_int_exn: %d is out of range" i
    else Int64.of_int i

  let of_uint8 i = Int64.of_int i

  let of_uint16 i = Int64.of_int i

  let of_uint32 i = i

  let of_nonnegative_int32_exn i =
    if Int32.compare i 0l < 0
    then
      Misc.fatal_errorf "Uint64.of_nonnegative_int64_exn: %ld is out of range" i
    else Int64.of_int32 i

  let of_nonnegative_int64_exn i =
    if Int64.compare i 0L < 0
    then
      Misc.fatal_errorf "Uint64.of_nonnegative_int64_exn: %Ld is out of range" i
    else i

  let to_int64 t = t

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 = Stdlib.compare t1 t2

    let equal t1 t2 = compare t1 t2 = 0

    let hash t = Hashtbl.hash t

    let print ppf t = Format.fprintf ppf "0x%Lx" t

    let output _ _ = Misc.fatal_error "Not yet implemented"
  end)
end
