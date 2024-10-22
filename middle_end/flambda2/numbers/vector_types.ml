(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2019 OCamlPro SAS                                    *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type Vector_width = sig
  val size_in_int64s : int
end

module Vector_by_bit_pattern (Width : Vector_width) = struct
  module T0 = struct
    type t = Int64.t Array.t

    let rec compare l r i =
      if i = Width.size_in_int64s
      then 0
      else
        let cmp = Int64.compare l.(i) r.(i) in
        if cmp = 0 then compare l r (i + 1) else cmp

    let compare l r = compare l r 0

    let equal = Array.for_all2 Int64.equal

    let hash v = Hashtbl.hash v

    let print ppf t =
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ':')
        (fun ppf i64 -> Format.fprintf ppf "%016Lx" i64)
        ppf (Array.to_list t)
  end

  include T0
  module Self = Container_types.Make (T0)
  include Self

  let zero = Array.init Width.size_in_int64s (fun _ -> 0L)

  let to_int64_array t = t

  let of_int64_array t =
    if not (Array.length t = Width.size_in_int64s)
    then
      Misc.fatal_error
        "Vector_by_bit_pattern.of_int64_array: wrong length array";
    t
end

module Vec128 = struct
  module Bit_pattern = struct
    include Vector_by_bit_pattern (struct
      let size_in_int64s = 2
    end)

    type bits =
      { high : int64;
        low : int64
      }

    let to_bits t =
      match to_int64_array t with
      | [| high; low |] -> { high; low }
      | _ -> Misc.fatal_error "Vec128.to_bits: wrong size vector"

    let of_bits { high; low } = of_int64_array [| high; low |]
  end
end
