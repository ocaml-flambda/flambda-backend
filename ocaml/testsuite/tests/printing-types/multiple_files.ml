(* See [test_multiple_files.ml]. *)

module B = struct
  type t = B1 | B2
end

type _b = B.t = B1 | B2
