type t =
  | C0
  | C1
  | B0 of int
  | B1 of int

let out _x =
  Sys.opaque_identity ()
[@@inline never] [@@local never]

let emit_instr = function
  | C0 -> out (0, 0)
  | C1 -> out (0, 1)
  | B0 _ -> out (1, 0)
  | B1 _ -> out (1, 1)

let rec emit = function
  | [] -> ()
  | C0 :: B0 k :: rem when Sys.opaque_identity false ->
    out (2, 1); emit rem
  | B1 _ :: rem ->
    out (2, 2); emit rem
  | instr :: rem ->
    emit_instr instr; emit rem
