type ('a, 'b) result = Ok of 'a | Error of 'b

external string_length : string -> int = "%string_length"
external string_get : string -> int -> char = "%string_safe_get"
external ( && ) : bool -> bool -> bool = "%sequand"
external ( = ) : 'a -> 'a -> bool = "%equal"
external not : bool -> bool = "%boolnot"

let[@inline always] char_of_string s =
  match string_length s with
  | 1 -> string_get s 0
  | _ -> assert false

let[@inline always] result_is_error r =
  match r with
  | Ok _ -> false
  | Error _ -> true

let[@inline always] result_try_with f =
  try Ok (f ()) with
  | exn -> Error exn

let[@inline always] test () =
  char_of_string "c" = 'c'
  && result_is_error (result_try_with (fun () -> char_of_string ""))

let top () =
  if not (test ()) then assert false
