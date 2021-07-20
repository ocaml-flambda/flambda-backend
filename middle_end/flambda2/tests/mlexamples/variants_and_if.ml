(* Like Typedecl.transl_declaration *)

type t =
  | A
  | B of int

exception Foo

external raise : exn -> 'a = "%raise"

let f x t =
  if x then begin
    match t with
    | A -> raise Foo
    | B _ -> ()
  end;
  match t with
  | A -> 1
  | B _ -> 2
