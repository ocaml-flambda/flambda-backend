(* TEST
  * native
    reference = "${test_source_directory}/get_header.opt.reference"
  * bytecode
    reference = "${test_source_directory}/get_header.byte.reference"
*)

external repr : ('a[@local_opt]) -> (Obj.t[@local_opt]) = "%identity"
external get_header_unsafe : (Obj.t[@local_opt]) -> nativeint = "%get_header"
external is_int : (Obj.t[@local_opt]) -> bool = "%obj_is_int"

let get_header (local_ repr) =
  if is_int repr then
    None
  else
    Some (get_header_unsafe repr)

type header = {
  wosize : int;
  color : int;
  tag : int
}

let parse_header : nativeint -> header =
  fun header ->
    let wosize =
      Nativeint.to_int
        (Nativeint.shift_right_logical header 10)
    in

    let color =
      0x3 land
      (Nativeint.to_int (Nativeint.shift_right_logical header 8))
    in

    let tag = 0xff land (Nativeint.to_int header) in

    {wosize; color; tag}

let get_header_parsed repr =
  Option.map parse_header (get_header repr)

let print_header ppf header =
  let {wosize; color; tag} = header in
  Format.fprintf ppf "wosize=%i,color=%i,tag=%i" wosize color tag

let print_maybe_header ppf header =
  match header with
  | None -> Format.fprintf ppf "None"
  | Some header -> Format.fprintf ppf "Some(%a)" print_header header

let is_local repr =
  match get_header_parsed repr with
  | None -> false
  | Some {color; _} -> color = 2

(* immediate *)
let () =
  let x = 42 in
  let rp = repr x in
  Format.printf "%a %a\n" print_maybe_header (get_header_parsed rp)
    Format.pp_print_bool (is_local rp)

(* global*)
let () =
  let s = "hello" in
  let _r = ref s in
  let rp = repr s in
  Format.printf "%a %a\n" print_maybe_header (get_header_parsed rp)
    Format.pp_print_bool (is_local rp)

(* local *)
let foo x =
  let local_ s = ref x in
  let rp = repr s in
  Format.printf "%a %a\n" print_maybe_header (get_header_parsed rp)
    Format.pp_print_bool (is_local rp)

let () = foo 42
