(* TEST
   * bytecode
     reference = "${test_source_directory}/get_header.heap.reference"
   * stack-allocation
   ** native
      reference = "${test_source_directory}/get_header.stack.reference"
   * no-stack-allocation
   ** native
      reference = "${test_source_directory}/get_header.heap.reference"
 *)

(* We're likely to remove %get_header in favour of calls to
   caml_obj_is_stack under runtime5 (since testing a block's colour isn't
   sufficient to check for local allocations) so this doesn't check for local
   allocations any more. *)

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

(* immediate *)
let () =
  let x = 42 in
  let rp = repr x in
  Format.printf "%a\n" print_maybe_header (get_header_parsed rp)

(* global*)
let () =
  let s = "hello" in
  let _r = ref s in
  let rp = repr s in
  Format.printf "%a\n" print_maybe_header (get_header_parsed rp)
