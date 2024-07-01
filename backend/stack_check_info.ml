[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR xclerc for xclerc: factor out with zero_alloc equivalent. *)

module String = Misc.Stdlib.String

type value =
  | No_checks
  | Check_as_first_instruction of
      { size_in_bytes : int;
        includes : string list (* CR-soon xclerc for xclerc: for debug/test *)
      }
  | Check_moved_down

let string_of_value = function
  | No_checks -> "no_checks"
  | Check_as_first_instruction { size_in_bytes; includes } ->
    Printf.sprintf "check_as_first_instruction %d bytes%s" size_in_bytes
      (match includes with
      | [] -> ""
      | _ :: _ -> ", includes " ^ String.concat ", " includes)
  | Check_moved_down -> "check_moved_down"

(* CR xclerc for xclerc: rather use a Tbl.t? *)
type t = { mutable stack_check : value String.Map.t }

let create () = { stack_check = String.Map.empty }

let reset t = t.stack_check <- String.Map.empty

let merge src ~into:dst =
  dst.stack_check
    <- String.Map.union
         (fun key -> Misc.fatal_errorf "duplicate symbol %s" key)
         dst.stack_check src.stack_check

let get_value t name = String.Map.find_opt name t.stack_check

let set_value t name value =
  t.stack_check
    <- String.Map.update name
         (function
           | Some _ -> Misc.fatal_errorf "duplicate symbol %s" name
           | None -> Some value)
         t.stack_check

module Raw = struct
  (* CR xclerc for xclerc; rather use an array? *)
  type entries = (string * value) list

  type t = entries option

  let print t =
    Printf.printf "Stack checks:\n";
    match t with
    | None -> ()
    | Some (entries : entries) ->
      List.iter
        (fun (name, value) ->
          Printf.printf "\t\t%s = %s\n" name (string_of_value value))
        entries
end

(* CR xclerc for xclerc: do what the zero_alloc equivalent does, and encode the
   occurrences of `value` as mere `int`s. *)

let to_raw t =
  if String.Map.is_empty t.stack_check
  then None
  else Some (String.Map.bindings t.stack_check)

let of_raw raw =
  match raw with
  | None -> create ()
  | Some entries ->
    { stack_check = entries |> List.to_seq |> String.Map.of_seq }
