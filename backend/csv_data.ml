module type Cell = sig
    type t
    val to_string : t -> string
    val empty : unit -> t
end

(* CR-soon gtulba-lecu for gtulba-lecu: move this to a more fitting place in the codebase *)
module Make(C : Cell) = struct
  module Row_contents = Map.Make(String)

  type row = string * C.t Row_contents.t
  type t = {column_names: string list; mutable rows: row list}

  let column_names t = t.column_names
  let rows t = t.rows

  let empty_row row_name column_names = 
    (row_name, List.fold_left (fun row column_name -> Row_contents.add column_name (C.empty ()) row) Row_contents.empty column_names)

  let add_row t row = t.rows <- row :: t.rows

  let update_row (row_name, row) column_name (update_func: C.t -> C.t) =
    if Row_contents.exists (fun (key: Row_contents.key) (_value: C.t) -> String.equal key column_name) row then 
      (row_name, Row_contents.update column_name (Option.map update_func) row)
    else 
      (* assert if the column name does not exist *)
      assert false

  let create column_names = {column_names; rows = []}

  let row_to_string (row_name, row) =
    row_name :: (Row_contents.fold (fun key value list -> (key, value) :: list) row []
    |> List.sort (fun (key1, _value1) (key2, _value2) -> String.compare key1 key2)
    |> List.map (fun (_key, value) -> value)
    |> List.map C.to_string)
    |> String.concat ", "
    
  let to_string t =
    (String.concat ", " ("row_name" :: (List.sort String.compare t.column_names))) :: (List.map row_to_string t.rows)
    |> String.concat "\n"
end