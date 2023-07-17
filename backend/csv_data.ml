[@@@ocaml.warning "+a-4-30-40-41-42"]

module type Cell = sig
  type t

  val to_string : t -> string

  val empty : unit -> t
end

(* CR-soon gtulba-lecu for gtulba-lecu: move this to a more fitting place in the
   codebase *)
module Make (C : Cell) = struct
  module Row_contents = Map.Make (String)

  type row = string * C.t Row_contents.t

  type t =
    { column_names : string list;
      mutable rows : row list
    }

  let column_names t = t.column_names

  let rows t = t.rows

  let set_rows t rows = t.rows <- rows

  let empty_row row_name column_names =
    ( row_name,
      List.fold_left
        (fun row column_name -> Row_contents.add column_name (C.empty ()) row)
        Row_contents.empty column_names )

  let add_row t row = t.rows <- row :: t.rows

  let add_empty_row t row_name =
    t.rows <- empty_row row_name t.column_names :: t.rows

  let update_row (row_name, row) column_name (update_func : C.t -> C.t) =
    if Row_contents.exists
         (fun (key : Row_contents.key) (_value : C.t) ->
           String.equal key column_name)
         row
    then row_name, Row_contents.update column_name (Option.map update_func) row
    else (* assert if the column name does not exist *) assert false

  let create column_names = { column_names; rows = [] }

  let print_column_names oc column_names =
    Stdlib.output_string oc "row_name";
    List.iter
      (fun column_name ->
        Stdlib.output_string oc ", ";
        Stdlib.output_string oc column_name)
      column_names;
    Stdlib.output_string oc "\n"

  let print_row oc (row_name, row_contents) column_names =
    Stdlib.output_string oc row_name;
    List.iter
      (fun column_name ->
        Stdlib.output_string oc ", ";
        Stdlib.output_string oc
          (C.to_string (Row_contents.find column_name row_contents)))
      column_names;
    Stdlib.output_string oc "\n"

  let print t file_name =
    let oc = Stdlib.Out_channel.open_text file_name in
    let sorted_column_names = List.sort String.compare t.column_names in
    print_column_names oc sorted_column_names;
    List.iter (fun row -> print_row oc row sorted_column_names) t.rows;
    Stdlib.Out_channel.close oc
end
