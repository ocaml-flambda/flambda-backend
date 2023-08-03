(* Minimizer rem-def : remove definitions *)

open Utils
open Tast_mapper
open Typedtree

let minimize should_remove map cur_name =
  let remove_def =
    {
      Tast_mapper.default with
      structure =
        (fun _mapper str ->
          {
            str with
            str_items =
              List.filter (fun _ -> not (should_remove ())) str.str_items;
          });
    }
  in
  let nstr = remove_def.structure remove_def (Smap.find cur_name map) in
  Smap.add cur_name nstr map

let minimizer = { minimizer_name = "rem-def"; minimizer_func = minimize }
