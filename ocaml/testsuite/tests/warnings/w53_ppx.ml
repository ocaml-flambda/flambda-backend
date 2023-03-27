open Ast_mapper

let replace_attr ({ Parsetree.attr_name; _} as attr) =
  { attr with
    attr_name =
      if String.equal attr.attr_name.txt "test" then
        { attr_name with txt = "immediate" }
      else attr_name
  }

let () =
  register "test" (fun _ ->
    { default_mapper with attribute = fun _ attr -> replace_attr attr })
