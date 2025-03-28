
let __dummy1__ _ = assert false[@@inline never ]
external __dummy2__ : unit -> 'a = "%opaque"
external __ignore__ : 'a -> unit = "%ignore"
external __opaque__ : 'a -> 'a = "%opaque"

exception Aliased_node

let rec module_type_elements ?(trans=true) (mt : Odoc_module.t_module_type) =
  let rec iter_kind x =
    match (x : Odoc_module.module_type_kind option) with
    | None
    | Some (Module_type_typeof _)
    | Some (Module_type_functor _)
    | Some (Module_type_alias _)
    | Some (Module_type_struct _)
      -> []
    | Some (Module_type_with (k, _)) -> iter_kind (Some k)
  in
  iter_kind mt.mt_kind

let foo (mt : Odoc_module.t_module_type) =
  try
    if __opaque__ false then ();
    module_type_elements ~trans:false mt
  with | Aliased_node -> []

