let (f @ portable) () =
  let module Monoid_utils_of_list_monoid =
    Monoid_utils(Monoid)(List_monoid) [@jane.non_erasable.instances]
  in
  (* module alias doesn't walk locks; using it does. *)
  let _ = Monoid_utils_of_list_monoid.concat in
  ()
