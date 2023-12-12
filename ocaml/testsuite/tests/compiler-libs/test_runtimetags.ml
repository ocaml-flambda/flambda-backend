(* TEST
   flags = "-I ${ocamlsrcdir}/utils"
   include ocamlcommon
 * expect
*)

let check_tag name left right =
  match Int.equal left right with
  | true -> Format.printf "values for %S agree@." name
  | false -> Format.printf "values for %S disagree (%d vs %d)@." name left right

let check_tags l =
  List.iter (fun (name, left, right) -> check_tag name left right) l

let () = check_tags [
  "first_non_constant_constructor_tag", Obj.first_non_constant_constructor_tag, Runtimetags.first_non_constant_constructor_tag;
  "last_non_constant_constructor_tag", Obj.last_non_constant_constructor_tag, Runtimetags.last_non_constant_constructor_tag;
  "forcing_tag", Obj.forcing_tag, Runtimetags.forcing_tag;
  "cont_tag", Obj.cont_tag, Runtimetags.cont_tag;
  "lazy_tag", Obj.lazy_tag, Runtimetags.lazy_tag;
  "closure_tag", Obj.closure_tag, Runtimetags.closure_tag;
  "object_tag", Obj.object_tag, Runtimetags.object_tag;
  "infix_tag", Obj.infix_tag, Runtimetags.infix_tag;
  "forward_tag", Obj.forward_tag, Runtimetags.forward_tag;
  "no_scan_tag", Obj.no_scan_tag, Runtimetags.no_scan_tag;
  "abstract_tag", Obj.abstract_tag, Runtimetags.abstract_tag;
  "string_tag", Obj.string_tag, Runtimetags.string_tag;
  "double_tag", Obj.double_tag, Runtimetags.double_tag;
  "double_array_tag", Obj.double_array_tag, Runtimetags.double_array_tag;
  "custom_tag", Obj.custom_tag, Runtimetags.custom_tag;
  "int_tag", Obj.int_tag, Runtimetags.int_tag;
  "out_of_heap_tag", Obj.out_of_heap_tag, Runtimetags.out_of_heap_tag;
  "unaligned_tag", Obj.unaligned_tag, Runtimetags.unaligned_tag;
]

[%%expect{|
val check_tag : string -> int -> int -> unit = <fun>
val check_tags : (string * int * int) list -> unit = <fun>
values for "first_non_constant_constructor_tag" agree
values for "last_non_constant_constructor_tag" agree
values for "forcing_tag" agree
values for "cont_tag" agree
values for "lazy_tag" agree
values for "closure_tag" agree
values for "object_tag" agree
values for "infix_tag" agree
values for "forward_tag" agree
values for "no_scan_tag" agree
values for "abstract_tag" agree
values for "string_tag" agree
values for "double_tag" agree
values for "double_array_tag" agree
values for "custom_tag" agree
values for "int_tag" agree
values for "out_of_heap_tag" agree
values for "unaligned_tag" agree
|}]
