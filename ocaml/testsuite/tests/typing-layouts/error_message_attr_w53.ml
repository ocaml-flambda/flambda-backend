(* TEST

flags = "-w +A-60-70 -extension layouts"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output

*)

(* CR layouts v1.5: move this to [warnings/w53.ml] when layout annotation is generally avaliable
   without the layouts extension flag. *)
module type TestErrorMessageSig = sig
  val f : int ->
    (int as ('a:value)[@error_message ""][@error_message ""]) (* reject second *)
end

module TestErrorMessageStruct = struct
  let f1 v: ((_ : value)[@error_message ""][@error_message ""]) = v (* reject second *)
  let f2 v: (('a : value)[@error_message ""][@error_message ""]) = v (* reject second *)
end
