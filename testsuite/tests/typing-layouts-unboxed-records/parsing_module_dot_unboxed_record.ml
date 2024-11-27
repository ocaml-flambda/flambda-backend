(* TEST
   flags = "-extension-universe beta";
   setup-ocamlc.byte-build-env;
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

(* CR layouts v7.2: These should parse. *)

module M = struct
  type t = #{ i : int }
end

let t = M.#{ i = 1 }

let M.#{ i } = #{ i = 1 }
