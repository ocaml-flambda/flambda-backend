(* TEST
 flags = "-extension layouts_alpha";
 program = "${test_build_directory}/hash.exe";
 all_modules = "hash.ml";
 flambda2;
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run;
   reference = "${test_source_directory}/hash.byte.reference";
   check-program-output;
 }{
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt;
   run;
   reference = "${test_source_directory}/hash.native.reference";
   check-program-output;
 }
*)

(* Currently bytecode/native hashes of mixed records are different.
   Mixed records are represented as mixed blocks in native code and
   normal blocks in bytecode. Hash raises on mixed blocks (but not
   on normal blocks).

   We could consider making native hash return a value instead. But,
   if it's a different value than bytecode, it's important that users
   can't marshal mixed block values from native code to bytecode.
   (Otherwise you could marshal a hashtable, breaking its invariants.)
*)

let hash x =
 match Hashtbl.hash x with
 | exception exn -> Printf.sprintf "raised %s" (Printexc.to_string exn)
 | i -> string_of_int i

let printf = Printf.printf

let () = printf "All Float Mixed Records\n"

let () =
  let open struct
    type t =
      { x : float;
        y : float#;
      }
  end in
  hash { x = 4.0; y = #5.1 }
  |> printf "\t{ x : float; y : float# } = %s\n"


let () =
  let open struct
    type t =
      { x : float#;
        y : float;
      }
  end in
  hash { x = #4.0; y = 5.1 }
  |> printf "\t{ x : float#; y : float } = %s\n"

let () = printf "General Mixed Records\n"


let () =
  let open struct
    type t =
      { x : string;
        y : float#;
      }
  end in
  hash { x = "abc"; y = #5.1 }
  |> printf "\t{ x : string; y : float# } = %s\n"

let () =
  let open struct
    type t =
      { x : int;
        y : float#;
      }
  end in
  hash { x = 23940; y = #5.1 }
  |> printf "\t{ x : int; y : float# } = %s\n"

let () =
  let open struct
    type t =
      { x : int;
        y : float#;
        z : int;
      }
  end in
  hash { x = 23940; y = #5.1; z = 1340 }
  |> printf "\t{ x : int; y : float#; z : int } = %s\n"

let () =
  let open struct
    type t =
      { a : string;
        x : int;
        y : float#;
        z : int;
      }
  end in
  hash { a = "abc"; x = 23940; y = #5.1; z = 1340 }
  |> printf "\t{ a : string; x : int; y : float#; z : int } = %s\n"
