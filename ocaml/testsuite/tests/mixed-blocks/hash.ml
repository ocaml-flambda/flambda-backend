(* TEST
 flags = "-extension layouts_alpha"
 program = "${test_build_directory}/hash.exe"
all_modules = "hash.ml"

 * flambda2
 ** setup-ocamlc.opt-build-env
 *** ocamlc.opt
 **** run
 ***** check-program-output
   reference = "${test_source_directory}/hash.byte.reference"
 ** setup-ocamlopt.opt-build-env
 *** ocamlopt.opt
 **** run
 ***** check-program-output
   reference = "${test_source_directory}/hash.native.reference"
*)

(* Currently bytecode/native hashes of mixed records are different.
   Mixed records are represented as mixed blocks in native code and
   normal blocks in bytecode. We don't make any special effort to get
   their hash values to line up. This is something we could consider
   revisiting -- the simplest way to accomplish this may well be to
   support mixed blocks in bytecode.
 *)

let hash = Hashtbl.hash
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
  |> printf "\t{ x : float; y : float# } = %d\n"


let () = 
  let open struct
    type t =
      { x : float#;
        y : float;
      }
  end in
  hash { x = #4.0; y = 5.1 }
  |> printf "\t{ x : float#; y : float } = %d\n"

let () = printf "General Mixed Records\n"


let () =
  let open struct
    type t =
      { x : string;
        y : float#;
      }
  end in
  hash { x = "abc"; y = #5.1 }
  |> printf "\t{ x : string; y : float# } = %d\n"

let () =
  let open struct
    type t =
      { x : int;
        y : float#;
      }
  end in
  hash { x = 23940; y = #5.1 }
  |> printf "\t{ x : int; y : float# } = %d\n"

let () =
  let open struct
    type t =
      { x : int;
        y : float#;
        z : int;
      }
  end in
  hash { x = 23940; y = #5.1; z = 1340 }
  |> printf "\t{ x : int; y : float#; z : int } = %d\n"

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
  |> printf "\t{ a : string; x : int; y : float#; z : int } = %d\n"
