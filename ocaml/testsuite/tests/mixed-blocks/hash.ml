(* TEST
 flags = "-extension layouts_alpha";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
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
