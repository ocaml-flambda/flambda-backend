(* from dll.ml *)

type 'a ref = { mutable contents : 'a; }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"

let [@inline never] input_line () = "bar"

let ld_conf_contents () =
  let path = ref [] in
  begin try
    begin try
      while true do
        path := input_line () :: !path
      done
    with End_of_file -> ()
    end;
  with Sys_error _ -> ()
  end;
  !path
