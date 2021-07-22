let rec iter f = function
    [] -> ()
  | a::l -> f a; iter f l

type 'a ref = { mutable contents : 'a; }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
external raise : exn -> 'a = "%raise"

type ref_and_value = R : 'a ref * 'a -> ref_and_value

let protect_refs =
  let set_refs l = iter (fun (R (r, v)) -> r := v) l in
  fun refs f ->
    set_refs refs;
    f ()

type unification_mode =
  | Expression
  | Pattern

let umode = ref Expression

let[@inline never] set_mode_pattern f =
  protect_refs
    [R (umode, Pattern)] f

let () =
  set_mode_pattern (fun () -> ())
