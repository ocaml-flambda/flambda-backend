type 'a ref = { mutable contents : 'a; }

external opaque : 'a -> 'a = "%opaque"
external ref : 'a -> 'a ref = "%makemutable"
external (!) : 'a ref -> 'a = "%field0"
external (:=) : 'a ref -> 'a -> unit = "%setfield0"

let id x = x

let g y =
  let rec p = function
    | [] -> opaque []
    | hd :: tl -> (opaque id) tl
  in
  p y

let f x =
  let c = ref x in
  begin try
    while true do
      c := g !c
    done
  with _ -> ()
  end;
  !c
