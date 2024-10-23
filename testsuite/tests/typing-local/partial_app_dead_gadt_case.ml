(* TEST
   {
     stack-allocation;
     ocamlopt_flags = "-Oclassic";
     native;
   }{
     stack-allocation;
     ocamlopt_flags = "-O3";
     native;
   }
*)

type (_,_) eq = Refl : ('a, 'a) eq

let apply (type a) (equ : (a, unit -> unit) eq option) (f : a) =
  match equ with
  | None -> ()
  | Some Refl -> f ()

let fn (local_ x) (local_ y) = ()

let bad p = apply p fn

(* More minimal example: *)

let cast (type a b) (Refl : (a, b) eq) (f : a) : b = f

let fn (local_ x) (local_ y) = ()
let bad2 p = cast p fn ()
