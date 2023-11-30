(* TEST
   flags = "-extension layouts_alpha"
   * native
   * bytecode
*)

let unbox = Stdlib__Float_u.of_float

module type S = sig
  type t : any

  val add : t -> t -> t
  val one : unit -> t
  val print : (unit -> t) -> unit
end

module M1 : S with type t = float# = struct
  type t = float#

  let add x y = Stdlib__Float_u.add x y
  let one () = unbox 1.
  let print f = Printf.printf "Printing a float#: %f\n" (Stdlib__Float_u.to_float (f ()))
end

module M2 : S with type t = int = struct
  type t = int

  let add x y = x + y
  let one () = 1
  let print f = Printf.printf "Printing a int:  %d\n" (f ())
end

let () = Printf.printf "%f\n" (Stdlib__Float_u.to_float (M1.add (unbox 10.) (unbox 10.)))
let () = Printf.printf "%d\n" (M2.add 10 10)

module type Q = sig
  include S

  val print_one : unit -> unit
end

module Make (M : S) : Q = struct
  include M

  let print_one () = M.print M.one
end

module M1' = Make (M1)
module M2' = Make (M2)

let () = M1'.print_one ()
let () = M2'.print_one ()
let g : type (a : any). unit -> a -> a = fun () -> assert false

let () =
  try
    let r =
      g
        ()
        (Printf.printf "a\n";
         unbox 10.)
    in
    Printf.printf "%f\n" (Stdlib__Float_u.to_float r)
  with
  | _ -> Printf.printf "b\n"
;;

let () =
  try
    let r =
      g
        ()
        (Printf.printf "c\n";
         10)
    in
    Printf.printf "%d\n" r
  with
  | _ -> Printf.printf "d\n"
;;

(* This should type check *)
let rec f : type (a : any). unit -> a -> a = fun () -> f ()

let f' () =
  let _ = f () 10 in
  f () (unbox 10.)
;;
