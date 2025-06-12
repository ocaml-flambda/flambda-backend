(* TEST
   expect;
*)

type ('a : any) t

external read : ('a : any mod external_). 'a t -> 'a = "%peek"
  [@@layout_poly]

external write : ('a : any mod external_). 'a t -> 'a -> unit = "%poke"
  [@@layout_poly]

[%%expect {|
type ('a : any) t
external read : ('a : any mod external_). 'a t -> 'a = "%peek"
  [@@layout_poly]
external write : ('a : any mod external_). 'a t -> 'a -> unit = "%poke"
  [@@layout_poly]
|}]

let bad_read p : string = read p
[%%expect {|
Line 1, characters 26-32:
1 | let bad_read p : string = read p
                              ^^^^^^
Error: Unsupported layout for the peek primitive
|}]

let bad_write p (s : string) = write p s
[%%expect {|
Line 1, characters 31-40:
1 | let bad_write p (s : string) = write p s
                                   ^^^^^^^^^
Error: Unsupported layout for the poke primitive
|}]
