type 'a t = T
module type T = sig type t end

type r = { x:unit }
let r = { x = () }

type s = S
let s = S
<<<<<<< HEAD

type _ is_int = Is_int : int is_int
||||||| parent of 114ab8b0 (Enable layout histories (#1823))
=======

type u = int
>>>>>>> 114ab8b0 (Enable layout histories (#1823))
