(* TEST
 toplevel;
*)

let local_ foo : string @@ = "hello";;

let local_ foo @ = "hello";;

let local_ foo : 'a. 'a -> 'a @@  = fun x -> x;;

let foo : type a. a -> a @@  = fun x -> x;;

let (x, y) @ = "hello", "world";;

let (x, y) : _ @@ = "hello", "world";;

let foo @ = "hello";;

let foo = ("hello" : _ @@ );;

let foo = ("hello" @ );;

let foo ~bar = bar ^ "hello";;

let x =
  let bar = "world" in
  foo ~(bar : _ @@ )
;;

let x =
  let bar = "world" in
  foo ~(bar @ )
;;

type r = {a : string; b : string};;

let r = {a : _ @@ = "hello";
          b : _ @@ = "world"}
;;

let r =
  {a @  = "hello";
   b @  = "world"}
;;

let foo () =
  let bar = "hello" in
  let biz = "world" in
  ~(bar:_@@), ~(biz:_@@)
;;

let foo () =
  let bar = "hello" in
  let biz = "world" in
  ~(bar @ ), ~(biz @ )
;;

type r = local_ string @  -> unique_ string @ ;;

type r = local_ string * y:string @  -> local_ string * w:string @;;

type r = x:local_ string * y:string @  -> local_ string * w:string @;;

type r = local_ string @ -> string;;

type t = Foo of string @@ * global_ string;;

type r = {
  x : string @@
}
;;

let foo ?(local_ x @ = 42) () = () ;;

let foo ?(local_ x : _ @@ = 42) () = ();;

let foo ?(local_ x : 'a. 'a -> 'a @@ ) = ();;

let foo ?x:(local_ (x,y) @ = (42, 42)) () = ();;

let foo ?x:(local_ (x,y) : _ @@ = (42, 42)) () = ();;

let foo ?x:(local_ (x,y) : 'a.'a->'a @@ ) () = ();;

let foo ((x @ ), (y@)) = x + y ;;

let foo ((x : _ @@ ), (y : _ @@ )) = x + y;;

let foo () =
  let (bar @ ) a b = () in
  bar 42 24;
  ()
;;
