(* TEST
 expect;
*)

module Fn = struct
  let id x = x
end
;;
[%%expect{|
module Fn : sig val id : 'a -> 'a @@ global many portable end
|}]

let f = fun x -> Fn.id x
;;
[%%expect{|
val f : 'a -> 'a @@ global many = <fun>
|}]

let g = Fn.(fun x -> id x)
let h = let open Fn in fun x -> id x
;;
[%%expect{|
val g : 'a -> 'a @@ global many = <fun>
val h : 'a -> 'a @@ global many = <fun>
|}]

let i =
  let open struct
    let id x = x
  end in
  fun x -> id x

let iM =
  let module M = struct
    let id x = x
  end in
  fun x -> M.id x
;;
[%%expect{|
val i : 'a -> 'a @@ global many = <fun>
val iM : 'a -> 'a @@ global many = <fun>
|}]

let j =
  let open struct
    exception E
    let id x = x
  end in
  fun x -> id x

let jM =
  let module M = struct
    exception E
    let id x = x
  end in
  fun x -> M.id x
;;
[%%expect{|
val j : '_weak1 -> '_weak1 @@ global many = <fun>
val jM : '_weak2 -> '_weak2 @@ global many = <fun>
|}]

module Square(X : sig val x : int end) = struct
  let result = X.x * X.x
end
;;
[%%expect{|
module Square :
  functor (X : sig val x : int end) ->
    sig val result : int @@ global many portable end
|}]

let k =
  let open Square(struct let x = 3 end) in
  fun x -> x

let kM =
  let module M = Square(struct let x = 3 end) in
  fun x -> x
;;
[%%expect{|
val k : '_weak3 -> '_weak3 @@ global many = <fun>
val kM : '_weak4 -> '_weak4 @@ global many = <fun>
|}]

let op =
  let module M = struct
      open struct let r = ref [] end
      let s = r
  end in
  M.s
;;
[%%expect{|
val op : '_weak5 list ref @@ global many = {contents = []}
|}]
