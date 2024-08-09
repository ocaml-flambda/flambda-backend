(* TEST
  flags = "-extension layouts_alpha";
  expect;
*)

(* Perfect binary trees *)
type 'a perfect = Zero of 'a | Succ of ('a * 'a) perfect
[%%expect{|
type 'a perfect = Zero of 'a | Succ of ('a * 'a) perfect
|}]

(* Fold combiners for perfect trees *)
type ('f : value => value) folder = {
  zero : 'a. 'a -> 'a 'f;
  succ : 'a. ('a * 'a) 'f -> 'a 'f
}
[%%expect{|
type ('f : value => value) folder = {
  zero : 'a. 'a -> 'a 'f;
  succ : 'a. ('a * 'a) 'f -> 'a 'f;
}
|}]

(* Fold over perfect tree *)
let rec fold : 'a. 'f folder -> 'a perfect -> 'a 'f = fun folder -> function
| Zero l -> folder.zero l
| Succ p -> folder.succ (fold folder p)
[%%expect{|
val fold : ('f : value => value) 'a. 'f folder -> 'a perfect -> 'a 'f = <fun>
|}]

(* Applicatives *)
type ('t : value => value) applicative = {
  return : 'a. 'a -> 'a 't;
  lift : 'a 'b. ('a -> 'b) -> ('a 't -> 'b 't);
  both : 'a 'b. 'a 't -> 'b 't -> ('a * 'b) 't
}
let lift2 app f x y =
  (app.lift (fun (a, b) -> f a b)) (app.both x y)
[%%expect{|
type ('t : value => value) applicative = {
  return : 'a. 'a -> 'a 't;
  lift : 'a 'b. ('a -> 'b) -> 'a 't -> 'b 't;
  both : 'a 'b. 'a 't -> 'b 't -> ('a * 'b) 't;
}
val lift2 :
  ('a : value => value) 'b 'c 'd.
    'a applicative -> ('b -> 'c -> 'd) -> 'b 'a -> 'c 'a -> 'd 'a =
  <fun>
|}]

(* Identity applicative *)
type 'a id = { v : 'a }
let id = {
  return = (fun v -> { v });
  lift = (fun f { v } -> { v = f v });
  both = (fun { v } { v = v' } -> { v = v, v' })
}
[%%expect{|
type 'a id = { v : 'a; }
val id : id applicative = {return = <fun>; lift = <fun>; both = <fun>}
|}]


(* Correctness tests *)

let example = Succ (Succ (Zero (([0; 1; 2], [4; -1]), ([6; 3], []))))
[%%expect{|
val example : int list perfect =
  Succ (Succ (Zero (([0; 1; 2], [4; -1]), ([6; 3], []))))
|}]

let perfect_id perfect =
  fold {
    zero = (fun x -> Zero x);
    succ = (fun x -> Succ x)
  } perfect
let result = perfect_id example
[%%expect{|
val perfect_id : 'a perfect -> 'a perfect = <fun>
val result : int list perfect =
  Succ (Succ (Zero (([0; 1; 2], [4; -1]), ([6; 3], []))))
|}]

let leftmost perfect =
  fold {
    zero = (fun x -> id.return x);
    succ = (fun x -> id.lift fst x)
  } perfect
let result = leftmost example

[%%expect{|
val leftmost : 'a perfect -> 'a id = <fun>
val result : int list id = {v = [0; 1; 2]}
|}]

let leaves (perfect : 'a perfect) =
  fold {
    zero = (fun l -> [l]);
    succ = (fun l -> List.concat_map (fun (x, y) -> [x; y]) l)
  } perfect
let result = leaves example
let flat = List.concat result

[%%expect{|
val leaves : 'a perfect -> 'a list = <fun>
val result : int list list = [[0; 1; 2]; [4; -1]; [6; 3]; []]
val flat : int list = [0; 1; 2; 4; -1; 6; 3]
|}]
