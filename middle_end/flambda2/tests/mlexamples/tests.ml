external ( + ) : int -> int -> int = "%addint"

external ( - ) : int -> int -> int = "%subint"

external ( > ) : 'a -> 'a -> bool = "%greaterthan"

external ( < ) : 'a -> 'a -> bool = "%lessthan"

external array_get : 'a array -> int -> 'a = "%array_safe_get"

external array_set : 'a array -> int -> 'a -> unit = "%array_safe_set"

let[@inline always] to_inline _x _y = 42

let f c m n x' y' =
  let x = if c < 0 then x' else x' + 1 in
  let y = if c < 0 then y' else y' + 1 in
  match m with
  | None -> 0
  | Some a -> ( match n with None -> 1 | Some b -> to_inline (x + y) (a + b))
(* let [@inline always] to_inline' x y = x + y

   let f' c m n x' y' = let foo = if c < 0 then x' else x' + 1 in let y = if c <
   0 then y' else y' + 1 in match m with | None -> 0 | Some a -> match n with |
   None -> 1 | Some b -> to_inline' (foo + y) (a + b) (* let rec length_aux len
   = function [] -> len | _::l -> length_aux (len + 1) l

   let length l = (length_aux [@unrolled 3]) 0 l let [@inline always] to_inline'
   x y = x + y

   let foo_length = let len = 100 in let param = [] in match param with [] ->
   len | _::_ -> 42

   *)

   let length_aux len = function [] -> len | _::_ -> 42

   let foo_length = length_aux 444 []

   let n = let rec f x = if x > 0 then 1 + f (x - 1) else 42 in (f [@unrolled
   10]) 5

   module Int32 = struct external add : int32 -> int32 -> int32 = "%int32_add"
   external mul : int32 -> int32 -> int32 = "%int32_mul"

   let succ x = mul (add x 1l) 2l end

   module Int64 = struct external add : int64 -> int64 -> int64 = "%int64_add"
   external mul : int64 -> int64 -> int64 = "%int64_mul"

   let succ x = mul (add x 1L) 2L end type t = | A | B of int | C of int

   let foo () = let x = A in match x with | A -> 0 | B _ -> 1 | C _ -> 2

   let bar () = let x = B 42 in match x with | A -> 0 | B _ -> 1 | C _ -> 2

   let foo2 x = match x with | A -> begin match x with | A -> 100 | B _ -> 500 |
   C _ -> 200 end | B _ -> 1 | C _ -> 2

   let repeated_comparisons x _a = (* let x = x + array_get a 3 in *) if x < 10
   then if x < 10 then 42 else 1000 else 2

   let foo3 arr f i = array_set arr i (f (array_get arr i))

   let f c m n x' y' = let x = if c < 0 then x' else x' + 10 in let y = if c < 0
   then y' else y' + 20 in x + y, 2L *)
