type 'a list =
  | []
  | ( :: ) of ('a[@global]) * 'a list

external ( + ) : int -> int -> int = "%addint"

external ( mod ) : int -> int -> int = "%modint"

external ( = ) : int -> int -> bool = "%eq"

let return_local () = [%local] [1; 2; 3]

let rec map_local ~f:(f [@local]) (l [@local]) =
  [%local] (match l with [] -> [] | a :: l -> f a :: map_local ~f l)

let rec length (l [@local]) = match l with [] -> 0 | _ :: l -> 1 + length l

let () = assert (length [1] = 1)

let () = assert ((length [@unrolled 3]) (return_local ()) = 3)

let () =
  let ns = return_local () in
  let ms = map_local ns ~f:(fun i -> i + 1) in
  assert (length ms = 3)

let rec rev_app (l1 [@local]) l2 =
  match l1 with [] -> l2 | a :: l1 -> rev_app l1 (a :: l2)

let rec spans ~break_here l =
  match l with
  | [] -> []
  | _ ->
    let span, rest = find_span ~break_here l [] in
    rev_app span [] :: spans ~break_here rest

and find_span ~break_here l (acc [@local]) =
  [%local]
    (match l with
    | [] -> acc, []
    | a :: l ->
      let acc = a :: acc in
      if break_here a then acc, l else find_span ~break_here l acc)

let is_even i = i mod 2 = 0

let () =
  let even_spans = spans ~break_here:is_even [0; 1; 2; 3; 4] in
  assert (length even_spans = 3)
