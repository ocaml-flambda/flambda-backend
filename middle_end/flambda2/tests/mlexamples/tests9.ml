external ( <> ) : 'a -> 'a -> bool = "%notequal"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external compare : 'a -> 'a -> int = "%compare"
external ( = ) : 'a -> 'a -> bool = "%equal"
external ( asr ) : int -> int -> int = "%asrint"
external ( - ) : int -> int -> int = "%subint"
external ( + ) : int -> int -> int = "%addint"

let rec length_aux len = function
    [] -> len
  | _::l -> length_aux (len + 1) l

let length l = length_aux 0 l

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> rev_append l (a :: l2)

let rec chop k l =
  if k = 0 then l else begin
    match l with
    | _::t -> chop (k-1) t
    | _ -> assert false
  end

let sort_uniq cmp l =
  let rec rev_merge l1 l2 accu =
    match l1, l2 with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1::t1, h2::t2 ->
        let c = cmp h1 h2 in
        if c = 0 then rev_merge t1 t2 (h1::accu)
        else if c < 0
        then rev_merge t1 l2 (h1::accu)
        else rev_merge l1 t2 (h2::accu)
  in
  let rec rev_merge_rev l1 l2 accu =
    match l1, l2 with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1::t1, h2::t2 ->
        let c = cmp h1 h2 in
        if c = 0 then rev_merge_rev t1 t2 (h1::accu)
        else if c > 0
        then rev_merge_rev t1 l2 (h1::accu)
        else rev_merge_rev l1 t2 (h2::accu)
  in
  let rec sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ ->
       let c = cmp x1 x2 in
       if c = 0 then [x1]
       else if c < 0 then [x1; x2] else [x2; x1]
    | 3, x1 :: x2 :: x3 :: _ ->
       let c = cmp x1 x2 in
       if c = 0 then begin
         let c = cmp x2 x3 in
         if c = 0 then [x2]
         else if c < 0 then [x2; x3] else [x3; x2]
       end else if c < 0 then begin
         let c = cmp x2 x3 in
         if c = 0 then [x1; x2]
         else if c < 0 then [x1; x2; x3]
         else let c = cmp x1 x3 in
         if c = 0 then [x1; x2]
         else if c < 0 then [x1; x3; x2]
         else [x3; x1; x2]
       end else begin
         let c = cmp x1 x3 in
         if c = 0 then [x2; x1]
         else if c < 0 then [x2; x1; x3]
         else let c = cmp x2 x3 in
         if c = 0 then [x2; x1]
         else if c < 0 then [x2; x3; x1]
         else [x3; x2; x1]
       end
    | n, l ->
       let n1 = n asr 1 in
       let n2 = n - n1 in
       let l2 = chop n1 l in
       let s1 = rev_sort n1 l in
       let s2 = rev_sort n2 l2 in
       rev_merge_rev s1 s2 []
  and rev_sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ ->
       let c = cmp x1 x2 in
       if c = 0 then [x1]
       else if c > 0 then [x1; x2] else [x2; x1]
    | 3, x1 :: x2 :: x3 :: _ ->
       let c = cmp x1 x2 in
       if c = 0 then begin
         let c = cmp x2 x3 in
         if c = 0 then [x2]
         else if c > 0 then [x2; x3] else [x3; x2]
       end else if c > 0 then begin
         let c = cmp x2 x3 in
         if c = 0 then [x1; x2]
         else if c > 0 then [x1; x2; x3]
         else let c = cmp x1 x3 in
         if c = 0 then [x1; x2]
         else if c > 0 then [x1; x3; x2]
         else [x3; x1; x2]
       end else begin
         let c = cmp x1 x3 in
         if c = 0 then [x2; x1]
         else if c > 0 then [x2; x1; x3]
         else let c = cmp x2 x3 in
         if c = 0 then [x2; x1]
         else if c > 0 then [x2; x3; x1]
         else [x3; x2; x1]
       end
    | n, l ->
       let n1 = n asr 1 in
       let n2 = n - n1 in
       let l2 = chop n1 l in
       let s1 = sort n1 l in
       let s2 = sort n2 l2 in
       rev_merge s1 s2 []
  in
  let len = length l in
  if len < 2 then l else sort len l

let foo cmp l =
  let rec sort n l = rev_sort n l
  and rev_sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ ->
       let c = cmp x1 x2 in
       if c < 0 then [x1]
       else [x2]
    | n, l ->
       let s2 = sort n l in
       s2
  in
  sort 42 l
