(* TEST
   flags += "-extension-universe alpha";
   expect;
   reference = "${test_source_directory}/rbtree.byte.reference";
*)

(* CR uniqueness: To run this test replace 'expect' above by 'native'
   and delete the expect block. *)

type color = Red | Black

type ('k, 'v) tree =
  | Node of { color : color;
              left : ('k, 'v) tree;
              key : 'k @@ aliased many;
              value : 'v @@ aliased many;
              right : ('k, 'v) tree }
  | Leaf

let rec fold f b t =
  match t with
  | Node { left; key; value; right } ->
      fold f (f key value (fold f b left)) right
  | Leaf -> b

let work ~insert ~fold ~empty =
  let rec loop n t =
    if n <= 0 then t
    else loop (n - 1) (insert n (n mod 10 = 0) t)
  in
  let tree = loop 42000 empty in
  fold (fun _ v acc -> if v then acc + 1 else acc) 0 tree

(*************************************************************************
  Okasaki-style red-black trees
 *************************************************************************)

module Make_Okasaki(Ord : Map.OrderedType) = struct
  type 'a t = (Ord.t, 'a) tree

  let fold = fold

  (* t is black and its left child is red *)
  let balance_left t =
    match t with
    | Node t ->
        begin match t.left with
        | Node ({ left = Node ({ color = Red } as ll) } as l) ->
            Node { l with color = Red;
                          left = Node { ll with color = Black };
                          right = Node { t with color = Black; left = l.right } }
        | Node ({ right = Node ({ color = Red } as lr) } as l) ->
            Node { lr with color = Red;
                          left = Node { l with color = Black; right = lr.left };
                          right = Node { t with color = Black; left = lr.right } }
        | Node l ->
            Node { t with color = Black; left = Node { l with color = Red } }
        | Leaf -> assert false
        end
    | Leaf -> assert false

  (* t is black and its right child is red *)
  let balance_right t =
    match t with
    | Node t ->
        begin match t.right with
        | Node ({ right = Node ({ color = Red } as rr) } as r) ->
            Node { r with color = Red;
                          left = Node { t with color = Black; right = r.left };
                          right = Node { rr with color = Black } }
        | Node ({ left = Node ({ color = Red } as rl) } as r) ->
            Node { rl with color = Red;
                          left = Node { t with color = Black; right = rl.left };
                          right = Node { r with color = Black; left = rl.right } }
        | Node r ->
            Node { t with color = Black; right = Node { r with color = Red } }
        | Leaf -> assert false
        end
    | Leaf -> assert false

  let[@tail_mod_cons] rec ins k v t =
    match t with
    | Leaf -> Node { color = Red; left = Leaf; key = k; value = v; right = Leaf }
    | Node t ->
        match Ord.compare k t.key with
        | c when c < 0 -> begin
            match t.left with
            | Node { color = Red } ->
                balance_left (Node { t with left = ins k v t.left }) [@nontail]
            | _ -> Node { t with left = ins k v t.left } end
        | c when c > 0 -> begin
            match t.right with
            | Node { color = Red } ->
                balance_right (Node { t with right = ins k v t.right }) [@nontail]
            | _ -> Node { t with right = ins k v t.right } end
        | _ (* k == t.key *) -> Node { t with value = v }

  let set_black t =
    match t with
    | Node { color = Black } -> t
    | Node t -> Node { t with color = Black }
    | Leaf -> Leaf

  let insert k v t = set_black (ins k v t)
end


(*************************************************************************
   Okasaki-style red-black trees with uniqueness
 *************************************************************************)

module Make_Unique_Okasaki(Ord : Map.OrderedType) = struct
  type 'a t = (Ord.t, 'a) tree

  let fold = fold

  (* t is black and its left child is red *)
  let balance_left t =
    match t with
    | Node t ->
        begin match t.left with
        | Node { left = Node { color = Red } as ll } as l ->
            overwrite_ l with
              Node { color = Red;
                     left  = overwrite_ ll with Node { color = Black };
                     right = overwrite_ t  with Node { color = Black; left = l.right } }
        | Node { right = Node { color = Red } as lr } as l ->
            overwrite_ lr with
              Node { color = Red;
                     left  = overwrite_ l with Node { color = Black; right = lr.left };
                     right = overwrite_ t with Node { color = Black; left = lr.right } }
        | Node l ->
            overwrite_ t with
              Node { color = Black; left = overwrite_ l with Node { color = Red } }
        | Leaf -> assert false
        end
    | Leaf -> assert false

  (* t is black and its right child is red *)
  let balance_right t =
    match t with
    | Node t ->
        begin match t.right with
        | Node { right = Node { color = Red } as rr } as r ->
            overwrite_ r with
              Node { color = Red;
                     left  = overwrite_ t  with Node { color = Black; right = r.left };
                     right = overwrite_ rr with Node { color = Black } }
        | Node { left = Node { color = Red } as rl } as r ->
            overwrite_ rl with
              Node { color = Red;
                     left  = overwrite_ t with Node { color = Black; right = rl.left };
                     right = overwrite_ r with Node { color = Black; left = rl.right } }
        | Node r ->
            overwrite_ t with
              Node { color = Black; right = overwrite_ r with Node { color = Red } }
        | Leaf -> assert false
        end
    | Leaf -> assert false

  let[@tail_mod_cons] rec ins k v t =
    match t with
    | Leaf -> Node { color = Red; left = Leaf; key = k; value = v; right = Leaf }
    | Node t ->
        match Ord.compare k t.key with
        | c when c < 0 -> begin
            match t.left with
            | Node { color = Red } ->
                balance_left (overwrite_ t with Node { left = ins k v t.left }) [@nontail]
            | _ -> overwrite_ t with Node { left = ins k v t.left } end
        | c when c > 0 -> begin
            match t.right with
            | Node { color = Red } ->
                balance_right (overwrite_ t with Node { right = ins k v t.right }) [@nontail]
            | _ -> overwrite_ t with Node { right = ins k v t.right } end
        | _ (* k == t.key *) -> overwrite_ t with Node { value = v }

  let set_black t =
    match t with
    | Node _ as t -> overwrite_ t with Node { color = Black }
    | Leaf -> assert false

  let insert k v t = set_black (ins k v t)
end

(*************************************************************************
   Tree with explicit tags
 *************************************************************************)

type tree_tag = private Tree
type zipper_tag = private Zipper

type ('left, 'right, 'kind) tag =
  | Red : (tree_tag, tree_tag, tree_tag) tag
  | Black : (tree_tag, tree_tag, tree_tag) tag
  | Right_red : (tree_tag, zipper_tag, zipper_tag) tag
  | Right_black : (tree_tag, zipper_tag, zipper_tag) tag
  | Left_red : (zipper_tag, tree_tag, zipper_tag) tag
  | Left_black : (zipper_tag, tree_tag, zipper_tag) tag

type ('k, 'v, 'kind) tagged_tree =
  | Node : { color : ('left, 'right, 'kind) tag;
             left : ('k, 'v, 'left) tagged_tree;
             key : 'k;
             value : 'v;
             right : ('k, 'v, 'right) tagged_tree }
      -> ('k, 'v, 'kind) tagged_tree
  | Leaf : ('k, 'v, 'kind) tagged_tree

let rec tagged_fold : 'k 'v 'kind 'a. ('k -> 'v -> 'a -> 'a) -> 'a -> ('k, 'v, 'kind) tagged_tree -> 'a = fun f a t ->
  match t with
  | Node { left; key; value; right } ->
      tagged_fold f (f key value (tagged_fold f a left)) right
  | Leaf -> a

let unique_work ~insert ~fold ~empty =
  let rec loop n (t) =
    if n <= 0 then t
    else loop (n - 1) (insert n (n mod 10 = 0) t) in
  let tree = loop 42000 empty in
  fold (fun _ v acc -> if v then acc + 1 else acc) 0 tree


(*************************************************************************
   Okasaki-style red-black trees with uniqueness and explicit tags
 *************************************************************************)
module Make_Tagged_Okasaki(Ord : Map.OrderedType) = struct
  type 'a t = (Ord.t, 'a, tree_tag) tagged_tree

  let fold = tagged_fold

  (* t is black and its left child is red *)
  let balance_left t =
    match t with
    | Node ({ color = Black } as t) ->
        begin match t.left with
        | Node { color = Red; left = Node { color = Red } as ll } as l ->
            overwrite_ l with
              Node { color = Red;
                     left  = overwrite_ ll with Node { color = Black };
                     right = overwrite_ t  with Node { color = Black; left = l.right } }
        | Node { color = Red; right = Node { color = Red } as lr } as l ->
            overwrite_ lr with
              Node { color = Red;
                     left  = overwrite_ l with Node { color = Black; right = lr.left };
                     right = overwrite_ t with Node { color = Black; left = lr.right } }
        | Node { color = Red } as l ->
            overwrite_ t with
              Node { color = Black; left = overwrite_ l with Node { color = Red } }
        | _ -> assert false
        end
    | _ -> assert false

  (* t is black and its right child is red *)
  let balance_right t =
    match t with
    | Node ({ color = Black } as t) ->
        begin match t.right with
        | Node { color = Red; right = Node { color = Red } as rr } as r ->
            overwrite_ r with
              Node { color = Red;
                     left  = overwrite_ t  with Node { color = Black; right = r.left };
                     right = overwrite_ rr with Node { color = Black } }
        | Node { color = Red; left = Node { color = Red } as rl } as r ->
            overwrite_ rl with
              Node { color = Red;
                     left  = overwrite_ t with Node { color = Black; right = rl.left };
                     right = overwrite_ r with Node { color = Black; left = rl.right } }
        | Node { color = Red } as r ->
            overwrite_ t with
              Node { color = Black; right = overwrite_ r with Node { color = Red } }
        | _ -> assert false
        end
    | _ -> assert false

  let rec ins : 'a. Ord.t -> 'a -> 'a t -> 'a t = fun k v t ->
    match t with
    | Node { color = Black } as t -> begin
        match Ord.compare k t.key with
        | c when c < 0 -> begin
          match t.left with
          | Node { color = Red } ->
              balance_left (overwrite_ t with Node { left = ins k v t.left })
          | _ -> overwrite_ t with Node { left = ins k v t.left } end
        | c when c > 0 -> begin
          match t.right with
          | Node { color = Red } ->
            balance_right (overwrite_ t with Node { right = ins k v t.right })
          | _ -> overwrite_ t with Node { right = ins k v t.right } end
        | _ (* k == t.key *) -> overwrite_ t with Node { value = v } end
    (* next case exactly as above; copied due to GADT inference *)
    | Node { color = Red } as t -> begin
        match Ord.compare k t.key with
        | c when c < 0 -> begin
            match t.left with
            | Node { color = Red } ->
              balance_left (overwrite_ t with Node { left = ins k v t.left })
        | _ -> overwrite_ t with Node { left = ins k v t.left } end
        | c when c > 0 -> begin
          match t.right with
          | Node { color = Red } ->
            balance_right (overwrite_ t with Node { right = ins k v t.right })
        | _ -> overwrite_ t with Node { right = ins k v t.right } end
        | _ (* k == t.key *) -> overwrite_ t with Node { value = v } end
    | Leaf -> Node { color = Red; left = Leaf; key = k; value = v; right = Leaf }

  let set_black t =
    match t with
    | Node { color = Red } as t -> overwrite_ t with Node { color = Black }
    | t -> t

  let insert k v t = set_black (ins k v t)
end

(*************************************************************************
   Bottom-up red-black trees
 *************************************************************************)

module Make_Tagged_Bottom_Up(Ord : Map.OrderedType) = struct
  type 'a t = (Ord.t, 'a, tree_tag) tagged_tree
  type 'a z = (Ord.t, 'a, zipper_tag) tagged_tree

  let fold = tagged_fold

  let rec move_up : 'a. 'a z -> 'a t -> 'a t = fun z t ->
    match z with
    | Node { color = Left_red; left = z' } as z ->
        move_up z' (overwrite_ z with Node { color = Red; left = t })
    | Node { color = Left_black; left = z' } as z ->
        move_up z' (overwrite_ z with Node { color = Black; left = t })
    | Node { color = Right_red; right = z' } as z ->
        move_up z' (overwrite_ z with Node { color = Red; right = t })
    | Node { color = Right_black; right = z' } as z ->
        move_up z' (overwrite_ z with Node { color = Black; right = t })
    | Leaf -> t

  let rec fixup : 'a. 'a z -> 'a t -> 'a t = fun z t ->
    match z with
    | Node { color = Left_red; left = z'; right = zright } as z -> begin
        match z' with
        | Node { color = Left_black; left = z''; right = y } as z' -> begin
            match y with
            | Node { color = Red } as y ->
                fixup z'' (overwrite_ z' with
                  Node { color = Red;
                         left  = overwrite_ z with Node { color = Black; left = t };
                         right = overwrite_ y with Node { color = Black } })
            | Node { color = Black } | Leaf ->
                move_up z'' (overwrite_ z with
                  Node { color = Black;
                         right = overwrite_ z' with Node { color = Red; left = zright };
                         left  = t }) end
        | Node { color = Right_black; right = z''; left = y } as z' -> begin
            match y with
            | Node { color = Red } as y ->
                fixup z'' (overwrite_ z' with
                  Node { color = Red;
                         right = overwrite_ z with Node { color = Black; left = t };
                         left  = overwrite_ y with Node { color = Black } })
            | Node { color = Black } | Leaf ->
                match t with
                | Node ({ color = Red; left = tl; right = tr } as t) ->
                    move_up z'' (overwrite_ t with
                      Node { color = Black;
                             left  = overwrite_ z' with Node { color = Red; right = tl };
                             right = overwrite_ z  with Node { color = Red; left = tr } })
                 | _ -> assert false end
        | _ -> assert false end
    | Node { color = Right_red; right = z'; left = zleft } as z -> begin
        match z' with
        | Node { color = Right_black; right = z''; left = y } as z' -> begin
            match y with
            | Node { color = Red } as y ->
                fixup z'' (overwrite_ z' with
                  Node { color = Red;
                         right = overwrite_ z with Node { color = Black; right = t };
                         left  = overwrite_ y with Node { color = Black } })
            | Node { color = Black } | Leaf ->
                move_up z'' (overwrite_ z with
                  Node { color = Black;
                         left  = overwrite_ z' with Node { color = Red; right = zleft };
                         right = t }) end
        | Node { color = Left_black; left = z''; right = y } as z' -> begin
            match y with
            | Node { color = Red } as y ->
                fixup z'' (overwrite_ z' with
                  Node { color = Red;
                         left  = overwrite_ z with Node { color = Black; right = t };
                         right = overwrite_ y with Node { color = Black } })
            | Node { color = Black } | Leaf ->
                match t with
                | Node { color = Red; left = tl; right = tr } as t ->
                    move_up z'' (overwrite_ t with
                      Node { color = Black;
                             left  = overwrite_ z  with Node { color = Red; right = tl };
                             right = overwrite_ z' with Node { color = Red; left = tr } })
                 | _ -> assert false end
        | _ -> assert false end
    | _ -> move_up z t

  let rec ins : 'a. Ord.t -> 'a -> 'a z -> 'a t -> 'a t = fun k v z t ->
    match t with
    | Node { color = Black; left; right } as t -> begin
        match Ord.compare k t.key with
        | c when c < 0 ->
            ins k v (overwrite_ t with Node { color = Left_black; left = z }) left
        | c when c > 0 ->
            ins k v (overwrite_ t with Node { color = Right_black; right = z }) right
        | _ (* k == t.key *) -> move_up z (overwrite_ t with Node { value = v }) end
    | Node { color = Red; left; right } as t -> begin
        match Ord.compare k t.key with
        | c when c < 0 ->
            ins k v (overwrite_ t with Node { color = Left_red; left = z }) left
        | c when c > 0 ->
            ins k v (overwrite_ t with Node { color = Right_red; right = z }) right
        | _ (* k == t.key *) -> move_up z (overwrite_ t with Node { value = v }) end
    | Leaf -> fixup z (Node { color = Red; left = Leaf; key = k; value = v; right = Leaf })

  let set_black t =
    match t with
    | Node { color = Red } as t -> overwrite_ t with Node { color = Black }
    | _ -> t

  let insert k v t = set_black (ins k v Leaf t)
end

module IntOrder = struct
  type t = int

  let compare t1 t2 = t1 - t2
end

let () =
  let module Ord = IntOrder in
  let module M1 = Make_Okasaki(Ord) in
  let module M2 = Make_Unique_Okasaki(Ord) in
  let module M3 = Make_Tagged_Okasaki(Ord) in
  let module M4 = Make_Tagged_Bottom_Up(Ord) in

  let baseline_allocation = Gc.allocated_bytes() -. Gc.allocated_bytes() in
  let bytes_per_word = Sys.word_size / 8 in
  let node_size = 6 in
  let compute_delta before after =
    int_of_float ((after -. before) -. baseline_allocation) / bytes_per_word / node_size
  in

  let r1 = work ~insert:M1.insert ~fold ~empty:Leaf in

  let before_r2 = Gc.allocated_bytes () in
  let r2 = unique_work ~insert:M2.insert ~fold ~empty:Leaf in
  let after_r2 = Gc.allocated_bytes () in
  let delta_r2 = compute_delta before_r2 after_r2 in

  let before_r3 = Gc.allocated_bytes () in
  let r3 = unique_work ~insert:M3.insert ~fold:tagged_fold ~empty:Leaf in
  let after_r3 = Gc.allocated_bytes () in
  let delta_r3 = compute_delta before_r3 after_r3 in

  let before_r4 = Gc.allocated_bytes () in
  let r4 = unique_work ~insert:M4.insert ~fold:tagged_fold ~empty:Leaf in
  let after_r4 = Gc.allocated_bytes () in
  let delta_r4 = compute_delta before_r4 after_r4 in

  Printf.printf "%d\n%d %d\n%d %d\n%d %d\n"
    r1 r2 delta_r2 r3 delta_r3 r4 delta_r4

[%%expect{|
type color = Red | Black
type ('k, 'v) tree =
    Node of { color : color; left : ('k, 'v) tree; key : 'k @@ many aliased;
      value : 'v @@ many aliased; right : ('k, 'v) tree;
    }
  | Leaf
val fold :
  'a 'b ('c : value_or_null).
    ('a -> 'b -> 'c -> 'c) -> 'c -> ('a, 'b) tree -> 'c
  @@ global many = <fun>
val work :
  ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
    insert:(int -> bool -> 'a -> 'a) ->
    fold:(('b -> bool -> int -> int) -> int -> 'a -> 'c) -> empty:'a -> 'c
  @@ global many = <fun>
Line 85, characters 16-71:
85 |                 balance_right (Node { t with right = ins k v t.right }) [@nontail]
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 72 [tmc-breaks-tailcall]: This call
is in tail-modulo-cons position in a TMC function,
but the function called is not itself specialized for TMC,
so the call will not be transformed into a tail call.
Please either mark the called function with the [@tail_mod_cons]
attribute, or mark this call with the [@tailcall false] attribute
to make its non-tailness explicit.

Line 80, characters 16-68:
80 |                 balance_left (Node { t with left = ins k v t.left }) [@nontail]
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 72 [tmc-breaks-tailcall]: This call
is in tail-modulo-cons position in a TMC function,
but the function called is not itself specialized for TMC,
so the call will not be transformed into a tail call.
Please either mark the called function with the [@tail_mod_cons]
attribute, or mark this call with the [@tailcall false] attribute
to make its non-tailness explicit.

module Make_Okasaki :
  functor (Ord : Map.OrderedType) ->
    sig
      type 'a t = (Ord.t, 'a) tree
      val fold :
        'a 'b ('c : value_or_null).
          ('a -> 'b -> 'c -> 'c) -> 'c -> ('a, 'b) tree -> 'c
        @@ global many
      val balance_left : ('a, 'b) tree -> ('a, 'b) tree @@ global many
        portable
      val balance_right : ('a, 'b) tree -> ('a, 'b) tree @@ global many
        portable
      val ins : Ord.t -> 'a -> (Ord.t, 'a) tree -> (Ord.t, 'a) tree @@ global
        many
      val set_black : ('a, 'b) tree -> ('a, 'b) tree @@ global many portable
      val insert : Ord.t -> 'a -> (Ord.t, 'a) tree -> (Ord.t, 'a) tree @@
        global many
    end
Lines 114-117, characters 12-88:
114 | ............overwrite_ l with
115 |               Node { color = Red;
116 |                      left  = overwrite_ ll with Node { color = Black };
117 |                      right = overwrite_ t  with Node { color = Black; left = l.right } }
Error: This expression has type "('a, 'b) tree" which is not a record type.
|}]
