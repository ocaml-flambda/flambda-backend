(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   expect;
 }
*)

type 'a r = #{ a : 'a }
and 'a ok = F : 'a r -> 'a ok [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
and 'a ok = F : 'a r -> 'a ok [@@unboxed]
|}]

type 'a r = #{ a : 'a }
and 'a ok = F : { x : 'a r } -> 'a ok [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
and 'a ok = F : { x : 'a r; } -> 'a ok [@@unboxed]
|}]

type 'a r = #{ a : 'a }
type bad = F : 'a r -> bad [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
Line 2, characters 0-38:
2 | type bad = F : 'a r -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

type 'a r = #{ a : 'a }
type bad = F : { x : 'a r } -> bad [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
Line 2, characters 0-46:
2 | type bad = F : { x : 'a r } -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

type 'a r = #{ a : 'a }
and 'a r2 = #{ a : 'a r }
and bad = F : 'a r2 -> bad [@@unboxed]
[%%expect{|
Line 3, characters 0-38:
3 | and bad = F : 'a r2 -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

type 'a r = #{ a : 'a }
and bad = F : { x : 'a r } -> bad [@@unboxed]
[%%expect{|
Line 2, characters 0-45:
2 | and bad = F : { x : 'a r } -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

(* CR layouts v12: Once we allow products containing void in unboxed GADTs,
   we'll have to make sure the below fails separability checking: *)
type t_void : void
and 'a r = #{ a : 'a ; v : t_void }
and bad = F : 'a r -> bad [@@unboxed]
[%%expect{|
Line 3, characters 0-37:
3 | and bad = F : 'a r -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "bad" is value_or_null & void
         because it is an unboxed record.
       But the kind of type "bad" must be a subkind of value & void
         because it's an [@@unboxed] type,
         chosen to have kind value & void.
|}]

type t_void : void
and 'a r = #{ a : 'a ; v : t_void }
and bad = F : { x : 'a r } -> bad [@@unboxed]
[%%expect{|
Line 3, characters 0-45:
3 | and bad = F : { x : 'a r } -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "bad" is value_or_null & void
         because it is an unboxed record.
       But the kind of type "bad" must be a subkind of value & void
         because it's an [@@unboxed] type,
         chosen to have kind value & void.
|}]

type t_void : void
and ('a : value) r = #{ a : 'a ; v : t_void }
and bad = F : 'a r -> bad [@@unboxed]
[%%expect{|
Line 3, characters 0-37:
3 | and bad = F : 'a r -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "bad" is value_or_null & void
         because it is an unboxed record.
       But the kind of type "bad" must be a subkind of value & void
         because it's an [@@unboxed] type,
         chosen to have kind value & void.
|}]

(* CR layouts v12: Double-check this is safe when we add [void]. *)
(* CR reisenberg: This is obviously terrible. The workaround is not
   to use [any] in the kind annotation there, when [void] would be
   more accurate. I think fixing this properly will require enhancing
   [Ctype.unbox_once] to look through unboxed records better.
   My local branch [expand-unboxed-records] (in my [fl-kinds] clone)
   has a start to a solution. But I think ccasinghino doesn't like
   it, so we should discuss. *)
type t_void : void
and 'a r : value & any = #{ a : 'a ; v : t_void }
and bad = F : { x : 'a r } -> bad [@@unboxed]
[%%expect{|
>> Fatal error: Jkind.sort_of_jkind
Uncaught exception: Misc.Fatal_error

|}]
