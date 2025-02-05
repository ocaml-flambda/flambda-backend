type 'a my_list =
  | Nil
  | Cons of 'a * 'a my_list

type 'a my_ref = { mutable contents : 'a }

type 'a rec1 =
  | Foo1 of 'a rec2 * int
  | Bar1 of 'a

and 'a rec2 =
  | Foo2 of 'a rec1 * int
  | Bar2 of 'a

type ('a : value & value) unboxed_rec = #{ x : 'a ; y : string }

module type Optionish = sig
  type 'a my_option =
    | Nothing
    | Just of 'a
end

module Optionish2 = struct
  type 'a my_option =
    | Nothing
    | Just of 'a
end
