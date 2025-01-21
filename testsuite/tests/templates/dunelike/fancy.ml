open! Fancy__
open! No_direct_access_to_fancy

module Flourish = Flourish
module Ornament = Ornament
module PI = PI

type t = {
  basic : Basic.t;
  flourish : Flourish.t;
  ornament : Ornament.t;
}

let create p flourish ornament =
  let basic = Basic.create p in
  { basic; flourish; ornament }

let basic { basic; _ } = basic

let to_string { basic; flourish; ornament } =
  Basic.to_string basic
  ^ Flourish.to_string flourish
  ^ Ornament.to_string ornament
