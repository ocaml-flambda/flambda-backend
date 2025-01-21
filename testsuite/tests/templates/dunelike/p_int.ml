type t = int

let create () = 1
let frob = succ
let to_string = string_of_int

module Flunf = struct
  module Biz = struct
    type t = unit
    let thing = 42
  end

  module Boz = struct
    type t = unit
    let stuff = 99
  end
end

module Fleez = Flunf

let frob_biz t = t
