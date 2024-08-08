(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

module Subtyping = struct
  type at_least_one = [ `at_least_one ]
  type at_most_one = [ `at_most_one ]
  type coerce = [ `coerce ]
  type construct = [ `construct ]
  type get = [ `get ]
  type map = [ `map ]

  type isomorphism =
    [ get
    | map
    | at_most_one
    | at_least_one
    | construct
    ]

  type field =
    [ get
    | map
    | at_most_one
    | at_least_one
    ]
    
  type getter =
    [ get
    | at_least_one
    | at_most_one
    ]

  type mapper = map
end
[%%expect{|
module Subtyping :
  sig
    type at_least_one = [ `at_least_one ]
    type at_most_one = [ `at_most_one ]
    type coerce = [ `coerce ]
    type construct = [ `construct ]
    type get = [ `get ]
    type map = [ `map ]
    type isomorphism =
        [ `at_least_one | `at_most_one | `construct | `get | `map ]
    type field = [ `at_least_one | `at_most_one | `get | `map ]
    type getter = [ `at_least_one | `at_most_one | `get ]
    type mapper = map
  end
|}]

module Dictionary = struct
  open Subtyping

  module Isomorphism = struct
    type ('w : (value, value) => value) t = {
      f : 'a 'b 'at 'bt. get:('at -> 'a) -> construct:('b -> 'bt) -> ('a, 'b) 'w -> ('at, 'bt) 'w
    }
  end

  module Field = struct
    type ('w : (value, value) => value) t = { 
      f : 'a 'b 'at 'bt. ('at -> 'a * ('b -> 'bt)) -> ('a, 'b) 'w -> ('at, 'bt) 'w
    }
  end

  module Getter = struct
    type ('w : (value, value) => value) t = { 
      f : 'a 'b 'at 'bt. ('at -> 'a) -> ('a, 'b) 'w -> ('at, 'bt) 'w 
    }
  end

  module Mapper = struct
    type ('w : (value, value) => value) t = {
      f : 'a 'b 'at 'bt. ('at -> f:('a -> 'b) -> 'bt) -> ('a, 'b) 'w -> ('at, 'bt) 'w
    }
  end

  type (_, _ : (value, value) => value) t = 
    | Isomorphism : ('w : (value, value) => value). 'w Isomorphism.t -> ([> isomorphism ], 'w) t
    | Field : ('w : (value, value) => value). 'w Field.t -> ([> field ], 'w) t
    | Getter : ('w : (value, value) => value). 'w Getter.t -> ([> getter ], 'w) t
    | Mapper : ('w : (value, value) => value). 'w Mapper.t -> ([> mapper ], 'w) t

  type (_ : (value, value) => value)  isomorphism_hack = Isomorphism_hack : ('w : (value, value) => value). ([< isomorphism ], 'w) t -> 'w isomorphism_hack
  let isomorphism t ~get ~construct =
    let (Isomorphism_hack t) = Isomorphism_hack t in
    match t with
    | Isomorphism { f } -> f ~get ~construct
    | Field { f } -> f (fun at -> get at, construct)
    | Getter { f } -> f get
    | Mapper { f } -> f (fun at ~f -> construct (f (get at)))

  type (_ : (value, value) => value) getter_hack = Getter_hack : ('w : (value, value) => value). ([< getter ], 'w) t -> 'w getter_hack
  let getter t get = 
    let (Getter_hack t) = Getter_hack t in
    match t with 
    | Getter { f } -> f get

  type (_ : (value, value) => value) mapper_hack = Mapper_hack : ('w : (value, value) => value). ([< mapper ], 'w) t -> 'w mapper_hack
  let mapper t =
    let (Mapper_hack (Mapper { f })) = Mapper_hack t in
    f
  ;;

  type (_ : (value, value) => value) field_hack = Field_hack : ('w : (value, value) => value). ([< field ], 'w) t -> 'w field_hack
  let field t field_f =
    let (Field_hack t) = Field_hack t in
    match t with
    | Field { f } -> f field_f
    | Getter { f } -> f (fun at -> fst (field_f at))
end
[%%expect{|
Lines 58-60, characters 4-52:
58 | ....match t with
59 |     | Field { f } -> f field_f
60 |     | Getter { f } -> f (fun at -> fst (field_f at))
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Mapper _

module Dictionary :
  sig
    module Isomorphism :
      sig
        type ('w : (value, value) => value) t = {
          f :
            'a 'b 'at 'bt.
              get:('at -> 'a) ->
              construct:('b -> 'bt) -> ('a, 'b) 'w -> ('at, 'bt) 'w;
        }
      end
    module Field :
      sig
        type ('w : (value, value) => value) t = {
          f :
            'a 'b 'at 'bt.
              ('at -> 'a * ('b -> 'bt)) -> ('a, 'b) 'w -> ('at, 'bt) 'w;
        }
      end
    module Getter :
      sig
        type ('w : (value, value) => value) t = {
          f : 'a 'b 'at 'bt. ('at -> 'a) -> ('a, 'b) 'w -> ('at, 'bt) 'w;
        }
      end
    module Mapper :
      sig
        type ('w : (value, value) => value) t = {
          f :
            'a 'b 'at 'bt.
              ('at -> f:('a -> 'b) -> 'bt) -> ('a, 'b) 'w -> ('at, 'bt) 'w;
        }
      end
    type (_, _ : (value, value) => value) t =
        Isomorphism : ('w : (value, value) => value).
          'w Isomorphism.t -> ([> Subtyping.isomorphism ], 'w) t
      | Field : ('w : (value, value) => value).
          'w Field.t -> ([> Subtyping.field ], 'w) t
      | Getter : ('w : (value, value) => value).
          'w Getter.t -> ([> Subtyping.getter ], 'w) t
      | Mapper : ('w : (value, value) => value).
          'w Mapper.t -> ([> Subtyping.mapper ], 'w) t
    type (_ : (value, value) => value) isomorphism_hack =
        Isomorphism_hack : ('w : (value, value) => value).
          ([< Subtyping.isomorphism ], 'w) t -> 'w isomorphism_hack
    val isomorphism :
      ('a : (value, value) => value) 'b 'c 'd 'e.
        ([< Subtyping.isomorphism ], 'a) t ->
        get:('b -> 'c) -> construct:('d -> 'e) -> ('c, 'd) 'a -> ('b, 'e) 'a
    type (_ : (value, value) => value) getter_hack =
        Getter_hack : ('w : (value, value) => value).
          ([< Subtyping.getter ], 'w) t -> 'w getter_hack
    val getter :
      ('a : (value, value) => value) 'b 'c 'd 'e.
        ([< Subtyping.getter ], 'a) t ->
        ('b -> 'c) -> ('c, 'd) 'a -> ('b, 'e) 'a
    type (_ : (value, value) => value) mapper_hack =
        Mapper_hack : ('w : (value, value) => value).
          ([< Subtyping.mapper ], 'w) t -> 'w mapper_hack
    val mapper :
      ('a : (value, value) => value) 'b 'c 'd 'e.
        ([< Subtyping.mapper ], 'a) t ->
        ('b -> f:('c -> 'd) -> 'e) -> ('c, 'd) 'a -> ('b, 'e) 'a
    type (_ : (value, value) => value) field_hack =
        Field_hack : ('w : (value, value) => value).
          ([< Subtyping.field ], 'w) t -> 'w field_hack
    val field :
      ('a : (value, value) => value) 'b 'c 'd 'e.
        ([< Subtyping.field ], 'a) t ->
        ('b -> 'c * ('d -> 'e)) -> ('c, 'd) 'a -> ('b, 'e) 'a
  end
|}]

module Mapping = struct
  type ('m, 'w : (value, value) => value) t = T : 'a 'b ('w : (value, value) => value). ('a, 'b) 'w -> ('a -> 'b, 'w) t
end

module General = struct
  type ('inner, 'outer, 'kind) t = {
    f : ('w : (value, value) => value). ('kind, 'w) Dictionary.t -> ('inner, 'w) Mapping.t -> ('outer, 'w) Mapping.t 
  }    
end

type ('inner, 'outer, 'kind) t = ('inner -> 'inner, 'outer -> 'outer, 'kind) General.t

[%%expect{|
module Mapping :
  sig
    type ('m, 'w : (value, value) => value) t =
        T : 'a 'b ('w : (value, value) => value).
          ('a, 'b) 'w -> ('a -> 'b, 'w) t
  end
module General :
  sig
    type ('inner, 'outer, 'kind) t = {
      f :
        ('w : (value, value) => value).
          ('kind, 'w) Dictionary.t ->
          ('inner, 'w) Mapping.t -> ('outer, 'w) Mapping.t;
    }
  end
type ('inner, 'outer, 'kind) t =
    ('inner -> 'inner, 'outer -> 'outer, 'kind) General.t
|}]

