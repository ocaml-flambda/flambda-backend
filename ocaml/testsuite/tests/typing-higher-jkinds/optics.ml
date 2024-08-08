(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type ('c : ((value, value) => value) => value, 's, 't, 'a, 'b) optic = {
  f : ('p : (value, value) => value). 'p 'c -> ('a, 'b) 'p -> ('s, 't) 'p
}

[%%expect{|
type ('c : ((value, value) => value) => value, 's, 't, 'a, 'b) optic = {
  f : ('p : (value, value) => value). 'p 'c -> ('a, 'b) 'p -> ('s, 't) 'p;
}
|}]

type ('p : (value, value) => value) profunctor = {
  dimap : 'a 'b 'c 'd. ('a, 'c) 'p -> contra_map:('b -> 'a) -> map:('c -> 'd) -> ('b, 'd) 'p
}
[%%expect{|
type ('p : (value, value) => value) profunctor = {
  dimap :
    'a 'b 'c 'd.
      ('a, 'c) 'p -> contra_map:('b -> 'a) -> map:('c -> 'd) -> ('b, 'd) 'p;
}
|}]

type ('p : (value, value) => value) cartesian = {
  profunctor : 'p profunctor;
  first : 'a 'b 'c. ('a, 'b) 'p -> ('a * 'c, 'b * 'c) 'p;
  second : 'a 'b 'c. ('a, 'b) 'p -> ('c * 'a, 'c * 'b) 'p
}
[%%expect{|
type ('p : (value, value) => value) cartesian = {
  profunctor : 'p profunctor;
  first : 'a 'b 'c. ('a, 'b) 'p -> ('a * 'c, 'b * 'c) 'p;
  second : 'a 'b 'c. ('a, 'b) 'p -> ('c * 'a, 'c * 'b) 'p;
}
|}]

type ('a, 'b, 's, 't) lens = {
  f : ('p : (value, value) => value). 'p cartesian -> ('a, 'b) 'p -> ('s, 't) 'p
}
[%%expect{|
type ('a, 'b, 's, 't) lens = {
  f :
    ('p : (value, value) => value).
      'p cartesian -> ('a, 'b) 'p -> ('s, 't) 'p;
}
|}]

type ('p : (value, value) => value) cocartesian = {
  profunctor : 'p profunctor;
  first : 'a 'b 'c. ('a, 'b) 'p -> (('a, 'c) Either.t, ('b, 'c) Either.t) 'p;
  second : 'a 'b 'c. ('a, 'b) 'p -> (('c, 'a) Either.t, ('c, 'b) Either.t) 'p
}
[%%expect{|
type ('p : (value, value) => value) cocartesian = {
  profunctor : 'p profunctor;
  first : 'a 'b 'c. ('a, 'b) 'p -> (('a, 'c) Either.t, ('b, 'c) Either.t) 'p;
  second : 'a 'b 'c. ('a, 'b) 'p -> (('c, 'a) Either.t, ('c, 'b) Either.t) 'p;
}
|}]

type ('a, 'b, 's, 't) prism = {
  f : ('p : (value, value) => value). 'p cocartesian -> ('a, 'b) 'p -> ('s, 't) 'p
}
[%%expect{|
type ('a, 'b, 's, 't) prism = {
  f :
    ('p : (value, value) => value).
      'p cocartesian -> ('a, 'b) 'p -> ('s, 't) 'p;
}
|}]

type (_ : (value, value) => value, _) dictionary =
  | Profunctor : ('p : (value, value) => value). 'p profunctor -> ('p, [< `profunctor]) dictionary
  | Cartesian : ('p : (value, value) => value). 'p cartesian -> ('p, [< `profunctor | `cartesian]) dictionary
  | Cocartesian : ('p : (value, value) => value). 'p cocartesian -> ('p, [< `profunctor | `cocartesian]) dictionary
[%%expect{|
type (_ : (value, value) => value, _) dictionary =
    Profunctor : ('p : (value, value) => value).
      'p profunctor -> ('p, [< `profunctor ]) dictionary
  | Cartesian : ('p : (value, value) => value).
      'p cartesian -> ('p, [< `cartesian | `profunctor ]) dictionary
  | Cocartesian : ('p : (value, value) => value).
      'p cocartesian -> ('p, [< `cocartesian | `profunctor ]) dictionary
|}]

type ('i, 'a, 'b, 'j, 's, 't, 'k) indexed_optic = {
  f : ('p : (value, value) => value). ('p, 'k) dictionary -> ('i * 'a, 'b) 'p -> ('j * 's, 't) 'p
}
[%%expect{|
type ('i, 'a, 'b, 'j, 's, 't, 'k) indexed_optic = {
  f :
    ('p : (value, value) => value).
      ('p, 'k) dictionary -> ('i * 'a, 'b) 'p -> ('j * 's, 't) 'p;
}
|}]
