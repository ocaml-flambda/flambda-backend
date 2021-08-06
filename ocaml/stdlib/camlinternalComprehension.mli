
val rev : 'a list -> 'a list
val map_cons: 
  ('a -> 'b list -> 'b list)  -> 'a list -> 'b list -> 'b list
val map_from_to_cons: 
  (int -> 'a list -> 'a list)  -> int -> int -> 'a list -> 'a list
val map_from_downto_cons: 
  (int -> 'a list -> 'a list) -> int -> int  -> 'a list -> 'a list

