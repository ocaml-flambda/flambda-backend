(* TEST
   flags = "-extension let_mutable";
   toplevel; *)

(* let mutable is not allowed in class definitions *)
class c =
  let mutable x = 20 in
  object
    method read_incr =
      x <- x + 1;
      x
  end
