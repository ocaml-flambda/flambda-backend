type b = { x : int }

type _ t =
  | A : b -> b t
  | B : int -> int t

let with_y b y = { x = b.x + y }

let[@inline never] f () : b t = A { x = 0 }

let get (t : b t) = match t with A x -> x | _ -> .

let test x =
  let op = f () in
  let field = get op in
  if with_y field 1 = { x = 1 } then None else Some (x, op)

let foo () = match test () with None -> failwith "Error" | Some op -> op
