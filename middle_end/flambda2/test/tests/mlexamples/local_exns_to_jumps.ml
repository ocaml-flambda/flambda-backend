type 'a ref = { mutable contents : 'a }

external ref : 'a -> 'a ref = "%makemutable"

external ( ! ) : 'a ref -> 'a = "%field0"

external ( := ) : 'a ref -> 'a -> unit = "%setfield0"

external ( < ) : 'a -> 'a -> bool = "%lessthan"

external ( + ) : int -> int -> int = "%addint"

external raise : exn -> 'a = "%raise"

external raise_notrace : exn -> 'a = "%raise_notrace"

exception Exit

(* Should not be converted: 1. raise not raise_notrace 2. the exn handler
   escapes to [g] *)
let f1 x g =
  let r = ref x in
  try
    while !r < 42 do
      if g !r then raise Exit;
      r := !r + 1
    done;
    0
  with Exit -> 42

(* Should not be converted: 1. raise not raise_notrace *)
let f2 x g =
  let r = ref x in
  try
    while !r < 42 do
      if try g !r with _ -> false then raise Exit;
      r := !r + 1
    done;
    0
  with Exit -> 42

(* Should be converted *)
let f3 x g =
  let r = ref x in
  try
    while !r < 42 do
      if try g !r with _ -> false then raise_notrace Exit;
      r := !r + 1
    done;
    0
  with Exit -> 42

exception Exit2 of int * int

(* Should be converted and unboxed *)
let f4 x g =
  let r = ref x in
  try
    while !r < 42 do
      if try g !r with _ -> false then raise_notrace (Exit2 (!r + 2, !r + 3));
      r := !r + 1
    done;
    0
  with Exit2 (a, b) -> a + b
