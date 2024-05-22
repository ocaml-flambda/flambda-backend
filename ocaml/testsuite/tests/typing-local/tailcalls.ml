(* TEST
 stack-allocation;
 native;
*)

open Printexc

let get_backtrace () =
  raw_backtrace_entries (get_callstack 10)

let equal_backtraces b1 b2 =
  Array.length b1 = Array.length b2
  && Array.for_all2
       (fun s1 s2 ->
          Int.equal
            (s1 : raw_backtrace_entry :> int)
            (s2 : raw_backtrace_entry :> int))
       b1 b2

let rec foo x stack =
  let r = local_ ref x in
  r := !r - 1;
  let stack' = get_backtrace () in
  begin match stack with
  | None -> ()
  | Some stack -> assert (equal_backtraces stack stack')
  end;
  if !r <= 0 then ()
  else foo !r (Some stack')

let () = foo 20 None

let[@inline never][@specialise never][@local never] bar stack =
  let stack' = get_backtrace () in
  assert (equal_backtraces stack stack');
  ()

let foo () =
  let stack = get_backtrace () in
  bar stack

(* Test the local functions optimisation *)

let[@inline never] ignore_local (local_ x) = ()

let rec foo n p a b stack =
  if n <= 0 then () else
  let stack' = get_backtrace () in
  begin match stack with
  | None -> ()
  | Some stack -> assert (equal_backtraces stack stack')
  end;
  let bar x y =
    foo (n - 1) p x y (Some stack')
  in
  ignore_local (Some a);
  if p then
    bar a b
  else
    bar b a

let () = foo 10 true 1 2 None

let rec foo2 n p a b stack =
  if n <= 0 then () else
  let stack' = get_backtrace () in
  begin match stack with
  | None -> ()
  | Some stack -> assert (equal_backtraces stack stack')
  end;
  let bar x y =
    foo2 (n - 1) p x y (Some stack')
  in
  let bar2 p x y =
    if p then
      bar y x
    else
      bar x y
  in
  ignore_local (Some a);
  if p then
    bar2 p a b
  else
    bar2 p b a

let () = foo2 10 true 1 2 None

let rec foo3 n p a b stack =
  if n <= 0 then () else
  let stack' = get_backtrace () in
  begin match stack with
  | None -> ()
  | Some stack -> assert (equal_backtraces stack stack')
  end;
  let bar x y =
    foo3 (n - 1) p x y (Some stack')
  in
  let bar2 p x y =
    ignore_local (Some a);
    if p then
      bar y x
    else
      bar x y
  in
  ignore_local (Some a);
  if p then
    bar2 p a b
  else
    bar2 p b a

let () = foo3 10 true 1 2 None

external local_stack_offset : unit -> int = "caml_local_stack_offset"

let[@inline never][@specialise never][@local never] allocate () =
  local_ ref 0

let[@inline never][@specialise never][@local never] use (local_ r) =
  r := 10

let[@inline never][@specialise never][@local never] bar original =
  let in_tail_call = local_stack_offset () in
  assert (original = in_tail_call)

let[@inline never][@specialise never][@local never] foo () =
  let original = local_stack_offset () in
  let r = allocate () in
  let with_ref = local_stack_offset () in
  assert (with_ref > original);
  use r;
  bar original

let () = foo ()

let[@inline never][@specialise never][@local never] foo f =
  let original = local_stack_offset () in
  let r = allocate () in
  let with_ref = local_stack_offset () in
  assert (with_ref > original);
  use r;
  f original

let () = foo bar

let[@inline never][@specialise never][@local never] iftail c f =
  let original = local_stack_offset () in
  let r = allocate () in
  let with_ref = local_stack_offset () in
  assert (with_ref > original);
  use r;
  if c then f original

let () = iftail true bar

let[@inline never][@specialise never][@local never] bar original
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () () =
  let in_tail_call = local_stack_offset () in
  assert (original = in_tail_call)

let[@inline never][@specialise never][@local never] foo () =
  let original = local_stack_offset () in
  let r = allocate () in
  let with_ref = local_stack_offset () in
  assert (with_ref > original);
  use r;
  bar original
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()

let () = foo ()

let[@inline never][@specialise never][@local never] foo f =
  let original = local_stack_offset () in
  let r = allocate () in
  let with_ref = local_stack_offset () in
  assert (with_ref > original);
  use r;
  f original
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()
     () () () () () () () () () ()

let () = foo bar

let[@inline never][@specialise never][@local never] rec foo previous =
  match previous with
  | None ->
      let original = local_stack_offset () in
      let r = allocate () in
      let with_ref = local_stack_offset () in
      assert (with_ref > original);
      use r;
      foo (Some original)
  | Some original ->
      let in_tail_call = local_stack_offset () in
      assert (original = in_tail_call)

let () = foo None

let[@inline always] inl a b = let local_ z = (a, b) in let r = fst z + snd z in r
let[@inline never] lret a b = local_ let local_ _z = [|a;b|] in inl a b
let () = assert ((lret 1 2)+1 = 4)
