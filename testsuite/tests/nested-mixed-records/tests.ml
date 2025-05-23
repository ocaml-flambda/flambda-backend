(* TEST
   flambda2;
   native;
*)

external box_int32 : int32# -> (int32[@local_opt]) = "%box_int32"
external box_int64 : int64# -> (int64[@local_opt]) = "%box_int64"
external box_float : float# -> (float[@local_opt]) = "%box_float"

let size x =
  let x = Obj.repr x in
  match Obj.Uniform_or_mixed.(repr (of_block x)) with
  | Uniform -> Printf.sprintf "uniform[%d]" (Obj.size x)
  | Mixed { scannable_prefix_len; } ->
    Printf.sprintf "mixed[%d/%d]" scannable_prefix_len (Obj.size x)

module T00 = struct

  (* All values in inner record are boxed, all values in outer record are boxed. *)

  type sub = #{ i32 : int32; i64 : int64; }

  type record = { str : string; sub : sub; }

  let run () =
    let value = Sys.opaque_identity {
      str = "something";
      sub = #{ i32 = 123l; i64 = 456L; };
    } in
    Printf.printf "size=%s value.str=%S value.sub.#i32=%ld value.sub.#i64=%Ld \n%!"
      (size value)
      value.str
      value.sub.#i32
      value.sub.#i64

end

module T01 = struct

  (* All values in inner record are unboxed, all values in outer record are boxed. *)

  type sub = #{ i32 : int32#; i64 : int64#; }

  type record = { str : string; sub : sub; }

  let run () =
    let value = Sys.opaque_identity {
      str = "something";
      sub = #{ i32 = #123l; i64 = #456L; };
    } in
    Printf.printf "size=%s value.str=%S value.sub.#i32=%ld value.sub.#i64=%Ld \n%!"
      (size value)
      value.str
      (box_int32 value.sub.#i32)
      (box_int64 value.sub.#i64)

end

module T02 = struct

  (* All values in inner record are boxed, all values in outer record are unboxed. *)

  type sub = #{ i32 : int32; i64 : int64; }

  type record = { i : int32#; sub : sub; }

  let run () =
    let value = Sys.opaque_identity {
      i = #987l;
      sub = #{ i32 = 123l; i64 = 456L; };
    } in
    Printf.printf "size=%s value.i=%ld value.sub.#i32=%ld value.sub.#i64=%Ld \n%!"
      (size value)
      (box_int32 value.i)
      value.sub.#i32
      value.sub.#i64

end

module T03 = struct

  (* All values in inner record are unboxed, all values in outer record are unboxed. *)

  type sub = #{ i32 : int32#; i64 : int64#; }

  type record = { i : int32#; sub : sub; }

  let run () =
    let value = Sys.opaque_identity {
      i = #987l;
      sub = #{ i32 = #123l; i64 = #456L; };
    } in
    Printf.printf "size=%s value.i=%ld value.sub.#i32=%ld value.sub.#i64=%Ld \n%!"
      (size value)
      (box_int32 value.i)
      (box_int32 value.sub.#i32)
      (box_int64 value.sub.#i64)

end

module T04 = struct

  (* Both inner and outer records have boxed and unboxed values. *)

  type sub = #{ i32 : int32#; i64 : int64#; s : string; }

  type record = { str : string; sub : sub; i: int32#; }

  let run () =
    let value = Sys.opaque_identity {
      str = "something";
      sub = #{ i32 = #123l; i64 = #456L; s = "s"; };
      i = #789l;
    } in
    Printf.printf "size=%s value.str=%S value.sub.#i32=%ld value.sub.#i64=%Ld value.sub.s=%s value.i=%ld \n%!"
      (size value)
      value.str
      (box_int32 value.sub.#i32)
      (box_int64 value.sub.#i64)
      value.sub.#s
      (box_int32 value.i)

end

module T05 = struct

  (* Inner record has only one element (`Product` expects at least two elements). *)

  type sub = #{ i32 : int32#; }

  type record = { str : string; sub : sub; }

  let run () =
    let value = Sys.opaque_identity {
      str = "something";
      sub = #{ i32 = #123l; };
    } in
    Printf.printf "size=%s value.str=%S value.sub.#i32=%ld \n%!"
      (size value)
      value.str
      (box_int32 value.sub.#i32)

end

module T06 = struct

  (* Three levels of nesting. *)

  type aux = #{ i64 : int64#; }

  type sub = #{ i32 : int32#; aux : aux; }

  type record = { str : string; sub : sub; }

  let run () =
    let value = Sys.opaque_identity {
      str = "something";
      sub = #{ i32 = #123l; aux = #{ i64 = #456L; }; };
    } in
    Printf.printf "size=%s value.str=%S value.sub.#i32=%ld value.sub.#aux.#i64=%Ld \n%!"
      (size value)
      value.str
      (box_int32 value.sub.#i32)
      (box_int64 value.sub.#aux.#i64)

end

let tests = [
  T00.run;
  T01.run;
  T02.run;
  T03.run;
  T04.run;
  T05.run;
  T06.run;
]

let () = List.iter (fun test -> test ()) tests
