open Buf

(* Move-to-front codec *)
module Mtf_table : sig
  type 'a t
  val create : unit -> 'a t

  type index = int
  val not_found : index

  val encode : 'a t -> if_absent:(unit -> 'a) -> string -> index * 'a

  val decode : 'a t -> if_absent:(unit -> string * 'a) -> index -> string * 'a
  val last : 'a t -> 'a option
end = struct
  type 'a entry = Empty | Full of string * 'a
  type 'a t = 'a entry array
  let length = 31
  let create () = Array.make length Empty

  type index = int
  let not_found = length

  let swap mtf i =
    assert (i <> not_found);
    let e = match mtf.(i) with
      | Empty -> assert false
      | Full _ as e -> e in
    Array.blit mtf 0 mtf 1 i;
    mtf.(0) <- e

  let push mtf k v =
    Array.blit mtf 0 mtf 1 (length - 1);
    mtf.(0) <- Full (k, v)

  let encode mtf ~if_absent x =
    let rec go mtf x i =
      if i = length then
        let v = if_absent () in
        push mtf x v;
        not_found, v
      else match mtf.(i) with
        | Empty ->
          let v = if_absent () in
          push mtf x v;
          not_found, v
        | Full (k, v) when String.equal k x ->
          swap mtf i;
          i, v
        | Full _ -> go mtf x (i + 1) in
    go mtf x 0

  let decode mtf ~if_absent i =
    if i = not_found then begin
      let (k, v) as kv = if_absent () in
      push mtf k v;
      kv
    end else begin
      swap mtf i;
      match mtf.(0) with
      | Empty -> assert false
      | Full (k, v) -> k, v
    end

  let last mtf =
    match mtf.(length - 1) with
    | Empty -> None
    | Full (_, v) -> Some v
end

(** Source locations *)
module Location = struct
  type t = {
    filename : string;
    line : int;
    start_char : int;
    end_char : int;
    defname : string;
  }

  let to_string { filename; line; start_char; end_char; defname } =
    Printf.sprintf "%s@%s:%d:%d-%d" defname filename line start_char end_char

  let unknown =
    { filename = "<unknown>";
      line = 1;
      start_char = 1;
      end_char = 1;
      defname = "??"  }
end

type state = unit Mtf_table.t Mtf_table.t

module Writer = struct
  open Buf.Write

  type t = state
  let create () = Mtf_table.create ()

  let max_length = 4 * 1024

  let put_location (file_mtf : t) b (id, locs) =
    let total_size_max =
      (* Worst-case size, assuming no MTF hits *)
      List.fold_left (fun sz (loc  : Location.t) ->
        sz + 6 + (String.length loc.filename + 1) + (String.length loc.defname + 1))
        (8 + 1) locs in
    let no_truncation = List.length locs <= 255 && total_size_max <= max_length in
    let locs = if no_truncation then locs else [ Location.unknown ] in
    let start_pos = b.Write.pos in
    put_64 b (Int64.of_int id);
    put_8 b (List.length locs);
    locs |> List.iter (fun (loc : Location.t) ->
      let clamp n lim = if n < 0 || n > lim then lim else n in
      let line_number = clamp loc.line 0xfffff in
      let start_char = clamp loc.start_char 0xfff in
      let end_char = clamp loc.end_char 0xfff in
      let filename_code, defn_mtf =
        Mtf_table.encode file_mtf
          ~if_absent:Mtf_table.create loc.filename in
      let defname_code, () =
        Mtf_table.encode defn_mtf
          ~if_absent:(fun () -> ()) loc.defname in
      let encoded =
        Int64.(
          logor (of_int line_number)
         (logor (shift_left (of_int start_char) 20)
         (logor (shift_left (of_int end_char) (20 + 8))
         (logor (shift_left (of_int (filename_code :> int)) (20 + 8 + 10))
                (shift_left (of_int (defname_code :> int)) (20 + 8 + 10 + 5)))))) in
      put_32 b (Int64.to_int32 encoded);
      put_16 b (Int64.(to_int (shift_right encoded 32)));
      if filename_code = Mtf_table.not_found then
        put_string b loc.filename;
      if defname_code = Mtf_table.not_found then
        put_string b loc.defname);
    if no_truncation then
      assert (b.pos - start_pos <= total_size_max)
end

module Reader = struct
  open Buf.Read

  type t = state
  let create () = Mtf_table.create ()

  let get_location (file_mtf : t) b =
    let id = Int64.to_int (get_64 b) in
    let nlocs = get_8 b in
    let locs = List.init nlocs (fun _ ->
      let low = get_32 b in
      let high = get_16 b in
      let encoded = Int64.(logor (shift_left (of_int high) 32)
                             (logand (of_int32 low) 0xffffffffL)) in
      let line, start_char, end_char, filename_code, defname_code =
        Int64.(
          to_int (logand 0xfffffL encoded),
          to_int (logand 0xffL (shift_right encoded 20)),
          to_int (logand 0x3ffL (shift_right encoded (20 + 8))),
          to_int (logand 0x1fL (shift_right encoded (20 + 8 + 10))),
          to_int (logand 0x1fL (shift_right encoded (20 + 8 + 10 + 5)))) in
      let filename, defn_mtf =
        Mtf_table.decode file_mtf
          ~if_absent:(fun () ->
            let s = get_string b in
            (* Reuse the defname MTF table that's about to be pushed off.
               This is only present to match a bug in the v001 encoder,
               which sometimes generated traces relying on this behaviour.
               The current encoder never relies on this, so once v001
               trace files stop mattering, this match can be deleted *)
            let d = match Mtf_table.last file_mtf with
              | Some v -> v
              | None -> Mtf_table.create () in
            s, d)
          filename_code in
      let defname, () =
        Mtf_table.decode defn_mtf
          ~if_absent:(fun () ->
            let s = get_string b in
            s, ())
          defname_code in
      { Location.line; start_char; end_char; filename; defname; })
    in
    (id, locs)
end
