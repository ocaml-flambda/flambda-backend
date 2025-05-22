let __dummy1__ _ = assert false [@@inline never]

external __dummy2__ : unit -> 'a = "%opaque"

external __ignore__ : 'a -> unit = "%ignore"

type block_t = { size : int }

module Dependencies = struct
  module Operation : sig
    type t

    val is_adjacent : t -> t -> block_t -> t -> bool

    val is_disjoint : t -> t -> block_t -> t -> bool
  end = struct
    type t

    let offset_in_bytes ~arg_offset_in_bytes (t1 : t) (t2 : t) = __dummy2__ ()
      [@@inline never] [@@local never]

    let actual_arg_offset_in_bytes block reaching_definitions r1 r2 =
      __dummy2__ ()
      [@@inline never] [@@local never]

    let is_adjacent reaching_definitions =
      let arg_offset_in_bytes =
        actual_arg_offset_in_bytes (__dummy2__ ()) (__dummy2__ ())
        [@@inline never] [@@local never]
      in
      match
        offset_in_bytes ~arg_offset_in_bytes (__dummy2__ ()) (__dummy2__ ())
      with
      | None -> __dummy2__ ()
      | Some offset_in_bytes -> __dummy2__ ()
      [@@inline never] [@@local never]

    let is_before ~arg_offset_in_bytes t1 t2 block =
      let res =
        match
          offset_in_bytes ~arg_offset_in_bytes (__dummy2__ ()) (__dummy2__ ())
        with
        | None -> __dummy2__ ()
        | Some offset_in_bytes -> __dummy2__ ()
      in
      __dummy2__ ()
      [@@inline never] [@@local never]

    let is_disjoint ~arg_offset_in_bytes t1 t2 block =
      let res =
        __ignore__
          (is_before ~arg_offset_in_bytes (__dummy2__ ()) (__dummy2__ ())
             (__dummy2__ ()));
        __dummy2__ ()
      in
      __dummy2__ ()
      [@@inline never] [@@local never]

    let is_disjoint reaching_definitions =
      let arg_offset_in_bytes r1 r2 =
        __dummy2__ ()
        [@@inline never] [@@local never]
      in
      is_disjoint ~arg_offset_in_bytes (__dummy2__ ()) (__dummy2__ ())
        (__dummy2__ ())
      [@@inline never] [@@local never]
  end

  let from_block = __dummy2__ () [@@inline never] [@@local never]
end

let cfg =
  (__dummy2__ ()) ~f:(fun label ->
      __ignore__
        ((__dummy2__ ()) (fun block ->
             let deps = lazy (Dependencies.from_block block) in
             let deps = (__dummy2__ ()) deps in
             __dummy2__ ()));
      __dummy2__ ());
  __dummy2__ ()
  [@@inline never] [@@local never]
