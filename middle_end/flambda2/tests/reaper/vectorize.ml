let __dummy1__ _ = assert false [@@inline never]

external __dummy2__ : unit -> 'a = "%opaque"

external __ignore__ : 'a -> unit = "%ignore"

module Dependencies = struct
  let offset_in_bytes ~arg_offset_in_bytes t1 t2 = __dummy2__ ()
    [@@inline never] [@@local never]

  let actual_arg_offset_in_bytes block reaching_definitions r1 r2 =
    __dummy2__ ()
    [@@inline never] [@@local never]

  let is_adjacent =
    let arg_offset_in_bytes =
      actual_arg_offset_in_bytes (__dummy2__ ()) (__dummy2__ ())
      [@@inline never] [@@local never]
    in
    __ignore__
      (offset_in_bytes ~arg_offset_in_bytes (__dummy2__ ()) (__dummy2__ ()));
    __dummy2__ ()

  let from_block = __dummy2__ () [@@inline never] [@@local never]
end

let cfg =
  (__dummy2__ ()) ~f:(fun label ->
      (__dummy2__ ()) (fun block ->
          let deps = lazy (Dependencies.from_block block) in
          let deps = (__dummy2__ ()) deps in
          __dummy2__ ());
      __dummy2__ ())
  [@@inline never] [@@local never]
