let v =
    let[@inline always] f () =
      let x = ref 0 in
      try
        while true do
          incr x;
          if !x = 12 then begin
            incr x;
            raise Exit
          end
        done; (1, 2)
      with _ ->
        let v = (!x, 1 + 41) in
        v
    in
    ignore ();
    f ()
