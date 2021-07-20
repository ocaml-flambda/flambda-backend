module GPR_109 = struct

  let f () =
    let r = ref 0. in
    for i = 1 to 1000 do
      let x = float i in
      let y = if i mod 2 = 0 then x else x +. 1. in
      r := !r +. y
    done;
    !r

end
