external magic : 'a -> 'b = "%opaque"

let g y =
  let rec spl () =
    let relist f ps = magic f in
    if magic y then relist spl 0 else relist spl 1
  in
  spl y
