let _ = 
  (* Small arrays are always allocated in the young generation *)
  let newa = [| 0 |] in
  (* Large arrays are always allocated in the old generation *)
  let olda = Array.make 1000 0 in
  let arr = Array.make 1000 olda in
  while true do 
    arr.(0) <- olda;  (* make an old-gen field point to an old-gen object *)
    arr.(0) <- newa   (* now make it point to a young object, a ref is recorded *)
  done
