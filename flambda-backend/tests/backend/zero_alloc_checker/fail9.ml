let[@zero_alloc] rec g x =
  if Sys.opaque_identity true then
    try g x with _ -> ()
  else raise (Failure x)
