let[@poll error] test n =
  let res = ref 0 in
  for i = 0 to n do
    res := Sys.opaque_identity (i) + !res
  done;
  !res

