let rev = List.rev;;

let map_cons f l acc =
  List.fold_left (fun acc el -> (f el acc )) acc l

let map_from_to_cons  f from to_ acc=
  let rec loop f from to_ acc =
    if to_ < from
    then acc
    else loop f (from + 1) to_ (f from acc)
  in
  loop f from to_ acc
;;

let map_from_downto_cons  f from to_ acc =
  let rec loop f from to_ acc =
    if to_ > from
    then acc
    else loop f (from - 1) to_ (f from acc)
  in
  loop f from to_ acc
;;
