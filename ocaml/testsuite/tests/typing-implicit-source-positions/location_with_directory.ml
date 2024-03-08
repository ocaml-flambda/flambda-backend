(* TEST
   * native

   flags = "-directory app/foo"
*)

let f = fun ~(src_pos:[%src_pos]) () -> src_pos
let _ = print_string (f ()).pos_fname;;
