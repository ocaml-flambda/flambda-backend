(* CR mshinwell: Find a way of doing this postprocessing properly and
   removing the duplication with backtrace_dynlink.ml *)

(* Postprocess backtrace to ignore differences between dune and make
   builds (in the former, Dynlink.Native is Dynlink_internal_native.Native) *)
let begins_with ?(from = 0) str ~prefix =
  (* From utils/misc.ml *)
  let rec helper idx =
    if idx < 0 then true
    else
      String.get str (from + idx) = String.get prefix idx && helper (idx-1)
  in
  let n = String.length str in
  let m = String.length prefix in
  if n >= from + m then helper (m-1) else false

let process_backtrace bt =
  let bt = String.split_on_char '\n' bt in
  let bt =
    List.map (fun line ->
        let prefix = "Called from Dynlink.Native" in
        if begins_with line ~prefix
        then
          "Called from Dynlink_internal_native.Native" ^
            (String.sub line (String.length prefix)
              (String.length line - String.length prefix))
        else
          let prefix = "Re-raised at Dynlink.Native" in
          if begins_with line ~prefix
          then
            "Re-raised at Dynlink_internal_native.Native" ^
              (String.sub line (String.length prefix)
                (String.length line - String.length prefix))
          else
            line
      )
      bt
  in
  String.concat "\n" bt

let () =
  try
    failwith "SUCCESS"
  with
  | e ->
     let c = Printexc.get_callstack 10 in
     process_backtrace (Printexc.raw_backtrace_to_string c)
     |> print_string;
     raise e
