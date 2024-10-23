type rosetta2_status =
  | Unknown
  | Off
  | On

let get_rosetta2_status () =
  let file_path = Filename.temp_file "ocaml-rosetta2" "sysctl" in
  let command = Printf.sprintf "/usr/sbin/sysctl -n sysctl.proc_translated > %S 2> /dev/null" file_path in
  let exit_code = Sys.command command in
  match exit_code with
  | 0 ->
    begin try
        let file_chan = open_in file_path in
        let first_line = input_line file_chan in
        match first_line with
        | "0" -> Off
        | "1" -> On
        | _ -> Unknown
      with _ ->
        Unknown
    end
  | _ ->
    Unknown

let run_if_not_under_rosetta2 ~f =
  match get_rosetta2_status () with
  | Unknown | On -> ()
  | Off -> f ()
