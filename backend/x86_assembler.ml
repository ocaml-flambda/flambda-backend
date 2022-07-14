module StringMap = Map.Make (String)

let name s_l s_opt s_l' =
  let first = String.concat "," s_l in
  let mid = match s_opt with None -> "" | Some s -> Printf.sprintf ",%S" s in
  let last = match s_l' with [] -> "" | l -> "," ^ String.concat "," l in
  first ^ mid ^ last

let append key l t =
  StringMap.update key (function None -> Some l | Some l' -> Some (l' @ l)) t

let from_program l =
  let open X86_ast in
  let rec aux acc current_section current_instrs l =
    let add_current () = append current_section (List.rev current_instrs) acc in
    match l with
    | [] -> add_current ()
    | Section (s_l, s_opt, s_l') :: tl ->
      let acc = add_current () in
      let current_section = name s_l s_opt s_l' in
      let current_instrs = [] in
      aux acc current_section current_instrs tl
    | (_ as instr) :: tl -> aux acc current_section (instr :: current_instrs) tl
  in
  match l with
  | [] -> StringMap.empty
  | Section (s_l, s_opt, s_l') :: tl ->
    let current_section = name s_l s_opt s_l' in
    let current_instrs = [] in
    aux StringMap.empty current_section current_instrs tl
  | _line :: _ -> failwith "Invalid program, should start with section"

let hexa = "0123456789abcdef"

and hexa1 =
  "0000000000000000111111111111111122222222222222223333333333333333444444444444444455555555555555556666666666666666777777777777777788888888888888889999999999999999aaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbccccccccccccccccddddddddddddddddeeeeeeeeeeeeeeeeffffffffffffffff"

and hexa2 =
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

let of_string_fast s =
  let len = String.length s in
  let buf = Bytes.create (len * 2) in
  for i = 0 to len - 1 do
    Bytes.unsafe_set buf (i * 2)
      (String.unsafe_get hexa1 (Char.code (String.unsafe_get s i)));
    Bytes.unsafe_set buf
      (succ (i * 2))
      (String.unsafe_get hexa2 (Char.code (String.unsafe_get s i)))
  done;
  Bytes.to_string buf

let assemble asm output_file =
  let sections = from_program asm in
  let sections =
    StringMap.fold
      (fun key instructions acc ->
        { X86_binary_emitter.sec_name = key;
          sec_instrs = Array.of_list instructions
        }
        :: acc)
      sections []
  in
  let sections =
    List.map
      (fun (section : X86_binary_emitter.section) ->
        section.sec_name, X86_binary_emitter.assemble_section X64 section)
      sections
  in
  Printf.printf "length: %d\n" (List.length sections);
  List.iter
    (fun (sec_name, section) ->
      Printf.printf "section name: %s\n%s\n" sec_name
        (of_string_fast (X86_binary_emitter.contents section));
      ())
    sections;
  Printf.printf "test";
  ()
