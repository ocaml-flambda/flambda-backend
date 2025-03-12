(******************************************************************************
 *                             flambda-backend                                *
 *                        Xavier Clerc, Jane Street                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

type path = int list

type 'a shape = 'a Lambda.mixed_block_element array

type 'a shape_with_paths = (('a Lambda.mixed_block_element) * path) array

type 'a tree =
  | Leaf of {
    element : 'a Lambda.mixed_block_element;
    new_index : int;
  }
  | Node of {
      children : 'a tree array;
    }

type 'a t =
  { (* CR-soon xclerc for xclerc: once the nesting/flatenning work is done,
       revisit this record to only keep the fields actually needed. *)
    original_shape : 'a shape; (* OK *)
    prefix : 'a shape; (* invariant: no `Product` *)
    suffix : 'a shape; (* invariant: no `Product` *)
    flattened_shape : 'a shape; (* invariant: no `Product` *)
    new_index_to_old_path : path array;
    (* CR-soon xclerc for xclerc: consider building a tree rather than a hashtable. *)
    old_path_to_new_index : (path, int) Hashtbl.t;
    forest : 'a tree array;
  }

let _XXX t = t.forest

let concat_arrays
  : 'a array array -> 'a array
  = fun arrays ->
    (* CR-soon xclerc for xclerc: should we simply use the following?
       `arrays |> Array.to_list |> Array.concat` *)
    let total_len = ref 0 in
    let init = ref None in
    for i = 0 to pred (Array.length arrays) do
      let array = Array.unsafe_get arrays i in
      let len = Array.length array in
      total_len := !total_len + len;
      if len > 0 then
        init := Some (Array.unsafe_get array 0)
    done;
    match !total_len, !init with
    | 0, None -> [||]
    | 0, Some _ -> Misc.fatal_error "broken invariant"
    | _, None -> Misc.fatal_error "broken invariant"
    | _, Some init ->
      let dst = Array.make !total_len init in
      let dst_pos = ref 0 in
      for i = 0 to pred (Array.length arrays) do
        let array = Array.unsafe_get arrays i in
        let len = Array.length array in
        (* CR-soon xclerc for xclerc: use unsafe_blit? *)
        ArrayLabels.blit ~src:array ~src_pos:0 ~dst ~dst_pos:!dst_pos ~len;
        dst_pos := !dst_pos + len
      done;
      dst

(* CR-soon xclerc for xclerc: it is probably quite inefficient to map/concat repeatedly. *)
let rec flatten_one
  : int -> 'a Lambda.mixed_block_element -> 'a shape_with_paths
  = fun index element ->
  match element with
  | Value _
  | Float_boxed _
  | Float64
  | Float32
  | Bits32
  | Bits64
  | Vec128
  | Word -> [|element, [index]|]
  | Product sub_elements->
    flatten_list sub_elements
    |> Array.map (fun (sub_element, path) -> (sub_element, index :: path))

and flatten_list
  : 'a Lambda.mixed_block_element array -> 'a shape_with_paths
  = fun sub_elements ->
    Array.mapi flatten_one sub_elements
    |> concat_arrays

(* CR xclerc for xclerc: should/could be merged with the flattening. *)
let rec build_tree_one
  : (path, int) Hashtbl.t -> path -> int -> 'a Lambda.mixed_block_element -> 'a tree
  = fun old_path_to_new_index path index element ->
    match element with
    | Value _
    | Float_boxed _
    | Float64
    | Float32
    | Bits32
    | Bits64
    | Vec128
    | Word ->
      let path = List.rev (index :: path) in
      begin match Hashtbl.find_opt old_path_to_new_index path with
      | None ->
        Format.eprintf "XXX path=%s\n%!"
          (String.concat ", " (List.map string_of_int path))
        ;
        assert false
      | Some new_index -> Leaf { element; new_index }
      end
    | Product sub_elements ->
      let children =
        build_tree_list old_path_to_new_index (index :: path) sub_elements
      in
      Node { children; }

and build_tree_list
  : (path, int) Hashtbl.t -> path -> 'a Lambda.mixed_block_element array -> 'a tree array
  = fun old_path_to_new_index path sub_elements ->
    Array.mapi
      (fun i sub_element ->
         build_tree_one old_path_to_new_index path i sub_element)
      sub_elements

let print_indentation k =
  for _ = 1 to k do
    Format.eprintf "  "
  done

let print_element (element : 'a Lambda.mixed_block_element) new_index =
  match element with
  | Value _ -> Format.eprintf "Value (new_index=%d)\n%!" new_index
  | Float_boxed _ -> Format.eprintf "Float_boxed _ (new_index=%d)\n%!" new_index
  | Float64 -> Format.eprintf "Float64 (new_index=%d)\n%!" new_index
  | Float32 -> Format.eprintf "Float32 (new_index=%d)\n%!" new_index
  | Bits32 -> Format.eprintf "Bits32 (new_index=%d)\n%!" new_index
  | Bits64 -> Format.eprintf "Bits64 (new_index=%d)\n%!" new_index
  | Vec128 -> Format.eprintf "Vec128 (new_index=%d)\n%!" new_index
  | Word -> Format.eprintf "Word (new_index=%d)\n%!" new_index
  | Product _ -> assert false

let rec print_tree ~indent ~index tree =
  match tree with
  | Leaf { element; new_index } ->
    print_indentation indent;
    Format.eprintf "[%d] " index;
    print_element element new_index
  | Node { children; } ->
    print_indentation indent;
    Format.eprintf "[%d]\n%!" index;
    print_trees ~indent:(succ indent) children

and print_trees ~indent trees =
  Array.iteri
    (fun index tree -> print_tree ~indent ~index tree)
    trees

let of_mixed_block_elements (original_shape : 'a shape) : 'a t =
  let flattened_shape_with_paths =
    flatten_list original_shape
  in
  let prefix = ref [] in
  let suffix = ref [] in
  for idx = Array.length flattened_shape_with_paths - 1 downto 0 do
    let elem, path = flattened_shape_with_paths.(idx) in
    let is_value =
      match elem with
      | Value _ -> true
      | Float_boxed _ | Float64 | Float32 | Bits32 | Bits64 | Vec128 | Word ->
        false
      | Product _ -> Misc.fatal_error "broken invariant"
    in
    if is_value
    then prefix := (elem, path) :: !prefix
    else suffix := (elem, path) :: !suffix
  done;
  let prefix = Array.of_list !prefix in
  let suffix = Array.of_list !suffix in
  let flattened_and_reordered_shape =
    Array.append prefix suffix
  in
  let new_index_to_old_path =
    Array.make (Array.length flattened_and_reordered_shape) []
  in
  let old_path_to_new_index =
    Hashtbl.create (Array.length original_shape)
  in
  Array.iteri
    (fun new_index (_elem, old_path) ->
       new_index_to_old_path.(new_index) <- old_path;
       Hashtbl.replace old_path_to_new_index old_path new_index)
    flattened_and_reordered_shape;
  let forest =
    build_tree_list old_path_to_new_index [] original_shape
  in
  Format.eprintf "new_index_to_old_path:\n%!";
  Array.iteri
    (fun index path ->
       Format.eprintf "  - %d -> %s\n%!" index (String.concat ", " (List.map string_of_int path)))
    new_index_to_old_path;
  Format.eprintf "old_path_to_new_index:\n%!";
  Hashtbl.iter
    (fun path index ->
       Format.eprintf "  - %s -> %d\n%!" (String.concat ", " (List.map string_of_int path)) index)
    old_path_to_new_index;
  Format.eprintf "forest:\n%!";
  print_trees ~indent:0 forest;
  let _ = assert false in
  { original_shape;
    prefix = Array.map fst prefix;
    suffix = Array.map fst suffix;
    flattened_shape = Array.map fst flattened_and_reordered_shape;
    new_index_to_old_path;
    old_path_to_new_index;
    forest;
  }

let value_prefix t = t.prefix

let flat_suffix t = t.suffix

let value_prefix_len t = Array.length t.prefix

let flat_suffix_len t = Array.length t.suffix

let original_shape t = t.original_shape

let flattened_shape t = t.flattened_shape

let flattened_shape_unit t =
  Array.map
    (fun (elt : _ Lambda.mixed_block_element) : (unit Lambda.mixed_block_element) ->
      match elt with
        | Float_boxed _ -> Float_boxed ()
        | Product _sub_elems -> Misc.fatal_error "broken invariant"
        | (Value _ | Float64 | Float32 | Bits32 | Bits64 | Vec128 | Word) as elem
          ->
          elem)
    t.flattened_shape

let new_index_to_old_path t i =
  t.new_index_to_old_path.(i)

let old_path_to_new_index t p =
  match Hashtbl.find_opt t.old_path_to_new_index p with
  | Some i -> i
  | None -> Misc.fatal_errorf "invalid path (%s)" (String.concat ", " (List.map string_of_int p))

let old_path_to_new_indices _t _p =
  assert false
