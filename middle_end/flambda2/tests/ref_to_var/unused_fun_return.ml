[@@@ocaml.warning "-27"]

let f x = x [@@inline never]

let g x y =
  let r = ref 0 in
  let o = ref y in
  while Sys.opaque_identity true do
    let _v = f x in
    let _w = f x in
    r := y + !o
  done;
  !r

let mouf x y =
  let r = ref 0 in
  (* This is not marked as invariant because not a variable. Would be the same
     thing with a const : TODO improve this *)
  let o = ref (1, 2) in
  while Sys.opaque_identity true do
    let _v = f x in
    let _w = f x in
    r := y + fst (Sys.opaque_identity !o)
  done;
  !r

(* let g b x y z =
 *   let kont r o =
 *     while Sys.opaque_identity true do
 *       let _v = f x in
 *       let _w = f x in
 *       r := y + !o
 *     done;
 *     !r
 *   in
 *   let r = ref 0 in
 *   let o = ref 0 in
 *   let mortadelle = Sys.opaque_identity y in
 *   let () =
 *     if b then
 *       o := mortadelle
 *     else
 *       o := y
 *   in
 *     if Sys.opaque_identity false then
 *       kont r o
 *     else
 *       begin
 *         incr r;
 *         kont r o
 *       end
 * 
 * let plop b_plop x_plop y_plop z_plop =
 *   (g[@inlined]) b_plop x_plop y_plop z_plop
 * 
 * let mouarf x_mouarf y_mouarf z_mouarf =
 *   (plop[@inlined]) true x_mouarf y_mouarf z_mouarf *)
