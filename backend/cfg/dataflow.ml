[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare

module type Domain = sig
  type t

  val top : t

  val bot : t

  val compare : t -> t -> int

  val join : t -> t -> t

  val to_string : t -> string
end

module type Transfer = sig
  type domain

  type t = {
    normal : domain;
    exceptional : domain;
  }

  val basic : domain -> Cfg.basic Cfg.instruction -> t

  val terminator : domain -> Cfg.terminator Cfg.instruction -> t
end

module type S = sig
  type domain

  type init = {
    value : domain;
    in_work_set : bool;
  }

  type map = domain Label.Tbl.t

  val run :
    Cfg.t ->
    ?max_iteration:int ->
    init:(Cfg.basic_block -> init) ->
    unit ->
    (map, map) Result.t
end

module Forward (D : Domain) (T : Transfer with type domain = D.t) :
  S with type domain = D.t = struct
  type domain = D.t
  type transfer = T.t

  type init = {
    value : domain;
    in_work_set : bool;
  }

  type map = domain Label.Tbl.t

  module WorkSetElement = struct
    type t = {
      label : Label.t;
      value : domain;
    }
    let compare
        { label = left_label; value = left_value; }
        { label = right_label; value = right_value; } =
      match Label.compare left_label right_label with
      | 0 -> D.compare left_value right_value
      | res -> res
  end

  module WorkSet = Set.Make (WorkSetElement)

  let transfer_block : domain -> Cfg.basic_block -> transfer =
    fun value block ->
    let transfer f (acc_normal, acc_exceptional) instr =
      let { T.normal; exceptional; } = f acc_normal instr in
      normal, D.join exceptional acc_exceptional
    in
    let normal, exceptional =
      transfer
        T.terminator
        ((ListLabels.fold_left block.body ~init:(value, value) ~f:(transfer T.basic)))
        block.terminator
    in
    { normal; exceptional; }

  let create :
      Cfg.t ->
      init:(Cfg.basic_block -> init) ->
      map * WorkSet.t ref =
   fun cfg ~init ->
    (* CR xclerc for xclerc: what should be the initial size? *)
    let map = Label.Tbl.create 32 in
    let set = ref WorkSet.empty in
    Cfg.iter_blocks cfg ~f:(fun label block ->
        let { value; in_work_set; } = init block in
        Label.Tbl.replace map label value;
        if in_work_set then set := WorkSet.add { WorkSetElement.label; value; } !set);
    map, set

  let remove_and_return : Cfg.t -> WorkSet.t ref -> WorkSetElement.t * Cfg.basic_block =
    fun cfg set ->
    let element = WorkSet.choose !set in
    set := WorkSet.remove element !set;
    element, Cfg.get_block_exn cfg element.label

  let run :
      Cfg.t ->
      ?max_iteration:int ->
      init:(Cfg.basic_block -> init) ->
      unit ->
      (map, map) Result.t =
   fun cfg ?(max_iteration = max_int) ~init () ->
    let res, work_set = create cfg ~init in
    let iteration = ref 0 in
    while (not (WorkSet.is_empty !work_set)) && !iteration < max_iteration do
      incr iteration;
      let element, block = remove_and_return cfg work_set in
      let { normal; exceptional; } : T.t = transfer_block element.value block in
      let update ~normal ~exn new_value : unit =
        Label.Set.iter
          (fun successor_label ->
             let old_value = Label.Tbl.find res successor_label in
             if D.compare old_value new_value <> 0 then begin
               Label.Tbl.replace res successor_label new_value;
               work_set := WorkSet.add {
                   WorkSetElement.label = successor_label;
                   value = new_value;
                 } !work_set
             end)
          (Cfg.successor_labels ~normal ~exn block)
      in
      update ~normal:true ~exn:false normal;
      update ~normal:false ~exn:true exceptional;
    done;
    if (!iteration < max_iteration) then
      Result.Ok res
    else
      Result.Error res
end

(* CR xclerc for xclerc: move the code below to another module *)

module Domain = struct
  type t =
    | Reachable
    | Unreachable

  let top = Reachable

  let bot = Unreachable

  let compare left right =
    match left, right with
    | Reachable, Reachable -> 0
    | Reachable, Unreachable -> 1
    | Unreachable, Reachable -> -1
    | Unreachable, Unreachable -> 0

  let join left right =
    match left, right with
    | Reachable, (Reachable | Unreachable) | Unreachable, Reachable -> Reachable
    | Unreachable, Unreachable -> Unreachable

  let to_string = function
    | Reachable -> "reachable"
    | Unreachable -> "unreachable"
end

module Transfer = struct
  type domain = Domain.t

  type t = {
    normal : domain;
    exceptional : domain;
  }

  let basic value _ = { normal = value; exceptional = value; }

  let terminator value _ = { normal = value; exceptional = value; }
end

module Dataflow = Forward (Domain) (Transfer)

let run_dead_code : Cfg_with_layout.t -> unit =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let is_entry_label label = Label.equal label cfg.entry_label in
  let is_trap_handler label = (Cfg.get_block_exn cfg label).is_trap_handler in
  let init { Cfg.start; _ } =
    if is_entry_label start || is_trap_handler start
    then { Dataflow.value = Domain.Reachable; in_work_set = true; }
    else { Dataflow.value = Domain.Unreachable; in_work_set = false; }
  in
  match Dataflow.run cfg ~init () with
  | Result.Error _ ->
    Misc.fatal_error "Dataflow.run_dead_code: forward analysis did not reach a fix-point";
  | Result.Ok map ->
    let unreachable_labels =
      Label.Tbl.fold
        (fun label value acc ->
           match value with
           | Domain.Reachable -> acc
           | Domain.Unreachable -> Label.Set.add label acc)
        map
        Label.Set.empty
    in
    Label.Set.iter
      (fun label ->
         let block = Cfg.get_block_exn cfg label in
         block.predecessors <- Label.Set.empty;
         Label.Set.iter
           (fun succ_label ->
              let succ_block = Cfg.get_block_exn cfg succ_label in
              succ_block.predecessors
              <- Label.Set.remove label succ_block.predecessors)
           (Cfg.successor_labels ~normal:true ~exn:true block);
         block.terminator <- { block.terminator with desc = Cfg_intf.S.Never };
         block.exns <- Label.Set.empty)
      unreachable_labels;
    Label.Set.iter
      (fun label -> Cfg_with_layout.remove_block cfg_with_layout label)
      unreachable_labels;
    (* CR xclerc for xclerc: temporary. *)
    Eliminate_dead_blocks.run cfg_with_layout
