open! Int_replace_polymorphic_compare

module type Domain = sig
  type t

  val equal : t -> t -> bool

  val join : t -> t -> t

  val to_string : t -> string
end

module type Transfer = sig
  type domain

  val basic : domain -> Cfg.basic Cfg.instruction -> domain

  val terminator : domain -> Cfg.terminator Cfg.instruction -> domain

  val exception_ : domain -> domain
end

module type S = sig
  type domain

  type value =
    { before : domain;
      after : domain option;
      exception_ : domain option
    }

  val run :
    Cfg.t ->
    ?max_iteration:int ->
    init:(Cfg.basic_block -> domain * bool) ->
    unit ->
    value Label.Tbl.t
end

module Forward (D : Domain) (T : Transfer with type domain = D.t) :
  S with type domain = D.t = struct
  type domain = D.t

  type value =
    { before : domain;
      after : domain option;
      exception_ : domain option
    }

  let transfer_block : value -> Cfg.basic_block -> value =
   fun value block ->
    let after =
      Some
        (T.terminator
           (ListLabels.fold_left block.body ~init:value.before ~f:T.basic)
           block.terminator)
    in
    let exception_ = Some (T.exception_ value.before) in
    { value with after; exception_ }

  let create :
      Cfg.t ->
      init:(Cfg.basic_block -> domain * bool) ->
      value Label.Tbl.t * Label.Set.t ref =
   fun cfg ~init ->
    (* CR xclerc for xclerc: what should be the initial size? *)
    let map = Label.Tbl.create 32 in
    let set = ref Label.Set.empty in
    Cfg.iter_blocks cfg ~f:(fun label block ->
        let before, in_work_set = init block in
        let after = None in
        let exception_ = None in
        Label.Tbl.replace map label { before; after; exception_ };
        if in_work_set then set := Label.Set.add label !set);
    map, set

  let remove_and_return : Cfg.t -> Label.Set.t ref -> Cfg.basic_block =
   fun cfg set ->
    let label = Label.Set.choose !set in
    set := Label.Set.remove label !set;
    Cfg.get_block_exn cfg label

  let different : domain option -> domain option -> bool =
   fun left right -> not (Option.equal D.equal left right)

  let run :
      Cfg.t ->
      ?max_iteration:int ->
      init:(Cfg.basic_block -> domain * bool) ->
      unit ->
      value Label.Tbl.t =
   fun cfg ?(max_iteration = max_int) ~init () ->
    let res, work_set = create cfg ~init in
    let iteration = ref 0 in
    while (not (Label.Set.is_empty !work_set)) && !iteration < max_iteration do
      incr iteration;
      let block = remove_and_return cfg work_set in
      let predecessors'outputs =
        ListLabels.concat_map (Cfg.predecessor_labels block)
          ~f:(fun predecessor_label ->
            let predecessor_block = Cfg.get_block_exn cfg predecessor_label in
            let if_among_successors ~normal ~exn ~f =
              let successor_labels =
                Cfg.successor_labels predecessor_block ~normal ~exn
              in
              if Label.Set.mem block.start successor_labels
              then [f (Label.Tbl.find res predecessor_label)]
              else []
            in
            if_among_successors ~normal:true ~exn:false ~f:(fun x -> x.after)
            @ if_among_successors ~normal:false ~exn:true ~f:(fun x ->
                  x.exception_))
        |> List.filter_map Fun.id
      in
      let old_value =
        match predecessors'outputs with
        | [] -> Label.Tbl.find res block.start
        | hd :: tl ->
          let before = ListLabels.fold_left tl ~init:hd ~f:D.join in
          { (Label.Tbl.find res block.start) with before }
      in
      let new_value = transfer_block old_value block in
      Label.Tbl.replace res block.start new_value;
      let successors_to_visit =
        Cfg.successor_labels
          ~normal:(different old_value.after new_value.after)
          ~exn:(different old_value.exception_ new_value.exception_)
          block
      in
      work_set := Label.Set.union !work_set successors_to_visit
    done;
    res
end

(* CR xclerc for xclerc: move the code below to another module *)

module Domain = struct
  type t =
    | Reachable
    | Unreachable

  let equal left right =
    match left, right with
    | Reachable, Reachable | Unreachable, Unreachable -> true
    | Reachable, Unreachable | Unreachable, Reachable -> false

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

  let basic domain _ = domain

  let terminator domain _ = domain

  let exception_ domain = domain
end

module Dead_code = Forward (Domain) (Transfer)

let run_dead_code : Cfg_with_layout.t -> unit =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let is_entry_label label = Label.equal label cfg.entry_label in
  let is_trap_handler label = (Cfg.get_block_exn cfg label).is_trap_handler in
  let init { Cfg.start; _ } =
    if is_entry_label start || is_trap_handler start
    then Domain.Reachable, true
    else Domain.Unreachable, false
  in
  let unreachable_labels =
    Label.Tbl.fold
      (fun label { Dead_code.before; _ } acc ->
        match before with
        | Reachable -> acc
        | Unreachable -> Label.Set.add label acc)
      (Dead_code.run cfg ~init ())
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
