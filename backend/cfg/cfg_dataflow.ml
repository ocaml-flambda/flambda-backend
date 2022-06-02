[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare

module type Forward_domain = sig
  type t

  val top : t

  val bot : t

  val compare : t -> t -> int

  val join : t -> t -> t

  val to_string : t -> string
end

module type Forward_transfer = sig
  type domain

  type t =
    { normal : domain;
      exceptional : domain
    }

  val basic : domain -> Cfg.basic Cfg.instruction -> t

  val terminator : domain -> Cfg.terminator Cfg.instruction -> t
end

module type Forward_S = sig
  type domain

  type map = domain Label.Tbl.t

  val run :
    Cfg.t -> ?max_iteration:int -> ?init:domain -> unit -> (map, map) Result.t
end

module Forward
    (D : Forward_domain)
    (T : Forward_transfer with type domain = D.t) :
  Forward_S with type domain = D.t = struct
  type domain = D.t

  type transfer = T.t

  type map = domain Label.Tbl.t

  module WorkSetElement = struct
    type t =
      { label : Label.t;
        value : domain
      }

    let compare { label = left_label; value = left_value }
        { label = right_label; value = right_value } =
      match Label.compare left_label right_label with
      | 0 -> D.compare left_value right_value
      | res -> res
  end

  module WorkSet = Set.Make (WorkSetElement)

  let transfer_block : domain -> Cfg.basic_block -> transfer =
   fun value block ->
    let transfer f (acc_normal, acc_exceptional) instr =
      let { T.normal; exceptional } = f acc_normal instr in
      normal, D.join exceptional acc_exceptional
    in
    let normal, exceptional =
      transfer T.terminator
        (ListLabels.fold_left block.body ~init:(value, value)
           ~f:(transfer T.basic))
        block.terminator
    in
    { normal; exceptional }

  let create : Cfg.t -> init:domain option -> map * WorkSet.t ref =
   fun cfg ~init ->
    let map = Label.Tbl.create (Label.Tbl.length cfg.Cfg.blocks) in
    let set = ref WorkSet.empty in
    let value = Option.value init ~default:D.top in
    (* The need to have several blocks in the initial work set stems from the
       fact that we currently need to consider all trap handlers as alive. *)
    Cfg.iter_blocks cfg ~f:(fun label block ->
        if Label.equal label cfg.entry_label || block.is_trap_handler
        then set := WorkSet.add { WorkSetElement.label; value } !set);
    map, set

  let remove_and_return :
      Cfg.t -> WorkSet.t ref -> WorkSetElement.t * Cfg.basic_block =
   fun cfg set ->
    let element = WorkSet.choose !set in
    set := WorkSet.remove element !set;
    element, Cfg.get_block_exn cfg element.label

  let run :
      Cfg.t -> ?max_iteration:int -> ?init:domain -> unit -> (map, map) Result.t
      =
   fun cfg ?(max_iteration = max_int) ?init () ->
    let res, work_set = create cfg ~init in
    let iteration = ref 0 in
    while (not (WorkSet.is_empty !work_set)) && !iteration < max_iteration do
      incr iteration;
      let element, block = remove_and_return cfg work_set in
      let ({ normal; exceptional } : T.t) =
        transfer_block element.value block
      in
      let update ~normal ~exn value =
        Label.Set.iter
          (fun successor_label ->
            let old_value =
              Option.value
                (Label.Tbl.find_opt res successor_label)
                ~default:D.bot
            in
            let new_value = D.join old_value value in
            if not (D.compare new_value old_value <= 0)
            then begin
              Label.Tbl.replace res successor_label new_value;
              work_set
                := WorkSet.add
                     { WorkSetElement.label = successor_label;
                       value = new_value
                     }
                     !work_set
            end)
          (Cfg.successor_labels ~normal ~exn block)
      in
      update ~normal:true ~exn:false normal;
      update ~normal:false ~exn:true exceptional
    done;
    if WorkSet.is_empty !work_set then Result.Ok res else Result.Error res
end

module type Backward_domain = sig
  type t

  val bot : t

  val compare : t -> t -> int

  val join : t -> t -> t

  val less_equal : t -> t -> bool

  val to_string : t -> string
end

module type Backward_transfer = sig
  type domain

  val basic : domain -> exn:domain -> has_an_exceptional_successor:bool -> Cfg.basic Cfg.instruction -> domain

  val terminator :
    domain -> exn:domain -> has_an_exceptional_successor:bool -> Cfg.terminator Cfg.instruction -> domain

  val exception_ : domain -> domain
end

module Instr = Numbers.Int

module type Backward_S = sig
  type domain

  type _ map =
    | Block : domain Label.Tbl.t map
    | Instr : domain Instr.Tbl.t map

  val run :
    Cfg.t ->
    ?max_iteration:int ->
    init:domain ->
    map:'a map ->
    unit ->
    ('a, 'a) Result.t
end

module Backward
    (D : Backward_domain)
    (T : Backward_transfer with type domain = D.t) :
  Backward_S with type domain = D.t = struct
  (* CR xclerc for xclerc: see what can be shared with `Forward`. *)

  type domain = D.t

  type _ map =
    | Block : domain Label.Tbl.t map
    | Instr : domain Instr.Tbl.t map

  module WorkSetElement = struct
    type t =
      { label : Label.t;
        value : domain
      }

    let compare { label = left_label; value = left_value }
        { label = right_label; value = right_value } =
      match Label.compare left_label right_label with
      | 0 -> D.compare left_value right_value
      | res -> res
  end

  module WorkSet = Set.Make (WorkSetElement)

  let transfer_block :
      domain Instr.Tbl.t option ->
      domain ->
      exn:domain ->
      Cfg.basic_block ->
      domain =
   fun tbl value ~exn block ->
    let replace (instr : _ Cfg.instruction) value =
      match tbl with
      | None -> value
      | Some tbl ->
        Instr.Tbl.replace tbl instr.id value;
        value
    in
    let has_an_exceptional_successor =
      not (Label.Set.is_empty (Cfg.successor_labels ~normal:false ~exn:true block))
    in
    let value =
      replace block.terminator (T.terminator value ~exn ~has_an_exceptional_successor block.terminator)
    in
    let value =
      ListLabels.fold_right block.body ~init:value ~f:(fun instr value ->
          replace instr (T.basic value ~exn ~has_an_exceptional_successor instr))
    in
    value

  let create :
      Cfg.t ->
      init:domain ->
      domain Label.Tbl.t * domain Instr.Tbl.t * WorkSet.t ref =
   fun cfg ~init ->
    let map_block = Label.Tbl.create (Label.Tbl.length cfg.Cfg.blocks) in
    let map_instr = Instr.Tbl.create (Label.Tbl.length cfg.Cfg.blocks * 16) in
    let set = ref WorkSet.empty in
    let value = init in
    Cfg.iter_blocks cfg ~f:(fun label _block ->
        Label.Tbl.replace map_block label value;
        set := WorkSet.add { WorkSetElement.label; value } !set);
    map_block, map_instr, set

  let remove_and_return :
      Cfg.t -> WorkSet.t ref -> WorkSetElement.t * Cfg.basic_block =
   fun cfg set ->
    let element = WorkSet.choose !set in
    set := WorkSet.remove element !set;
    element, Cfg.get_block_exn cfg element.label

  let run :
      type a.
      Cfg.t ->
      ?max_iteration:int ->
      init:domain ->
      map:a map ->
      unit ->
      (a, a) Result.t =
   fun cfg ?(max_iteration = max_int) ~init ~map () ->
    let res_block, res_instr, work_set = create cfg ~init in
    let iteration = ref 0 in
    (* note: `handler_map` contains the value at the *start* of the block. *)
    let handler_map : D.t Label.Tbl.t =
      Label.Tbl.create (Label.Tbl.length cfg.Cfg.blocks)
    in
    let instr_map : D.t Instr.Tbl.t option =
      match map with Block -> None | Instr -> Some res_instr
    in
    while (not (WorkSet.is_empty !work_set)) && !iteration < max_iteration do
      incr iteration;
      let element, block = remove_and_return cfg work_set in
      let exn : domain =
        Option.map
          (fun exceptional_successor ->
            Label.Tbl.find_opt handler_map exceptional_successor)
          block.exn
        |> Option.join
        |> Option.value ~default:D.bot
      in
      let value = transfer_block instr_map element.value ~exn block in
      if block.is_trap_handler
      then begin
        let old_value =
          Option.value
            (Label.Tbl.find_opt handler_map block.start)
            ~default:D.bot
        in
        let new_value = T.exception_ value in
        if not (D.less_equal new_value old_value)
        then begin
          Label.Tbl.replace handler_map block.start new_value;
          List.iter
            (fun predecessor_label ->
              let current_value =
                Option.value
                  (Label.Tbl.find_opt res_block predecessor_label)
                  ~default:D.bot
              in
              work_set
                := WorkSet.add
                     { WorkSetElement.label = predecessor_label;
                       value = current_value
                     }
                     !work_set)
            (Cfg.predecessor_labels block)
        end
      end
      else
        List.iter
          (fun predecessor_label ->
            let old_value =
              Option.value
                (Label.Tbl.find_opt res_block predecessor_label)
                ~default:D.bot
            in
            let new_value = D.join old_value value in
            if not (D.less_equal new_value old_value)
            then begin
              Label.Tbl.replace res_block predecessor_label new_value;
              let already_in_workset = ref false in
              work_set
                := WorkSet.filter
                     (fun { WorkSetElement.label; value } ->
                       if Label.equal label predecessor_label
                       then begin
                         if D.less_equal new_value value
                         then already_in_workset := true;
                         not (D.less_equal value new_value)
                       end
                       else true)
                     !work_set;
              if not !already_in_workset
              then
                work_set
                  := WorkSet.add
                       { WorkSetElement.label = predecessor_label;
                         value = new_value
                       }
                       !work_set
            end)
          (Cfg.predecessor_labels block)
    done;
    let return x =
      if WorkSet.is_empty !work_set then Result.Ok x else Result.Error x
    in
    match map with Block -> return res_block | Instr -> return res_instr
end
