open Regalloc_gi_utils

module Int_min = struct
    include Int

    let compare l r = Int.compare r l
end

module Test (I : Order with type t = int) = struct

    module Q = Make_max_priority_queue (I)
    module M = Map.Make (I)

    let equal l r = I.compare l r = 0

    let enq map i d =
        M.update i (function
                    | None -> Some (M.singleton d 1)
                    | Some ds -> Some (M.update d (function
                                                   | None -> Some 1
                                                   | Some j -> Some (j + 1)) ds)) map

    let deq map i d =
        M.update i (function
                    | None -> assert false
                    | Some ds when M.equal equal ds (M.singleton d 1) -> None
                    | Some ds -> Some (M.update d (function
                                                   | None | Some 0 -> assert false
                                                   | Some 1 -> None
                                                   | Some j -> Some (j - 1)) ds)) map

    let check_top q m =
        if Q.is_empty q then assert (M.is_empty !m)
        else (let Q.{ priority = qi; data = qd } = Q.get q in
              let mi, mds = M.max_binding !m in
              assert (qi = mi);
              assert (M.mem qd mds))

    let enq q m =
        let i = Random.int 100 in
        let d = Random.int 1_000 in
        Q.add q ~priority:i ~data:d;
        m := enq !m i d;
        check_top q m

    let deq q m =
        let Q.{ priority = qi; data = qd } = Q.get q in
        let Q.{ priority = qi'; data = qd' } = Q.get_and_remove q in
        assert (qi = qi');
        assert (qd = qd');
        m := deq !m qi qd;
        check_top q m

    let () =
        Random.init 42;
        let q = Q.make ~initial_capacity:10 in
        let m = ref M.empty in
        for _ = 0 to 1_000_000 do
            (* Bias towards adding elements. *)
            if Random.int 100 < 55 then enq q m
            else if Q.is_empty q then assert (M.is_empty !m)
            else deq q m
        done;
        while not (Q.is_empty q) do deq q m done;
        assert (M.is_empty !m)
    ;;
end

module Int_max_test = Test (Int)
module Int_min_test = Test (Int_min)
