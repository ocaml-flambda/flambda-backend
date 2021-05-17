let cache_size = 1 lsl 14
type cache_bucket = int  (* 0 to cache_size - 1 *)

module Writer = struct
  open Buf.Write

  (* The writer cache carries slightly more state than the reader cache,
     since the writer must make decisions about which slot to use.
     (The reader just follows the choices made by the writer) *)
  type t = {
    cache : int array;
    cache_date : int array;
    (* when an entry was added to the cache (used for eviction) *)
    cache_next : cache_bucket array;
    (* last time we saw this entry, which entry followed it? *)
    mutable next_verify_ix : int;
  }

  let create () =
    { cache = Array.make cache_size 0;
      cache_date = Array.make cache_size 0;
      cache_next = Array.make cache_size 0;
      next_verify_ix = 4242 }

  let max_length = 4096

  let put_backtrace cache b ~alloc_id ~callstack ~callstack_len ~log_new_location =
    let max_entry = 2 + 8 in
    let limit = b.pos + max_length - max_entry in
    let put_hit b bucket ncorrect =
      match ncorrect with
      | 0 -> put_16 b (bucket lsl 2)
      | 1 -> put_16 b ((bucket lsl 2) lor 1)
      | n -> put_16 b ((bucket lsl 2) lor 2); put_8 b n in
    let rec code_no_prediction predictor pos ncodes =
      if pos < 0 || b.pos > limit then
        ncodes
      else begin
        let mask = cache_size - 1 in
        let slot = callstack.(pos) in
        (* Pick the least recently used of two slots, selected by two
           different hashes. *)
        let hash1 = ((slot * 0x4983723) lsr 11) land mask in
        let hash2 = ((slot * 0xfdea731) lsr 21) land mask in
        if cache.cache.(hash1) = slot then begin
          code_cache_hit predictor hash1 pos ncodes
        end else if cache.cache.(hash2) = slot then begin
          code_cache_hit predictor hash2 pos ncodes
        end else begin
          (* cache miss *)
          log_new_location ~index:pos;
          let bucket =
            if cache.cache_date.(hash1) < cache.cache_date.(hash2) then
              hash1
            else
              hash2 in
          (* Printf.printf "miss %05d %016x\n%!"
               bucket slot; (*" %016x\n%!" bucket slot;*) *)
          cache.cache.(bucket) <- slot;
          cache.cache_date.(bucket) <- alloc_id;
          cache.cache_next.(predictor) <- bucket;
          put_16 b ((bucket lsl 2) lor 3);
          put_64 b (Int64.of_int slot);
          code_no_prediction bucket (pos-1) (ncodes + 1)
        end
      end
    and code_cache_hit predictor hit pos ncodes =
      (* Printf.printf "hit %d\n" hit; *)
      cache.cache_date.(hit) <- alloc_id;
      cache.cache_next.(predictor) <- hit;
      code_with_prediction hit hit 0 (pos-1) (ncodes+1)
    and code_with_prediction orig_hit predictor ncorrect pos ncodes =
      assert (ncorrect < 256);
      if pos < 0 || b.pos + 2 > limit then begin
        put_hit b orig_hit ncorrect;
        ncodes
      end else begin
        let slot = callstack.(pos) in
        let pred_bucket = cache.cache_next.(predictor) in
        if cache.cache.(pred_bucket) = slot then begin
          (* correct prediction *)
          (* Printf.printf "pred %d %d\n" pred_bucket ncorrect; *)
          if ncorrect = 255 then begin
            (* overflow: code a new prediction block *)
            put_hit b orig_hit ncorrect;
            code_cache_hit predictor pred_bucket pos ncodes
          end else begin
            code_with_prediction orig_hit pred_bucket (ncorrect + 1) (pos-1) ncodes
          end
        end else begin
          (* incorrect prediction *)
          put_hit b orig_hit ncorrect;
          code_no_prediction predictor pos ncodes
        end
      end in
    code_no_prediction 0 callstack_len 0

  let put_cache_verifier cache b =
    let ix = cache.next_verify_ix in
    cache.next_verify_ix <-
      (cache.next_verify_ix + 5413) land (cache_size - 1);
    put_16 b ix;
    put_16 b cache.cache_next.(ix);
    put_64 b (Int64.of_int cache.cache.(ix))

  let put_dummy_verifier b =
    put_16 b 0xffff;
    put_16 b 0;
    put_64 b 0L
end

module Reader = struct
  open Buf.Read

  type t = {
    cache_loc : int array;
    cache_pred : int array;
    mutable last_backtrace : int array;
  }

  let create () =
    { cache_loc = Array.make cache_size 0;
      cache_pred = Array.make cache_size 0;
      last_backtrace = [| |] }

  let[@inline never] realloc_bbuf bbuf pos (x : int) =
    assert (pos = Array.length bbuf);
    let new_size = Array.length bbuf * 2 in
    let new_size = if new_size < 32 then 32 else new_size in
    let new_bbuf = Array.make new_size x in
    Array.blit bbuf 0 new_bbuf 0 pos;
    new_bbuf

  let[@inline] put_bbuf bbuf pos (x : int) =
    if pos < Array.length bbuf then begin
      Array.unsafe_set bbuf pos x;
      bbuf
    end else
      realloc_bbuf bbuf pos x

  let get_backtrace ({ cache_loc ; cache_pred; _ } as cache) b ~nencoded ~common_pfx_len =
    assert (common_pfx_len <= Array.length cache.last_backtrace);
    let rec decode pred bbuf pos = function
      | 0 -> (bbuf, pos)
      | i ->
        let codeword = get_16 b in
        let bucket = codeword lsr 2 and tag = codeword land 3 in
        cache_pred.(pred) <- bucket;
        begin match tag with
        | 0 ->
           (* cache hit, 0 prediction *)
           let bbuf = put_bbuf bbuf pos cache_loc.(bucket) in
           predict bucket bbuf (pos + 1) (i - 1) 0
        | 1 ->
           (* cache hit, 1 prediction *)
           let bbuf = put_bbuf bbuf pos cache_loc.(bucket) in
           predict bucket bbuf (pos + 1) (i - 1) 1
        | 2 ->
           (* cache hit, N prediction *)
           let ncorrect = get_8 b in
           let bbuf = put_bbuf bbuf pos cache_loc.(bucket) in
           predict bucket bbuf (pos + 1) (i - 1) ncorrect
        | _ ->
           (* cache miss *)
           let lit = Int64.to_int (get_64 b) in
           cache_loc.(bucket) <- lit;
           let bbuf = put_bbuf bbuf pos lit in
           decode bucket bbuf (pos + 1) (i - 1)
        end
    and predict pred bbuf pos i = function
      | 0 -> decode pred bbuf pos i
      | n ->
        let pred' = cache_pred.(pred) in
        let bbuf = put_bbuf bbuf pos cache_loc.(pred') in
        predict pred' bbuf (pos + 1) i (n-1) in
    let (bbuf, pos) = decode 0 cache.last_backtrace common_pfx_len nencoded in
    cache.last_backtrace <- bbuf;
    (bbuf, pos)

  let skip_backtrace _cache b ~nencoded ~common_pfx_len:_ =
    for _ = 1 to nencoded do
      let codeword = get_16 b in
      if codeword land 3 = 2 then
        ignore (get_8 b) (* hitN *)
      else if codeword land 3 = 3 then
        ignore (get_64 b) (* miss *)
    done

  let check_cache_verifier cache b =
    let ix = get_16 b in
    let pred = get_16 b in
    let value = get_64 b in
    if ix <> 0xffff then
      (0 <= ix && ix < Array.length cache.cache_loc &&
       cache.cache_pred.(ix) = pred &&
       cache.cache_loc.(ix) = Int64.to_int value)
    else
      true

  let skip_cache_verifier b =
    let _ix = get_16 b in
    let _pred = get_16 b in
    let _value = get_64 b in
    ()
end

