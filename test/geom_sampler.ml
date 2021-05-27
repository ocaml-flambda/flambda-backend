open Memtrace.Geometric_sampler

let test ~sampling_rate =
  let t = make ~sampling_rate () in
  let total = 100000 in
  let s = ref total in
  let n = ref 0 in
  let n3 = ref 0 in
  let rec go () =
    let k = draw t in
    s := !s - k;
    if !s >= 0 then begin
      incr n;
      if k = 3 then incr n3;
      go ()
    end in
  go ();
  let observed_rate = (float_of_int !n /. float_of_int total) in
  let expected_p3 = (1. -. sampling_rate) *. (1. -. sampling_rate) *. sampling_rate in
  let observed_p3 = (float_of_int !n3 /. float_of_int !n) in
  (* These numbers are printed at low precision to have a high probability of producing
     the same answer. (We don't expect the observed and expected numbers to be exactly
     equal) *)
  Printf.printf "sample rate: %.2f (%.2f), P(X = 3): %.2f (%.2f)\n"
    sampling_rate observed_rate expected_p3 observed_p3


let () =
  [0.01; 0.13; 0.42; 0.73; 1.] |> List.iter (fun s -> test ~sampling_rate:s)
