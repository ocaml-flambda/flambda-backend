(* TEST
<<<<<<< HEAD
 include systhreads;
 hassysthreads;
 not-macos;
 libunix;
 {
   bytecode;
 }{
   native;
 }
||||||| 121bedcfd2

* hassysthreads
include systhreads

** libunix (* Broken on Windows (missing join?), needs to be fixed *)
*** bytecode
*** native

=======
 include systhreads;
 hassysthreads;
 libunix; (* Broken on Windows (missing join?), needs to be fixed *)
 {
   bytecode;
 }{
   native;
 }
>>>>>>> 5.2.0
*)

open Printf

(* Threads and sockets *)

let serve_connection s =
  let buf = Bytes.make 1024 '>' in
  let n = Unix.read s buf 2 (Bytes.length buf - 2) in
  Thread.delay 0.1;
  ignore (Unix.write s buf 0 (n + 2));
  Unix.close s

let server sock =
  while true do
    let (s, _) = Unix.accept sock in
    ignore(Thread.create serve_connection s)
  done

let mutex = Mutex.create ()
let lines = ref []

let client (addr, msg) =
  let sock =
    Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.connect sock addr;
  let buf = Bytes.make 1024 ' ' in
  ignore(Unix.write_substring sock msg 0 (String.length msg));
  let n = Unix.read sock buf 0 (Bytes.length buf) in
  Mutex.lock mutex;
  lines := (Bytes.sub buf 0 n) :: !lines;
  Mutex.unlock mutex

let () =
  let addr = Unix.ADDR_INET(Unix.inet_addr_loopback, 0) in
  let sock =
    Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock addr;
  let addr = Unix.getsockname sock in
  Unix.listen sock 5;
  ignore (Thread.create server sock);
  let client1 = Thread.create client (addr, "Client #1\n") in
  Thread.delay 0.05;
  client (addr, "Client #2\n");
  Thread.join client1;
  List.iter print_bytes (List.sort Bytes.compare !lines);
  flush stdout
