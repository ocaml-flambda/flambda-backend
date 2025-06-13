(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Nathanaëlle Courant, OCamlPro                      *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a with_name =
  { value : 'a;
    name : string
  }

type 'a with_names =
  { values : 'a;
    names : string list
  }

include Heterogenous_list

module Option_sender = struct
  include Make (struct
    type 'a t = 'a option Channel.sender
  end)

  let rec send : type s. s hlist -> s Constant.hlist -> unit =
   fun refs values ->
    match refs, values with
    | [], [] -> ()
    | r :: rs, v :: vs ->
      Channel.send r (Some v);
      send rs vs
end

module Option_receiver = struct
  include Make (struct
    type 'a t = 'a option Channel.receiver
  end)

  let rec recv : type s. s hlist -> s Constant.hlist = function
    | [] -> []
    | r :: rs -> Option.get (Channel.recv r) :: recv rs
end
