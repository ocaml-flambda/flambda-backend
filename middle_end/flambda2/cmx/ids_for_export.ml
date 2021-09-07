(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*   Copyright 2020 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Coercion = Reg_width_things.Coercion
module Const = Reg_width_things.Const
module Simple = Reg_width_things.Simple

type t =
  { symbols : Symbol.Set.t;
    variables : Variable.Set.t;
    simples : Simple.Set.t;
    consts : Const.Set.t;
    code_ids : Code_id.Set.t;
    continuations : Continuation.Set.t
  }

let empty =
  { symbols = Symbol.Set.empty;
    variables = Variable.Set.empty;
    simples = Simple.Set.empty;
    consts = Const.Set.empty;
    code_ids = Code_id.Set.empty;
    continuations = Continuation.Set.empty
  }

let create ?(symbols = Symbol.Set.empty) ?(variables = Variable.Set.empty)
    ?(simples = Simple.Set.empty) ?(consts = Const.Set.empty)
    ?(code_ids = Code_id.Set.empty) ?(continuations = Continuation.Set.empty) ()
    =
  { symbols; variables; simples; consts; code_ids; continuations }

let singleton_code_id code_id =
  create ~code_ids:(Code_id.Set.singleton code_id) ()

let singleton_continuation cont =
  create ~continuations:(Continuation.Set.singleton cont) ()

let singleton_symbol symbol = create ~symbols:(Symbol.Set.singleton symbol) ()

let add_const t const = { t with consts = Const.Set.add const t.consts }

let add_variable t var = { t with variables = Variable.Set.add var t.variables }

let add_symbol t sym = { t with symbols = Symbol.Set.add sym t.symbols }

let add_name t name =
  Name.pattern_match name ~var:(add_variable t) ~symbol:(add_symbol t)

let add_simple t simple =
  let simples =
    if Coercion.is_id (Simple.coercion simple)
    then t.simples
    else Simple.Set.add simple t.simples
  in
  let t = { t with simples } in
  Simple.pattern_match simple ~const:(add_const t)
    ~name:(fun name ~coercion:_ -> add_name t name)

let add_code_id t code_id =
  { t with code_ids = Code_id.Set.add code_id t.code_ids }

let add_continuation t continuation =
  { t with continuations = Continuation.Set.add continuation t.continuations }

let from_simple simple =
  let simples =
    if Coercion.is_id (Simple.coercion simple)
    then
      (* This simple will not be in the grand_table_of_simples *)
      Simple.Set.empty
    else Simple.Set.singleton simple
  in
  Simple.pattern_match simple
    ~const:(fun const -> create ~simples ~consts:(Const.Set.singleton const) ())
    ~name:(fun name ->
      Name.pattern_match name
        ~var:(fun var ~coercion:_ ->
          create ~simples ~variables:(Variable.Set.singleton var) ())
        ~symbol:(fun sym ~coercion:_ ->
          create ~simples ~symbols:(Symbol.Set.singleton sym) ()))

let union t1 t2 =
  { symbols = Symbol.Set.union t1.symbols t2.symbols;
    variables = Variable.Set.union t1.variables t2.variables;
    simples = Simple.Set.union t1.simples t2.simples;
    consts = Const.Set.union t1.consts t2.consts;
    code_ids = Code_id.Set.union t1.code_ids t2.code_ids;
    continuations = Continuation.Set.union t1.continuations t2.continuations
  }

let rec union_list ts =
  match ts with [] -> empty | t :: ts -> union t (union_list ts)
