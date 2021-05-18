(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-30-40-41-42"]

module Label = Label

module Cfg = struct
  include Cfg

  module Basic_block = struct
    type t = basic_block

    let start t = t.start

    let body t = t.body

    let terminator t = t.terminator
  end

  let successor_labels t b =
    successor_labels t ~normal:true ~exn:false b |> Label.Set.elements
end

module Cfg_with_layout = struct
  include Cfg_with_layout

  let eliminate_dead_blocks = Eliminate_dead_blocks.run

  (* eliminate fallthrough implies dead block elimination *)
  let eliminate_fallthrough_blocks = Eliminate_fallthrough_blocks.run

  let of_linear = Linear_to_cfg.run

  let to_linear = Cfg_to_linear.run
end

module Passes = struct
  let simplify_terminators = Simplify_terminator.run

  let add_extra_debug = Extra_debug.add
end

module Util = struct
  let verbose = Cfg.verbose

  let print_assembly = Cfg_to_linear.print_assembly
end
