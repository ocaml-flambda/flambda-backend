(* MIT License

   Copyright (c) 2022 Jane Street Group LLC

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)
type t =
  { mutable current_length : int;
    mutable strings : string list
  }

(* An index of 0 in the string table is reserved for the empty string *)
let create () = { current_length = 1; strings = [""] }

let add_string t string =
  t.current_length <- t.current_length + String.length string + 1;
  t.strings <- string :: t.strings

let current_length t = t.current_length

let write t sh_offset buf =
  let cursor = Compiler_owee.Owee_buf.cursor buf ~at:(Int64.to_int sh_offset) in
  List.iter
    (Compiler_owee.Owee_buf.Write.zero_terminated_string cursor)
    (List.rev t.strings)
