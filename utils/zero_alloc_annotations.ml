(******************************************************************************
 *                             flambda-backend                                *
 *                         Greta Yorsh, Jane Street                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)
type t = Check_default | Check_all | Check_opt_only | No_check

let all = [ Check_default; Check_all; Check_opt_only; No_check ]

let to_string = function
  | Check_default -> "default"
  | Check_all -> "all"
  | Check_opt_only -> "opt"
  | No_check -> "none"

let equal t1 t2 =
  match t1, t2 with
  | Check_default, Check_default -> true
  | Check_all, Check_all -> true
  | No_check, No_check -> true
  | Check_opt_only, Check_opt_only -> true
  | (Check_default | Check_all | Check_opt_only | No_check), _ -> false

let of_string v =
  let f t =
    if String.equal (to_string t) v then Some t else None
  in
  List.find_map f all

let doc =
  "\n\    The argument specifies which annotations to check: \n\
    \      \"opt\" means attributes with \"opt\" payload and is intended for debugging;\n\
    \      \"default\" means attributes without \"opt\" payload; \n\
    \      \"all\" covers both \"opt\" and \"default\" and is intended for optimized builds."
