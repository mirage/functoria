(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let ocamlify s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> Buffer.add_char b c
      | '-' -> Buffer.add_char b '_'
      | _ -> ())
    s;
  let s' = Buffer.contents b in
  if String.length s' = 0 || ('0' <= s'.[0] && s'.[0] <= '9') then
    raise (Invalid_argument s);
  s'

let ids = Hashtbl.create 1024
let names = Hashtbl.create 1024

let create name =
  let n = try 1 + Hashtbl.find ids name with Not_found -> 1 in
  Hashtbl.replace ids name n;
  Format.sprintf "%s%d" name n

let find_or_create tbl key create_value =
  try Hashtbl.find tbl key
  with Not_found ->
    let value = create_value () in
    Hashtbl.add tbl key value;
    value

let create key ~prefix = find_or_create names key (fun () -> create prefix)
