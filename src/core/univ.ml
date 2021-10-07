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

type 'a key = string * ('a -> exn) * (exn -> 'a)

let new_key : string -> 'a key =
 fun s (type a) ->
  let module M = struct
    exception E of a
  end in
  ( s,
    (fun a -> M.E a),
    function
    | M.E a -> a | _ -> raise @@ Invalid_argument ("duplicate key: " ^ s) )

module Map = Map.Make (String)

type t = exn Map.t

let empty = Map.empty
let add (kn, kput, _kget) v t = Map.add kn (kput v) t
let mem (kn, _, _) t = Map.mem kn t

let find (kn, _kput, kget) t =
  if Map.mem kn t then Some (kget @@ Map.find kn t) else None

let merge ~default m =
  let aux _k _def v = Some v in
  Map.union aux default m

let dump =
  let pp_elt ppf (k, v) = Fmt.pf ppf "%s: %a@ " k Fmt.exn v in
  let map_iter f = Map.iter (fun k v -> f (k, v)) in
  Fmt.(iter ~sep:(unit ", ")) map_iter pp_elt
