(*
 * Copyright (c) 2015 Gabriel Radanne <drupyog@zoho.com>
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

open Rresult
open Astring
open Functoria_misc

module Key = Functoria_key

type package = {
  opam : string ;
  pin : string option ;
  build : bool ;
  ocamlfind : String.Set.t ;
  min : String.Set.t ;
  max : String.Set.t ;
}

module Package = struct
  let merge opam a b =
    let ocamlfind = String.Set.union a.ocamlfind b.ocamlfind
    and min = String.Set.union a.min b.min
    and max = String.Set.union a.max b.max
    and pin =
      match a.pin, b.pin with
      | None, None -> None
      | None, Some a | Some a, None -> Some a
      | Some a, Some b when String.equal a b -> Some a
      | _ -> invalid_arg ("conflicting pin depends for " ^ opam)
    and build = a.build || b.build
    in
    match pin with
    | Some _ ->
      (* pin wins over min and max *)
      Some { opam ; build ; ocamlfind ; min = String.Set.empty ; max = String.Set.empty ; pin }
    | None ->
      Some { opam ; build ; ocamlfind ; min ; max ; pin }

  let package ?(build = false) ?sublibs ?ocamlfind ?min ?max ?pin opam =
    let ocamlfind = match sublibs, ocamlfind with
      | None, None -> [opam]
      | Some xs, None -> opam :: List.map (fun x -> opam ^ "." ^ x) xs
      | None, Some a -> a
      | Some _, Some _ ->
        invalid_arg ("dependent package " ^ opam ^ " may either specify ~sublibs or ~ocamlfind")
    in
    let ocamlfind = String.Set.of_list ocamlfind in
    let to_set = function None -> String.Set.empty | Some m -> String.Set.singleton m in
    let min = to_set min and max = to_set max in
    { opam ; build ; ocamlfind ; min ; max ; pin }

  let libraries ps =
    let ocamlfind p = if p.build then String.Set.empty else p.ocamlfind in
    String.Set.elements
      (List.fold_left String.Set.union String.Set.empty
         (List.map ocamlfind ps))

  let package_names ps =
    List.fold_left (fun acc p -> if p.build then acc else p.opam :: acc) [] ps

  let exts_to_string min max build =
    let bui = if build then "build & " else "" in
    let esc_prefix prefix e = Printf.sprintf "%s %S" prefix e in
    let min_strs = List.map (esc_prefix ">=") (String.Set.elements min)
    and max_strs = List.map (esc_prefix "<") (String.Set.elements max)
    and flat xs = String.concat ~sep:" & " xs
    in
    match String.Set.is_empty min, String.Set.is_empty max with
    | true, true -> if build then "{build}" else ""
    | false, true -> Printf.sprintf "{%s %s}" bui (flat min_strs)
    | true, false -> Printf.sprintf "{%s %s}" bui (flat max_strs)
    | false, false ->
      Printf.sprintf "{%s %s & %s}" bui (flat min_strs) (flat max_strs)

  let pp_package t ppf p =
    Fmt.pf ppf "%s%s%s %s" t p.opam t (exts_to_string p.min p.max p.build)
end

let package = Package.package

module Info = struct
  type t = {
    name: string;
    output: string option;
    build_dir: Fpath.t;
    keys: Key.Set.t;
    context: Key.context;
    packages: package String.Map.t;
  }

  let name t = t.name
  let build_dir t = t.build_dir
  let output t = t.output
  let with_output t output = { t with output = Some output }

  let packages t = List.map snd (String.Map.bindings t.packages)
  let libraries t = Package.libraries (packages t)
  let package_names t = Package.package_names (packages t)
  let pins t =
    List.fold_left
      (fun acc p -> match p.pin with None -> acc | Some u -> (p.opam, u) :: acc)
      [] (packages t)

  let keys t = Key.Set.elements t.keys
  let context t = t.context

  let create ~packages ~keys ~context ~name ~build_dir =
    let keys = Key.Set.of_list keys in
    let packages = List.fold_left (fun m p ->
        let n = p.opam in
        match String.Map.find p.opam m with
        | None -> String.Map.add n p m
        | Some p' -> match Package.merge p.opam p p' with
          | Some p -> String.Map.add n p m
          | None -> invalid_arg ("bad version constraints in " ^ p.opam))
        String.Map.empty packages
    in
    { name; build_dir; keys; packages; context; output = None }

  let pp_packages ?(surround = "") ?sep ppf t =
    Fmt.pf ppf "%a" (Fmt.iter ?sep List.iter (Package.pp_package surround)) (packages t)

  let pp verbose ppf ({ name ; build_dir ; keys ; context ; output; _ } as t) =
    let show name = Fmt.pf ppf "@[<2>%s@ %a@]@," name in
    let list = Fmt.iter ~sep:(Fmt.unit ",@ ") List.iter Fmt.string in
    show "Name      " Fmt.string name;
    show "Build-dir " Fpath.pp build_dir;
    show "Keys      " (Key.pps context) keys;
    show "Output    " Fmt.(option string) output;
    if verbose then show "Libraries " list (libraries t);
    if verbose then
      show "Packages  "
        (pp_packages ?surround:None ~sep:(Fmt.unit ",@ ")) t

  let opam ?name ppf t =
    let name = match name with None -> t.name | Some x -> x in
    Fmt.pf ppf "opam-version: \"2.0\"@." ;
    Fmt.pf ppf "name: \"%s\"@." name ;
    Fmt.pf ppf "depends: [ @[<hv>%a@]@ ]@."
      (pp_packages ~surround:"\"" ~sep:(Fmt.unit "@ ")) t ;
    match pins t with
    | [] -> ()
    | pin_depends ->
      let pp_pin ppf (package, url) =
        Fmt.pf ppf "[\"%s.dev\" %S]" package url
      in
      Fmt.pf ppf "pin-depends: [ @[<hv>%a@]@ ]@."
        Fmt.(list ~sep:(unit "@ ") pp_pin) pin_depends
end

type _ typ =
  | Type: 'a -> 'a typ
  | Function: 'a typ * 'b typ -> ('a -> 'b) typ

let is_functor : type a. a typ -> bool = function
  | Type _ -> false
  | Function _ -> true

let (@->) f t = Function (f, t)

let typ ty = Type ty

module Typeid: sig
  type 'a t
  val gen : unit -> 'a t
  val id : 'a t -> int
  type (_, _) witness = Eq : ('a, 'a) witness | NotEq : ('a, 'b) witness
  val witness : 'r t -> 's t -> ('r, 's) witness
  val to_bool : ('a, 'b) witness -> bool
end = struct
  type (_, _) witness = Eq : ('a, 'a) witness | NotEq : ('a, 'b) witness

  let to_bool : type a b. (a, b) witness -> bool = function
    | Eq -> true
    | NotEq -> false

  module Id = struct
    type _ t = ..
  end

  module type ID = sig
    type t

    type _ Id.t += Tid : t Id.t

    val id : int
  end

  type 'a t = (module ID with type t = 'a)

  let gen_id =
    let r = ref 0 in
    fun () ->
      incr r;
      !r

  let gen () (type s) =
    let module M = struct
      type t = s

      type _ Id.t += Tid : t Id.t

      let id = gen_id ()
    end in
    (module M : ID with type t = s)

  let witness : type r s. r t -> s t -> (r, s) witness =
    fun r s ->
    let module R = (val r : ID with type t = r) in
    let module S = (val s : ID with type t = s) in
    match R.Tid with S.Tid -> Eq | _ -> NotEq

  let id (type a) ((module M) : a t) = M.id
end

module rec Typ: sig
  type 'a t =
    | If : {
        cond : 't Key.value;
        branches : ('t * 'a t) list;
        default : 'a t;
      }
        -> 'a t
    | Dev : { dev : 'a Typ.configurable;
              args : ('a, 'v) tl;
              deps : abstract_impl list } -> 'v t
    | App : { f : 'a t; args : ('a, 'v) tl } -> 'v t

  and abstract_impl = Abstract : _ t -> abstract_impl

  and ('a, 'b) tl =
    | Nil : ('a, 'a) tl
    | Cons : 'a t * ('b, 'c) tl -> ('a -> 'b, 'c) tl

  class type ['ty] configurable = object
    method id: 'ty Typeid.t
    method ty: 'ty typ
    method name: string
    method module_name: string
    method keys: Key.t list
    method packages: package list Key.value
    method connect: Info.t -> string -> string list -> string
    method configure: Info.t -> (unit, R.msg) R.t
    method build: Info.t -> (unit, R.msg) R.t
    method clean: Info.t -> (unit, R.msg) R.t
    method deps: abstract_impl list
  end

end = Typ

include Typ

(** Constructors *)

let abstract t = Abstract t

let mk_dev ~args ~deps dev = Dev { dev; args; deps }

(* If *)

let mk_switch ~cond ~branches ~default = If { cond; branches; default }

let if_impl cond then_ else_ =
  mk_switch ~cond ~branches:[ (true, then_); (false, else_) ] ~default:then_

let match_impl cond ~default branches = mk_switch ~cond ~branches ~default

(* App *)

let rec concat_tl : type a b c. (a, b) tl -> (b, c) tl -> (a, c) tl =
 fun t1 t2 -> match t1 with Nil -> t2 | Cons (h, t) -> Cons (h, concat_tl t t2)

let rec mk_app : type a v. f:a t -> args:(a, v) tl -> v t =
 fun ~f ~args:args1 ->
  match f with
  | Dev { dev; args = args2; deps } ->
      mk_dev ~args:(concat_tl args2 args1) ~deps dev
  | App { f; args = args2 } -> mk_app ~f ~args:(concat_tl args2 args1)
  | _ -> App { f; args = args1 }

let ( $ ) f x = mk_app ~f ~args:(Cons (x, Nil))

(** Utilities *)

type 'a impl = 'a t
let impl d = mk_dev ~args:Nil ~deps:[] d

class ['ty] base_configurable = object
  method id : 'ty Typeid.t = Typeid.gen ()
  method packages: package list Key.value = Key.pure []
  method keys: Key.t list = []
  method connect (_:Info.t) (_:string) l =
    Printf.sprintf "return (%s)" (String.concat ~sep:", " l)
  method configure (_: Info.t): (unit, R.msg) R.t = R.ok ()
  method build (_: Info.t): (unit, R.msg) R.t = R.ok ()
  method clean (_: Info.t): (unit, R.msg) R.t = R.ok ()
  method deps: abstract_impl list = []
end

type job = JOB
let job = Type JOB

class ['ty] foreign
     ?(packages=[]) ?(keys=[]) ?(deps=[]) module_name ty
  : ['ty] configurable
  =
  let name = Name.create module_name ~prefix:"f" in
  object
    method ty = ty
    method id = Typeid.gen ()
    method name = name
    method module_name = module_name
    method keys = keys
    method packages = Key.pure packages
    method connect _ modname args =
      Fmt.strf
        "@[%s.start@ %a@]"
        modname
        Fmt.(list ~sep:sp string)  args
    method clean _ = R.ok ()
    method configure _ = R.ok ()
    method build _ = R.ok ()
    method deps = deps
  end

let foreign ?packages ?keys ?deps module_name ty =
  mk_dev ~args:Nil ~deps:[] (new foreign ?packages ?keys ?deps module_name ty)

(** Tables and traversals *)

(* **** WARNING ******
   The [impl] type forms a DAG, implemented as terms with sharing.
   It is *essential* to preserve sharing while walking the terms.
   Otherwise
   - We risk double initialization of devices
   - The DOT graph is a mess
   - We might collect information twice
   As such, the equality, hashing, and tables must be tuned to share
   [impl]s appropriately and the various traversals must use appropriate tables.
*)

let rec hash : type a. a t -> int = function
  | Dev { dev; args; deps } ->
    Hashtbl.hash
      (`Dev, hash_configurable dev, hash_tl args, List.map hash_abstract deps)
  | App { f; args } -> Hashtbl.hash (`App, hash f, hash_tl args)
  | If { cond; branches; default } ->
    Hashtbl.hash
      ( `If,
        cond,
        List.map (fun (p, t) -> Hashtbl.hash (p, hash t)) branches,
        hash default )

and hash_abstract (Abstract x) = hash x

and hash_tl : type a v. (a, v) tl -> int =
  fun x ->
  match x with
  | Nil -> Hashtbl.hash `Nil
  | Cons (h, t) -> Hashtbl.hash (`Cons, hash h, hash_tl t)

and hash_configurable : type a. a configurable -> int =
  fun c ->
  Hashtbl.hash
    (c#name, List.map Key.hash c#keys, List.map hash_abstract c#deps)

let equal_list p l1 l2 =
  List.length l1 = List.length l2 && List.for_all2 p l1 l2

let witness x y = Typeid.witness x#id y#id

let module_name t = t#module_name

type 'a id = 'a Typeid.t

let id t = Typeid.id t#id

let nice_name d =
  module_name d
  |> String.cuts ~sep:"."
  |> String.concat ~sep:"_"
  |> String.Ascii.lowercase
  |> Name.ocamlify

module Graph = struct
  type t =
    | D : { dev : 'a configurable; args : t list; deps : t list; id : int } -> t

  type dtree = t

  module IdTbl = Hashtbl.Make (struct
      type t = dtree

      let hash (D t) = t.id

      let equal (D t1) (D t2) = t1.id = t2.id
    end)

  (* We iter in *reversed* topological order. *)
  let fold f t z =
    let tbl = IdTbl.create 50 in
    let state = ref z in
    let rec aux v =
      if IdTbl.mem tbl v then ()
      else
        let (D { args; deps; _ }) = v in
        IdTbl.add tbl v ();
        List.iter aux deps;
        List.iter aux args;
        state := f v !state
    in
    aux t;
    !state
  let impl_name (D { dev; args = _; deps = _; id }) =
    match is_functor dev#ty with
    | false -> module_name dev
    | true ->
      let prefix = Astring.String.Ascii.capitalize (nice_name dev) in
      Fmt.strf "%s__%d" prefix id

  let var_name (D { dev; args = _; deps = _; id }) =
    let prefix = nice_name dev in
    Fmt.strf "%s__%i" prefix id
end

type ex = Ex : 'a -> ex

let rec equal : type t1 t2. t1 t -> t2 t -> (t1, t2) Typeid.witness =
  fun x y ->
  match (x, y) with
  | Dev c, Dev c' -> (
      match
        ( equal_list equal_abstract c.deps c'.deps,
          equal_tl c.args c'.args (witness c.dev c'.dev) )
      with
      | true, Typeid.Eq -> Eq
      | _ -> NotEq )
  | App a, App b -> (
      match equal_tl a.args b.args (equal a.f b.f) with
      | Typeid.Eq -> Eq
      | Typeid.NotEq -> NotEq )
  | If x1, If x2 -> (
      match
        ( equal x1.default x2.default,
          Obj.repr x1.cond == Obj.repr x2.cond,
          equal_list
            (fun (p1, t1) (p2, t2) ->
               Ex p1 = Ex p2 && equal_abstract (abstract t1) (abstract t2))
            x1.branches x2.branches )
      with
      | Typeid.Eq, true, true -> Eq
      | _ -> NotEq )
  | _ -> NotEq

and equal_abstract (Abstract x) (Abstract y) = Typeid.to_bool @@ equal x y

and equal_tl :
  type t1 t2 v1 v2.
  (t1, v1) tl ->
  (t2, v2) tl ->
  (t1, t2) Typeid.witness ->
  (v1, v2) Typeid.witness =
  fun x y eq ->
  match (x, y, eq) with
  | Nil, Nil, Typeid.Eq -> Eq
  | Cons (h1, t1), Cons (h2, t2), Typeid.Eq -> (
    match (equal h1 h2, equal_tl t1 t2 Eq) with
    | Typeid.Eq, Typeid.Eq -> Eq | _ -> NotEq )
  | _ -> NotEq

module Tbl = Hashtbl.Make (struct
    type t = abstract_impl
    let hash = hash_abstract
    let equal = equal_abstract
  end)

module Hashcons : sig
  type tbl

  val create : unit -> tbl

  val add : tbl -> 'a t -> 'a t -> unit
  val get : tbl -> 'a t -> 'a t option
end = struct
  type tbl = abstract_impl Tbl.t

  let create () = Tbl.create 50

  let add tbl a b = Tbl.add tbl (abstract a) (abstract b)

  let get (type a) tbl (oldv : a t) : a t option =
    if Tbl.mem tbl @@ abstract oldv then
      let (Abstract newv) = Tbl.find tbl (abstract oldv) in
      match equal oldv newv with
      | Typeid.Eq -> Some newv | Typeid.NotEq -> None
    else None
end

let simplify ~full ~context (Abstract t) =
  let tbl = Hashcons.create () in
  let rec aux : type a. a t -> a t =
    fun impl ->
      match Hashcons.get tbl impl with
      | Some impl' -> impl'
      | None ->
        let acc =
          match impl with
          | If { cond; branches; default } ->
            (* Either
               - A key is present in the context
               - We are in full mode, and we use its default value
            *)
            if full || Key.mem context cond then
              let path = Key.eval context cond in
              let t =
                try List.assoc path branches with Not_found -> default
              in
              aux t
            else
              let branches = List.map (fun (p, t) -> (p, aux t)) branches in
              mk_switch ~cond ~branches ~default
          | Dev { dev; args; deps } ->
            let args = aux_tl args in
            let deps = List.map aux_abstract deps in
            mk_dev ~args ~deps dev
          | App { f; args } ->
            let f = aux f in
            let args = aux_tl args in
            mk_app ~f ~args
        in
        Hashcons.add tbl impl acc;
        acc
  and aux_abstract (Abstract a) = Abstract (aux a)
  and aux_tl : type a v. (a, v) tl -> (a, v) tl = function
    | Nil -> Nil
    | Cons (h, t) -> Cons (aux h, aux_tl t)
  in
  Abstract (aux t)

let eval ~context (Abstract t) =
  let new_id =
    let r = ref 0 in
    fun () ->
      incr r;
      !r
  in
  let tbl = Tbl.create 50 in
  let rec aux : type a. a t -> Graph.t =
    fun impl ->
      if Tbl.mem tbl @@ abstract impl then Tbl.find tbl (abstract impl)
      else
        let acc =
          match impl with
          | Dev { dev; args; deps } ->
            let args = aux_tl args in
            let deps = List.map aux_abstract deps in
            Graph.D { dev; args; deps; id = new_id () }
          | App { f; args = extra_args } ->
            let Graph.D { dev; args; deps; id = _ } = aux f in
            let extra_args = aux_tl extra_args in
            D { dev; args = args @ extra_args; deps; id = new_id () }
          | If { cond; branches; default } ->
            let path = Key.eval context cond in
            let t = try List.assoc path branches with Not_found -> default in
            aux t
        in
        Tbl.add tbl (abstract impl) acc;
        acc
  and aux_abstract (Abstract a) = aux a
  and aux_tl : type a v. (a, v) tl -> _ = function
    | Nil -> []
    | Cons (h, t) ->
      let a = aux h in
      a :: aux_tl t
  in
  aux t

type 'b f_dev = { f : 'a. 'a configurable -> 'b }

let with_left_most_device ctx t (f : _ f_dev) =
  let rec aux : type a. a t -> _ = function
    | Dev d -> f.f d.dev
    | App a -> aux a.f
    | If { cond; branches; default } ->
      let path = Key.eval ctx cond in
      let t = try List.assoc path branches with Not_found -> default in
      aux t
  in
  aux t

type 'b f_dev_full = {
  f : 'a 'v. args:'b list -> deps:'b list -> 'a configurable -> 'b;
}

type 'a f_switch = {
  if_ : 'r. cond:'r Key.value -> branches:('r * 'a) list -> default:'a -> 'a;
}

type 'a f_app = f:'a -> args:'a list -> 'a

let map (type r) ~(mk_switch : _ f_switch) ~(mk_app : _ f_app)
    ~(mk_dev : _ f_dev_full) t =
  let tbl = Tbl.create 50 in
  let rec aux : type a. a t -> r =
    fun impl ->
      if Tbl.mem tbl @@ abstract impl then Tbl.find tbl (abstract impl)
      else
        let acc =
          match impl with
          | Dev { dev; args; deps } ->
            let deps =
              List.fold_right (fun (Abstract x) l -> aux x :: l) deps []
            in
            let args = aux_tl args in
            mk_dev.f ~args ~deps dev
          | App { f; args } ->
            let f = aux f in
            let args = aux_tl args in
            mk_app ~f ~args
          | If { cond; branches; default } ->
            let branches = List.map (fun (p, t) -> (p, aux t)) branches in
            let default = aux default in
            mk_switch.if_ ~cond ~branches ~default
        in
        Tbl.add tbl (abstract impl) acc;
        acc
  and aux_tl : type a v. (a, v) tl -> r list = function
    | Nil -> []
    | Cons (h, t) -> aux h :: aux_tl t
  in
  aux t

type label = If : _ Key.value -> label | Dev : _ configurable -> label | App

  let collect :
    type ty.
    (module Monoid with type t = ty) -> (label -> ty) -> abstract_impl -> ty =
    fun (module M) op (Abstract t) ->
    let r = ref M.empty in
    let add x = r := M.union (op x) !r in
    let mk_switch = { if_ = (fun ~cond ~branches:_ ~default:_ -> add @@ If cond) }
    and mk_app ~f:_ ~args:_ = add App
    and mk_dev = { f = (fun ~args:_ ~deps:_ dev -> add @@ Dev dev) } in
    let () = map ~mk_switch ~mk_app ~mk_dev t in
    !r

  (* {2 Dot output} *)
  module Dot = struct
    type edge_label =
      | Functor
      | Argument
      | Dependency
      | Branch of { default : bool }

    let as_dot_graph (Abstract t) =
      let r = ref 0 in
      let new_id () =
        incr r;
        !r
      in
      let vertices = ref [] in
      let edges = ref [] in
      let add r x = r := x :: !r in
      let mk_switch =
        {
          if_ =
            (fun ~cond ~branches ~default ->
               let id = new_id () in
               add vertices (id, If cond);
               List.iter
                 (fun (_, id') -> add edges (id, id', Branch { default = false }))
                 branches;
               add edges (id, default, Branch { default = true });
               id);
        }
      and mk_app ~f ~args =
        let id = new_id () in
        add vertices (id, App);
        add edges (id, f, Functor);
        List.iter (fun id' -> add edges (id, id', Argument)) args;
        id
      and mk_dev =
        {
          f =
            (fun ~args ~deps dev ->
               let id = new_id () in
               add vertices (id, Dev dev);
               List.iter (fun id' -> add edges (id, id', Argument)) args;
               List.iter (fun id' -> add edges (id, id', Dependency)) deps;
               id);
        }
      in
      let _ = map ~mk_switch ~mk_app ~mk_dev t in
      (List.rev !vertices, List.rev !edges)

    let pp_vertice ppf (id, label) =
      let attrs =
        match label with
        | App -> [ ("label", "$"); ("shape", "diamond") ]
        | If cond -> [ ("label", Fmt.strf "If\n%a" Key.pp_deps cond) ]
        | Dev dev ->
          let name = Fmt.strf "%s__%i" (nice_name dev) id in
          let label =
            Fmt.strf "%s\n%s\n%a" name dev#module_name
              Fmt.(list ~sep:(unit ", ") Key.pp)
              dev#keys
          in
          [ ("label", label); ("shape", "box") ]
      in
      let pp_attr ppf (field, v) = Fmt.pf ppf "%s=%S" field v in
      Fmt.pf ppf "%d [%a];" id (Fmt.list ~sep:(Fmt.unit ", ") pp_attr) attrs

    let pp_edges ppf (id, id', label) =
      let attrs =
        match label with
        | Functor -> [ ("style", "bold"); ("tailport", "sw") ]
        | Argument -> []
        | Dependency -> [ ("style", "dashed") ]
        | Branch { default } ->
          let l = [ ("style", "dotted"); ("headport", "n") ] in
          if default then ("style", "bold") :: l else l
      in
      let pp_attr ppf (field, v) = Fmt.pf ppf "%s=%S" field v in
      Fmt.pf ppf "%d -> %d [%a];" id id'
        (Fmt.list ~sep:(Fmt.unit ", ") pp_attr)
        attrs

    let pp ppf t =
      let vertices, edges = as_dot_graph t in
      Fmt.pf ppf {|@[<v2>digraph G {@,ordering=out;@,%a@,@,%a@,}@]|}
        (Fmt.list ~sep:Fmt.cut pp_vertice)
        vertices
        (Fmt.list ~sep:Fmt.cut pp_edges)
        edges
  end

  let pp_dot = Dot.pp

  type key = Functoria_key.t
  type context = Functoria_key.context
  type 'a value = 'a Functoria_key.value

  module type KEY =
    module type of Functoria_key
    with type 'a Arg.converter = 'a Functoria_key.Arg.converter
     and type 'a Arg.t = 'a Functoria_key.Arg.t
     and type Arg.info = Functoria_key.Arg.info
     and type 'a value = 'a Functoria_key.value
     and type 'a key = 'a Functoria_key.key
     and type t = Functoria_key.t
     and type Set.t = Functoria_key.Set.t
     and type 'a Alias.t = 'a Functoria_key.Alias.t
     and type context = Functoria_key.context

let equal x y = Typeid.to_bool (equal x y)
