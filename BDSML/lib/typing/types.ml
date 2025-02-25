(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module TVarId = struct
  type t = int [@@deriving show { with_path = false }]

  let to_string n = Int.to_string n
  let create n : t = n
  let compare = Int.compare
  let ( + ) = ( + )
end

type base_type =
  | TInt
  | TChar
  | TString
  | TBool
[@@deriving show { with_path = false }]

let show_base_type = function
  | TInt -> "int"
  | TChar -> "char"
  | TString -> "string"
  | TBool -> "bool"
;;

let pp_base_type fmt ty = Format.fprintf fmt "%s" @@ show_base_type ty

type type_val =
  | TVar of TVarId.t (** e.g. ['a] *)
  | TBase of base_type (** e.g. [int] *)
  | TParametric of type_val * type_val (** e.g. [int list] *)
  | TTuple of type_val list (** e.g. [int * int] *)
  | TArrow of type_val * type_val (** e.g. [int -> int] *)

let enclose s = "(" ^ s ^ ")"

let rec show_type_val = function
  | TBase b -> show_base_type b
  | TArrow (l, r) -> show_type_val l ^ " -> " ^ show_type_val r
  | TTuple l -> enclose @@ String.concat " * " (List.map show_type_val l)
  | TVar id -> "'" ^ Char.escaped (Char.chr (id + 97))
  | TParametric (t1, t2) -> show_type_val t1 ^ " " ^ show_type_val t2
;;

let pp_type_val fmt ty = Format.fprintf fmt "%s" @@ show_type_val ty

type error =
  | Unification_failed of type_val * type_val
  | Occurs_check
  | No_variable of string
  | Invalid_let

exception Unimplemented of string

module VarSet = Set.Make (TVarId)

let rec occurs_in (v : TVarId.t) = function
  | TVar b -> b = v
  | TParametric (l, r) -> occurs_in v l || occurs_in v r
  | TTuple (f :: tl) -> occurs_in v f || (occurs_in v @@ TTuple tl)
  | TArrow (l, r) -> occurs_in v l || occurs_in v r
  | _ -> false
;;

let free_vars =
  let rec helper acc = function
    | TVar b -> VarSet.add b acc
    | TArrow (l, r) -> helper (helper acc l) r
    | TTuple (h :: tl) -> helper (helper acc h) @@ TTuple tl
    | TParametric (l, r) -> helper (helper acc l) r
    | _ -> acc
  in
  helper VarSet.empty
;;
