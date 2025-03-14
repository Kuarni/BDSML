(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)
open Containers

type ty =
  | ITVar of int
  | ITPrim of string
  | ITArr of ty * ty
  | ITTuple of ty list
  | ITList of ty

val type_var : int -> ty
val tprim_int : ty
val tprim_bool : ty
val tprim_string : ty
val tprim_unit : ty
val tarr : ty -> ty -> ty
val ( @-> ) : ty -> ty -> ty
val tlist : ty -> ty
val ttuple : ty list -> ty
val pretty_pp_ty : Format.formatter -> ty * string VarIMap.t -> unit

type scheme = Scheme of VarISet.t * ty

type error =
  [ `Occurs_check
  | `Unification_failed of ty * ty
  | `Unexpected_type
  | `Unbound_variable of string
  | `Several_bound of string
  | `WrongRecursiveValueBinding
  ]

val pp_error : Format.formatter -> error -> unit
