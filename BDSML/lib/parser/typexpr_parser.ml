(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Ast
open Angstrom
open Utils

(** [int] *)
let parse_single =
  let+ id = parse_ident_name in
  Type_single id
;;

(** [int t list] *)
let parse_params s =
  let rec go acc =
    (let+ main_type = ws1 *> parse_ident_name in
     Type_params (acc, main_type))
    >>= go
    <|> return acc
  in
  let* init = parse_single <|> s in
  go init
;;

(** [int * string * int] *)
let parse_tuple s =
  sep_by1 (check_char '*') @@ parse_params s
  >>= function
  | _ :: _ :: _ as t_list -> return @@ Type_tuple t_list
  | [ t ] -> return t
  | _ -> fail "invalid state in tuple type"
;;

(** [int -> int] *)
let parse_fun s =
  sep_by1 (check_string "->") @@ parse_tuple s
  >>= function
  | _ :: _ :: _ as t_list -> return @@ Type_fun t_list
  | [ t ] -> return t
  | _ -> fail "invalid state in function type"
;;

(** [:int] *)
let parse_typexpr : typexpr t =
  check_char ':' *> fix (fun s -> remove_parents @@ parse_fun s <|> parse_fun s)
;;
