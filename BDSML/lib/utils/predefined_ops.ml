(** Copyright 2024-2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type predefined =
  { name : string
  ; t : string
  ; alt_name : string
  }

(** vars *)
let var_nothing = "__nothing"

(** default operators *)
let op_plus = { name = "( + )"; t = "int -> int -> int"; alt_name = "op_plus" }

let op_minus = { name = "( - )"; t = "int -> int -> int"; alt_name = "op_minus" }
let op_mult = { name = "( * )"; t = "int -> int -> int"; alt_name = "op_mult" }
let op_div = { name = "( / )"; t = "int -> int -> int"; alt_name = "op_div" }
let op_neg = { name = "( ~- )"; t = "int -> int"; alt_name = "op_neg" }
let op_pos = { name = "( ~+ )"; t = "int -> int"; alt_name = "op_pos" }
let op_not = { name = "not"; t = "bool -> bool"; alt_name = "op_not" }
let op_gt = { name = "( > )"; t = "'a -> 'a -> bool"; alt_name = "op_gt" }
let op_ge = { name = "( >= )"; t = "'a -> 'a -> bool"; alt_name = "op_ge" }
let op_lt = { name = "( < )"; t = "'a -> 'a -> bool"; alt_name = "op_lt" }
let op_le = { name = "( <= )"; t = "'a -> 'a -> bool"; alt_name = "op_le" }
let op_eq = { name = "( = )"; t = "'a -> 'a -> bool"; alt_name = "op_eq" }
let op_neq = { name = "( <> )"; t = "'a -> 'a -> bool"; alt_name = "op_neq" }
let op_or = { name = "( || )"; t = "bool -> bool -> bool"; alt_name = "op_or" }
let op_and = { name = "( && )"; t = "bool -> bool -> bool"; alt_name = "op_and" }
let op_phys_eq = { name = "( == )"; t = "'a -> 'a -> bool"; alt_name = "op_phys_eq" }
let print_int = { name = "print_int"; t = "int -> unit"; alt_name = "print_int" }

(** special pattern remover *)
let disassemble_constructor =
  { name = "disassemble"; t = "'a -> 'b"; alt_name = "disassemble" }
;;

let get_from_tuple =
  { name = "get_from_tuple"; t = "'a -> int -> 'c"; alt_name = "get_from_tuple" }
;;

let same_cons = { name = "same_cons"; t = "'a -> 'b -> bool"; alt_name = "same_cons" }

let get_cons_param =
  { name = "get_cons_param"; t = "'a -> 'b"; alt_name = "get_cons_param" }
;;

let exception_ = { name = "exception"; t = "string -> 'a"; alt_name = "exception" }

(** List with all ops*)
let predefine_operators =
  [ op_plus
  ; op_minus
  ; op_mult
  ; op_div
  ; op_neg
  ; op_pos
  ; op_not
  ; op_gt
  ; op_ge
  ; op_lt
  ; op_le
  ; op_eq
  ; op_neq
  ; op_or
  ; op_and
  ; op_phys_eq
  ; print_int
  ; disassemble_constructor
  ; get_from_tuple
  ; same_cons
  ; get_cons_param
  ; exception_
  ]
;;
