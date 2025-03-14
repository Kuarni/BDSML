(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Pmfast

val eliminate_pm_case
  :  pattern * pmf_expression
  -> pmf_expression * pmf_expression
  -> pmf_expression
