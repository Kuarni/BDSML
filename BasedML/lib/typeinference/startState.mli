(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val empty_state : StatementInfer.state
val start_state : StatementInfer.state
val start_state_with_system_fun : StatementInfer.state

val init_used_type_names
  :  Ast.let_declaration list
  -> (StatementInfer.state, unit) StatementInfer.t
