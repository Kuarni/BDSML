open Parser.Ast
open Monads
open Types

val infer_program : structure_item list -> ((VarId.t * type_val) list, string) result
