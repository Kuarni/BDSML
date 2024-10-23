open Ty
open Format

module Err : sig
  type t =
    | Occurs_check
    | Unification_failed of Ty.t * Ty.t
    | Wrong_exp
    | Wrong_type
    | Unbound_adt_type of string
    | Unbound_variable of string
    | Pattern_matching_failed

  val pp : Format.formatter -> t -> unit
end = struct
  type t =
    | Occurs_check
    | Unification_failed of Ty.t * Ty.t
    | Wrong_exp
    | Wrong_type
    | Unbound_adt_type of string
    | Unbound_variable of string
    | Pattern_matching_failed

  let pp fmt = function
    | Occurs_check -> fprintf fmt "Occurs_check"
    | Unification_failed (typ1, typ2) ->
      fprintf fmt "Unification_failed: %a # %a" Ty.pp typ1 Ty.pp typ2
    | Wrong_exp -> fprintf fmt "Wrong_exp"
    | Wrong_type -> fprintf fmt "Wrong_type"
    | Unbound_adt_type str -> fprintf fmt "Unbound_adt_type: %S" str
    | Unbound_variable str -> fprintf fmt "Unbound_variable: %S" str
    | Pattern_matching_failed -> fprintf fmt "Pattern_matching_failed"
  ;;
end
