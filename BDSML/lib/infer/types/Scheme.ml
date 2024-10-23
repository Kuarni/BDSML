open VarSet
open Ty
open Format

module Scheme : sig
  type t = Forall of VarSet.t * Ty.t

  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
  val free_vars : t -> VarSet.t
end = struct
  type t = Forall of VarSet.t * Ty.t [@@deriving ord, sexp_of]

  let pp ppf (Forall (quantified, ty)) =
    if VarSet.is_empty quantified
    then fprintf ppf "%a" Ty.pp ty
    else fprintf ppf "%a. %a" VarSet.pp quantified Ty.pp ty
  ;;

  let free_vars (Forall (quantified, ty)) = VarSet.diff (Ty.vars ty) quantified
end
