open Var
open Base
open Format

module VarSet = struct
  type t = (Var.t, Var.comparator_witness) Set.t

  let pp fmt set =
    let vars = Set.to_list set in
    pp_print_list ~pp_sep:(fun out () -> fprintf out " ") Var.pp fmt vars
  ;;

  let compare = Set.compare_direct
  let sexp_of_t set = Set.sexp_of_m__t (module Var) set
  let empty = Set.empty (module Var)
  let singleton = Set.singleton (module Var)
  let of_list = Set.of_list (module Var)
  let to_list = Set.to_list
  let is_empty = Set.is_empty
  let mem = Set.mem
  let add = Set.add
  let union = Set.union
  let union_list = Set.union_list (module Var)
  let inter = Set.inter
  let diff = Set.diff
  let filter = Set.filter
  let fold = Set.fold
  let fold_right = Set.fold_right
end
