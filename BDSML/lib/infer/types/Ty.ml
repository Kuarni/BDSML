open Format
open Var
open VarSet
open Base

module Ty : sig
  type t =
    | Ty_var of Var.t
    | Ty_arr of t * t
    | Ty_tuple of t list
    | Ty_con of string * t list
  [@@deriving eq, ord, sexp_of]

  val pp : Format.formatter -> t -> unit
  val unit : t
  val int : t
  val bool : t
  val char : t
  val string : t
  val vars : t -> VarSet.t
end = struct
  type t =
    | Ty_var of Var.t
    | Ty_arr of t * t
    | Ty_tuple of t list
    | Ty_con of string * t list
  [@@deriving eq, ord, sexp_of]

  let rec pp fmt = function
    | Ty_var var -> Var.pp fmt var
    | Ty_arr (ty1, ty2) ->
      (match ty1, ty2 with
       | Ty_arr (_, _), _ -> fprintf fmt "(%a) -> %a" pp ty1 pp ty2
       | _ -> fprintf fmt "%a -> %a" pp ty1 pp ty2)
    | Ty_tuple l ->
      fprintf fmt "(%a)" (pp_print_list pp ~pp_sep:(fun out () -> fprintf out " * ")) l
    | Ty_con (name, args) ->
      (match args with
       | [] -> fprintf fmt "%s" name
       | [ arg ] -> fprintf fmt "%a %s" pp arg name
       | _ ->
         fprintf
           fmt
           "(%a) %s"
           (pp_print_list pp ~pp_sep:(fun out () -> fprintf out ", "))
           args
           name)
  ;;

  let unit = Ty_con ("unit", [])
  let int = Ty_con ("int", [])
  let bool = Ty_con ("bool", [])
  let char = Ty_con ("char", [])
  let string = Ty_con ("string", [])

  let rec vars = function
    | Ty_var x -> VarSet.singleton x
    | Ty_arr (ty1, ty2) -> VarSet.union_list [ vars ty1; vars ty2 ]
    | Ty_tuple tys -> List.map ~f:vars tys |> VarSet.union_list
    | Ty_con (_, tys) -> List.map ~f:vars tys |> VarSet.union_list
  ;;
end
