open Utils
open Angstrom
open Const_parser
open Ast
open Typexpr_parser

let parse_pconst =
  let+ c = parse_const in
  Pat_constant c
;;

let parse_pvar =
  parse_ident_name
  >>| function
  | "_" -> Pat_any
  | _ as s -> Pat_var s
;;

let parse_pbase = choice [ parse_pvar; parse_pconst ]

let parse_ptuple p =
  sep_by (check_char ',') (parse_pbase <|> p)
  >>= function
  | [] -> fail "It cannot be this way"
  | [ h ] -> return h
  | h :: tl -> return (Pat_tuple (h :: tl))
;;

let parse_pcons p =
  let helper =
    check_string "::"
    *> return (fun p1 p2 -> Pat_construct ("::", Some (Pat_tuple [ p1; p2 ])))
  in
  chainr1 (parse_ptuple p) helper
;;

let parse_por p =
  let helper = check_char '|' *> return (fun ptr1 ptr2 -> Pat_or (ptr1, ptr2)) in
  chainl1 (parse_pcons p) helper
;;

let parse_plist p =
  let parse_list =
    sep_by (check_char ';') (parse_por p)
    >>| fun l ->
    let rec helper = function
      | h :: tl -> Pat_construct ("::", Some (Pat_tuple [ h; helper tl ]))
      | [] -> Pat_construct ("[]", None)
    in
    helper l
  in
  remove_square_brackets (return (Pat_construct ("[]", None)))
  <|> remove_square_brackets parse_list
  <|> parse_por p
;;

let parse_ptype p =
  let* pt = parse_plist p in
  (let+ t = parse_typexpr in
   Pat_type (pt, t))
  <|> return pt
;;

let parse_pattern = fix @@ fun p -> remove_parents (parse_ptype p) <|> parse_ptype p
