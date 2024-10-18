(** Copyright 2023-2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type constant =
  | Const_int of int (** Integer literal, e.g. [69] *)
  | Const_char of char (** Character literal, e.g. ['m'] *)
  | Const_string of string (** String literal, e.g. ["something"] *)
[@@deriving show { with_path = false }]

type rec_flag =
  | Nonrecursive
  | Recursive
[@@deriving show { with_path = false }]

type pattern =
  | Pat_any (** Pattern any [_] *)
  | Pat_var of string (** Var pattern, e.g. [x] *)
  | Pat_constant of constant (** Constant patterns, e.g. [69], ['m'], ["something"] *)
  | Pat_tuple of pattern list
  (** Pattern for many elements, e.g. [(P1, ..., Pn)] ([n >= 2]) *)
  | Pat_or of pattern * pattern (** Pattern for one of elements, e.g. [P1 | P2] *)
  | Pat_construct of string * pattern option
  (** [Pat_construct(C, args)] represents:
      - [C]   when [args] is [None]
      - [C P] when [args] is [Some P] *)
[@@deriving show { with_path = false }]

type value_binding =
  { pat : pattern
  ; expr : expression
  }
[@@deriving show { with_path = false }]

and case =
  { left : pattern
  ; right : expression
  }
[@@deriving show { with_path = false }]

and expression =
  | Exp_ident of string (** Identifiers, e.g. [some_var] *)
  | Exp_constant of constant (** Expression constant, e.g. [69], [m'], ["something"] *)
  | Exp_let of rec_flag * value_binding list * expression
  (** [Exp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:
      - [let P1 = E1 and ... and Pn = EN in E]     when [flag] is [Nonrecursive]
      - [let rec P1 = E1 and ... and Pn = EN in E] when [flag] is [Recursive].
        Invariant: [n >= 1] *)
  | Exp_fun of pattern list * expression
  (** [Exp_fun ([P1; ...; Pn], E)] represents [fun P1 ... Pn -> E].
      Invariant: [n >= 1] *)
  | Exp_function of case list
  (** [Exp_function([C1; ...; Cn])] represents [function C1 | ... | Cn].
      Invariant: [n >= 1] *)
  | Exp_apply of expression * expression (** [Exp_apply(E0, E1)] represents [E0 E1] *)
  | Exp_match of expression * case list
  (** [match E0 with P1 -> E1 | ... | Pn -> En]. Invariant: [n >= 1] *)
  | Exp_tuple of expression list (** Expressions [(E1, ..., En)]. Invariant: [n >= 2] *)
  | Exp_construct of string * expression option
  (** [Exp_construct(C, exp)] represents:
      - [C]               when [exp] is [None],
      - [C E]             when [exp] is [Some E],
      - [C (E1, ..., En)] when [exp] is [Some (Exp_tuple[E1;...;En])] *)
  | Exp_if of expression * expression * expression option (** [if E1 then E2 else E3] *)
  | Exp_sequence of expression * expression (** [E1; E2] *)
[@@deriving show { with_path = false }]
