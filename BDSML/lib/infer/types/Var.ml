open Format
open Base

module Var = struct
  module T = struct
    type t = string [@@deriving eq, ord, sexp_of]

    let pp fmt var = fprintf fmt "'%s" var
  end

  include T
  include Comparator.Make (T)
end
