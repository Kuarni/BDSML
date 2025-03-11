open Middleend.Anf_ast
open My_llvm
open Llvm

let () =
  let compiled4 =
    Codegen.compile_program
      ~verbose:true
      [ AbsStr_value
          ( "x"
          , LComplex
              (CExp_apply ("__op_plus", [ AExp_constant (Const_int 52); AExp_ident "n" ]))
          )
      ]
  in
  ignore compiled4
;;
