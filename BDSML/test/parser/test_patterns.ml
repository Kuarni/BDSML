let test_pattern =
  Test_utils.pp_parse_result Parser.Pattern_parser.parse_pattern Parser.Ast.pp_pattern
;;

let%expect_test "test any" =
  test_pattern "_";
  [%expect {| Pat_any |}]
;;

let%expect_test "test const int" =
  test_pattern "   5";
  [%expect {| (Pat_constant (Const_int 5)) |}]
;;

let%expect_test "test const string" =
  test_pattern " \"abraacadabra\"";
  [%expect {| (Pat_constant (Const_string "abraacadabra")) |}]
;;

let%expect_test "test const char" =
  test_pattern " (((\'Z\')))";
  [%expect {| (Pat_constant (Const_char 'Z')) |}]
;;

let%expect_test "test const char" =
  test_pattern " \'Z\' | \'V\'";
  [%expect
    {| (Pat_or ((Pat_constant (Const_char 'Z')), (Pat_constant (Const_char 'V')))) |}]
;;

let%expect_test "test tuple" =
  test_pattern "(((1), 2, (((3))), ((4))))";
  [%expect
    {|
    (Pat_tuple
       [(Pat_constant (Const_int 1)); (Pat_constant (Const_int 2));
         (Pat_constant (Const_int 3)); (Pat_constant (Const_int 4))])|}]
;;

let%expect_test "test list 1" =
  test_pattern "[1; 2; 3]";
  [%expect
    {|
    (Pat_construct ("::",
       (Some (Pat_tuple
                [(Pat_constant (Const_int 1));
                  (Pat_construct ("::",
                     (Some (Pat_tuple
                              [(Pat_constant (Const_int 2));
                                (Pat_construct ("::",
                                   (Some (Pat_tuple
                                            [(Pat_constant (Const_int 3));
                                              (Pat_construct ("[]", None))]))
                                   ))
                                ]))
                     ))
                  ]))
       ))
    |}]
;;

let%expect_test "test list 2" =
  test_pattern "((1) :: (2) :: [])";
  [%expect
    {|
    (Pat_construct ("::",
       (Some (Pat_tuple
                [(Pat_constant (Const_int 1));
                  (Pat_construct ("::",
                     (Some (Pat_tuple
                              [(Pat_constant (Const_int 2));
                                (Pat_construct ("[]", None))]))
                     ))
                  ]))
       ))
    |}]
;;

let%expect_test "test list 3" =
  test_pattern "[ [1] ]";
  [%expect
    {|
    (Pat_construct ("::",
       (Some (Pat_tuple
                [(Pat_construct ("::",
                    (Some (Pat_tuple
                             [(Pat_constant (Const_int 1));
                               (Pat_construct ("[]", None))]))
                    ));
                  (Pat_construct ("[]", None))]))
       ))
    |}]
;;

let%expect_test "test list 4" =
  test_pattern "_ :: [[2; 4]; [3]]";
  [%expect
    {|
    (Pat_construct ("::",
       (Some (Pat_tuple
                [Pat_any;
                  (Pat_construct ("::",
                     (Some (Pat_tuple
                              [(Pat_construct ("::",
                                  (Some (Pat_tuple
                                           [(Pat_constant (Const_int 2));
                                             (Pat_construct ("::",
                                                (Some (Pat_tuple
                                                         [(Pat_constant
                                                             (Const_int 4));
                                                           (Pat_construct ("[]",
                                                              None))
                                                           ]))
                                                ))
                                             ]))
                                  ));
                                (Pat_construct ("::",
                                   (Some (Pat_tuple
                                            [(Pat_construct ("::",
                                                (Some (Pat_tuple
                                                         [(Pat_constant
                                                             (Const_int 3));
                                                           (Pat_construct ("[]",
                                                              None))
                                                           ]))
                                                ));
                                              (Pat_construct ("[]", None))]))
                                   ))
                                ]))
                     ))
                  ]))
       ))
    |}]
;;

let%expect_test "or test" =
  test_pattern "1 | 2 :: 1, 2, 3";
  [%expect
    {|
    (Pat_or ((Pat_constant (Const_int 1)),
       (Pat_construct ("::",
          (Some (Pat_tuple
                   [(Pat_constant (Const_int 2));
                     (Pat_tuple
                        [(Pat_constant (Const_int 1));
                          (Pat_constant (Const_int 2));
                          (Pat_constant (Const_int 3))])
                     ]))
          ))
       ))
    |}]
;;

let%expect_test "or inside of list test" =
  test_pattern "[1|2; 3]";
  [%expect
    {|
    (Pat_construct ("::",
       (Some (Pat_tuple
                [(Pat_or ((Pat_constant (Const_int 1)),
                    (Pat_constant (Const_int 2))));
                  (Pat_construct ("::",
                     (Some (Pat_tuple
                              [(Pat_constant (Const_int 3));
                                (Pat_construct ("[]", None))]))
                     ))
                  ]))
       ))
    |}]
;;

let%expect_test "type any test" =
  test_pattern "_:int";
  [%expect {| (Pat_type (Pat_any, (Type_single "int"))) |}]
;;

let%expect_test "type var test" =
  test_pattern "a:int";
  [%expect {| (Pat_type ((Pat_var "a"), (Type_single "int"))) |}]
;;

let%expect_test "type tuple test" =
  test_pattern "(1, 2, 3, 4:int)";
  [%expect
    {|
    (Pat_type (
       (Pat_tuple
          [(Pat_constant (Const_int 1)); (Pat_constant (Const_int 2));
            (Pat_constant (Const_int 3)); (Pat_constant (Const_int 4))]),
       (Type_single "int")))
    |}]
;;

let%expect_test "type list test" =
  test_pattern "[1;2]:int";
  [%expect
    {|
    (Pat_type (
       (Pat_construct ("::",
          (Some (Pat_tuple
                   [(Pat_constant (Const_int 1));
                     (Pat_construct ("::",
                        (Some (Pat_tuple
                                 [(Pat_constant (Const_int 2));
                                   (Pat_construct ("[]", None))]))
                        ))
                     ]))
          )),
       (Type_single "int")))
    |}]
;;

let%expect_test "type or test" =
  test_pattern "1|2:int";
  [%expect
    {|
    (Pat_type (
       (Pat_or ((Pat_constant (Const_int 1)), (Pat_constant (Const_int 2)))),
       (Type_single "int")))
    |}]
;;
