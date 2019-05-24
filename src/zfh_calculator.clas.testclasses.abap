CLASS lcl_tests DEFINITION FINAL FOR TESTING
  INHERITING FROM zcl_abap_unit_wrapper
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: f_cut TYPE REF TO zfh_calculator.

    METHODS:
      setup,
      test_addition FOR TESTING RAISING cx_static_check,
      test_subtraction FOR TESTING RAISING cx_static_check,
      test_multiplication FOR TESTING RAISING cx_static_check,
      test_division FOR TESTING RAISING cx_static_check,

      test_calculate FOR TESTING RAISING cx_static_check,

      test_calculate_no_quit FOR TESTING RAISING cx_static_check,

      test_cases FOR TESTING RAISING cx_static_check,

      test_cases_pattern FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS lcl_tests IMPLEMENTATION.

  METHOD setup.
    f_cut = NEW #( ).
  ENDMETHOD.

  METHOD test_addition.

    " given
    DATA(input) = `2 + 2`.

    " when
    DATA(result) = zfh_calculator=>calculate( input ).

    " then
    assert_equals( exp = 4 act = result ).

  ENDMETHOD.

  METHOD test_subtraction.

    " given
    DATA(input) = `2 - 2`.

    " when
    DATA(result) = zfh_calculator=>calculate( input ).

    " then
    assert_equals( exp = 0 act = result ).

  ENDMETHOD.

  METHOD test_multiplication.

    " given
    DATA(input) = `2 * 3`.

    " when
    DATA(result) = zfh_calculator=>calculate( input ).

    " then
    assert_equals( exp = 6 act = result ).

  ENDMETHOD.

  METHOD test_division.

    " given
    DATA(input) = `4 / 2`.

    " when
    DATA(result) = zfh_calculator=>calculate( input ).

    " then
    assert_equals( exp = 4 act = result ).

  ENDMETHOD.

  METHOD test_calculate.
    assert_equals(
      act = zfh_calculator=>calculate( `2+2` )
      exp = 4 ).

    assert_equals(
      act = zfh_calculator=>calculate( `2-2` )
      exp = 0 ).

    assert_equals(
      act = zfh_calculator=>calculate( `2*3` )
      exp = 6 ).

    assert_equals(
      act = zfh_calculator=>calculate( `4/2` )
      exp = 2 ).
  ENDMETHOD.

  METHOD test_calculate_no_quit.
    assert_equals(
      act = zfh_calculator=>calculate( `2+2` )
      exp = 4
      quit = if_aunit_constants=>quit-no ).

    assert_equals(
      act = zfh_calculator=>calculate( `2-2` )
      exp = 0
      quit = if_aunit_constants=>quit-no ).

    assert_equals(
      act = zfh_calculator=>calculate( `2*3` )
      exp = 6
      quit = if_aunit_constants=>quit-no ).

    assert_equals(
      act = zfh_calculator=>calculate( `4/2` )
      exp = 2
      quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD test_cases.
    TYPES:
      BEGIN OF test_case,
        input TYPE string,
        exp   TYPE i,
      END OF test_case,
      test_cases TYPE HASHED TABLE OF test_case WITH UNIQUE KEY input.

    " given
    DATA(test) = VALUE test_cases(
      ( input = `2 + 2` exp = 4 )
      ( input = `2 - 2` exp = 0 )
      ( input = `2 * 2` exp = 4 )
      ( input = `2 / 2` exp = 1 )
     ).

    LOOP AT test ASSIGNING FIELD-SYMBOL(<test_case>).

      " when
      DATA(result) = f_cut->calculate( <test_case>-input ).

      " then
      assert_equals(
        exp = <test_case>-exp
        act = result
        msg = |Failed at { <test_case>-input }. Expected { <test_case>-exp }, received { result }|
        quit = if_aunit_constants=>quit-no ).

    ENDLOOP.

  ENDMETHOD.

  METHOD test_cases_pattern.

    TYPES:
      BEGIN OF test_case,
        input TYPE string,
        exp   TYPE string,
      END OF test_case,
      test_cases TYPE HASHED TABLE OF test_case WITH UNIQUE KEY input.

    DATA(test) = VALUE test_cases(
      ( input = `a` exp = `b` )
     ).

    LOOP AT test ASSIGNING FIELD-SYMBOL(<test_case>).
      DATA(result) = f_cut->tested_method( <test_case>-input ).
      assert_equals(
        exp = <test_case>-exp
        act = result
        msg = |Failed at { <test_case>-input }.| &&
              |Expected { <test_case>-exp }, received { result }.|
        quit = if_aunit_constants=>quit-no ).
    ENDLOOP.

  ENDMETHOD.




ENDCLASS.
