CLASS zfh_calculator DEFINITION
  PUBLIC FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: calculate IMPORTING input TYPE string
    RETURNING VALUE(result) type i.

    METHODS tested_method
      IMPORTING
        i_test_case_input TYPE string
      RETURNING
        value(r_result)   TYPE string.
ENDCLASS.



CLASS zfh_calculator IMPLEMENTATION.

  METHOD calculate.

  ENDMETHOD.


  METHOD tested_method.

  ENDMETHOD.

ENDCLASS.
