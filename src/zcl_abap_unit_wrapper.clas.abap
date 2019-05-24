CLASS zcl_abap_unit_wrapper DEFINITION PUBLIC FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:
      "! Severity
      BEGIN OF severity,
        low    TYPE int1 VALUE if_aunit_constants=>severity-low,
        medium TYPE int1 VALUE if_aunit_constants=>severity-medium,
        high   TYPE int1 VALUE if_aunit_constants=>severity-high,
      END OF severity,

      "! Control flow
      BEGIN OF quit,
        "! Raise failure and continue with test
        no   TYPE int1 VALUE if_aunit_constants=>quit-no,
        "! Raise failure and exit the current test
        test TYPE int1 VALUE if_aunit_constants=>quit-test,
      END OF quit.

    "! Default Tolerance for comparison type F
    CONSTANTS rtol_default TYPE f VALUE cl_abap_unit_assert=>rtol_default.

    CLASS-METHODS:
      "! Assert the validity of the reference
      "!
      "! @parameter act              | Reference variable to be checked
      "! @parameter msg              | Description
      "! @parameter level            | Severity (if_Aunit_Constants=>severity)
      "! @parameter quit             | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_bound
        IMPORTING VALUE(act)              TYPE any
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
        RETURNING VALUE(assertion_failed) TYPE abap_bool,

      "! Assert that character string fits to simple pattern
      "!
      "! @parameter act              | Actual Object
      "! @parameter exp              | Expected Template
      "! @parameter msg              | Message in Case of Error
      "! @parameter level            | Severity (if_Aunit_Constants=>severity)
      "! @parameter quit             | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_char_cp
        IMPORTING act                     TYPE csequence
                  exp                     TYPE csequence
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
        RETURNING VALUE(assertion_failed) TYPE abap_bool,

      "! Assert that character string does not fit to simple pattern
      "!
      "! @parameter act              | Actual text which shall not adhere to EXP pattern
      "! @parameter exp              | Simple text pattern
      "! @parameter msg              | Description
      "! @parameter level            | Severity (see if_Aunit_Constants=>severity)
      "! @parameter quit             | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_char_np
        IMPORTING VALUE(act)              TYPE csequence
                  exp                     TYPE csequence
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
        RETURNING VALUE(assertion_failed) TYPE abap_bool,

      "! Assert tuat 2 elementary data objects differ
      "!
      "! @parameter act              | Data object with current value
      "! @parameter exp              | Compare object with unexpected value
      "! @parameter tol              | Tolerance range for floating point comparison
      "! @parameter msg              | Message in case of error
      "! @parameter level            | Severity (see if_Aunit_Constants=>severity)
      "! @parameter quit             | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_differs
        IMPORTING VALUE(act)              TYPE simple
                  VALUE(exp)              TYPE simple
                  tol                     TYPE f OPTIONAL
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
        RETURNING VALUE(assertion_failed) TYPE abap_bool,

      "! Assert equality of two data objects
      "!
      "! @parameter act                  | Data object with current value
      "! @parameter exp                  | Data object with expected type
      "! @parameter ignore_Hash_Sequence | Ignore sequence in hash tables
      "! @parameter tol                  | Tolerance Range (for directly passed floating numbers)
      "! @parameter msg                  | Description
      "! @parameter level                | Severity (see if_Aunit_Constants=>severity)
      "! @parameter quit                 | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter assertion_Failed     | Condition was not met (and QUIT = NO)
      assert_equals
        IMPORTING VALUE(act)              TYPE any
                  VALUE(exp)              TYPE any
                  ignore_hash_sequence    TYPE abap_bool DEFAULT abap_false
                  tol                     TYPE f OPTIONAL
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
        RETURNING VALUE(assertion_failed) TYPE abap_bool,


      "! Assert approximate consistency of 2 floating point numbers
      "!
      "! @parameter act              | Data object with current value
      "! @parameter exp              | Data object with expected value
      "! @parameter rtol             | Relative tolerance
      "! @parameter msg              | Description
      "! @parameter level            | Severity (see if_Aunit_Constants=>severity)
      "! @parameter quit             | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_equals_float
        IMPORTING VALUE(act)              TYPE numeric
                  VALUE(exp)              TYPE numeric
                  rtol                    TYPE numeric DEFAULT rtol_default
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
        RETURNING VALUE(assertion_failed) TYPE abap_bool,

      "! Assert that boolean equals ABAP_FALSE
      "!
      "! @parameter act              | Actual data object
      "! @parameter msg              | Description
      "! @parameter level            | Severity (see if_Aunit_Constants=>severity)
      "! @parameter quit             | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_false
        IMPORTING VALUE(act)              TYPE abap_bool
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
        RETURNING VALUE(assertion_failed) TYPE abap_bool,

      "! Assert that data object value is initial
      "!
      "! @parameter act              | Actual data object
      "! @parameter msg              | Description
      "! @parameter level            | Severity (see if_Aunit_Constants=>severity)
      "! @parameter quit             | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_initial
        IMPORTING VALUE(act)              TYPE any DEFAULT sy-subrc
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
                    PREFERRED PARAMETER act
        RETURNING
                  VALUE(assertion_failed) TYPE abap_bool,

      "! Assert that refererence is initial or not valid
      "!
      "! @parameter act              | Reference variable to be checked
      "! @parameter msg              | Description
      "! @parameter level            | Severity (see if_Aunit_Constants=>severity)
      "! @parameter quit             | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_not_bound
        IMPORTING VALUE(act)              TYPE any
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
        RETURNING VALUE(assertion_failed) TYPE abap_bool,

      "! Assert that value of data object is not initial
      "!
      "! @parameter act              | Actual Data Object
      "! @parameter msg              | Message in Case of Error
      "! @parameter level            | Severity (see if_Aunit_Constants=>severity)
      "! @parameter quit             | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_not_initial
        IMPORTING VALUE(act)              TYPE any
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
        RETURNING
                  VALUE(assertion_failed) TYPE abap_bool,

      "! Assert that number is in given range
      "!
      "! @parameter lower            | Upper boundary
      "! @parameter upper            | Lower boundary
      "! @parameter number           | Number expected to be within the boundaries
      "! @parameter msg              | Description
      "! @parameter level            | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter quit             | Control flow in case of failed assertion
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_number_between
        IMPORTING lower                   TYPE numeric
                  upper                   TYPE numeric
                  number                  TYPE numeric
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
        RETURNING VALUE(assertion_failed) TYPE abap_bool,


      "! Assert specific value of return code
      "!
      "! @parameter exp              | Expected return code
      "! @parameter act              | Return code. default sy-msg
      "! @parameter msg              | Description
      "! @parameter level            | Severity (see if_Aunit_Constants=>severity)
      "! @parameter quit             | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter symsg            | System message
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_return_code
        IMPORTING VALUE(exp)              TYPE numeric
                  VALUE(act)              TYPE numeric DEFAULT sy-subrc
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
                  symsg                   TYPE symsg OPTIONAL
        RETURNING VALUE(assertion_failed) TYPE abap_bool,


      "! Assert specific value of return code
      "!
      "! @parameter exp              | Expected return code, optional, if not zero
      "! @parameter act              | Return code of ABAP statements
      "! @parameter msg              | Description
      "! @parameter level            | Severity (see if_Aunit_Constants=>severity)
      "! @parameter quit             | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter symsg            | System message
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_subrc
        IMPORTING VALUE(exp)              TYPE sysubrc DEFAULT 0
                  VALUE(act)              TYPE sysubrc DEFAULT sy-subrc
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
                  symsg                   TYPE symsg OPTIONAL
                    PREFERRED PARAMETER act
        RETURNING VALUE(assertion_failed) TYPE abap_bool,

      "! Assert that data is contained as line within internal table
      "!
      "! @parameter line             | Data Object that is typed like line of TABLE
      "! @parameter table            | Internal Table that shall contain LINE
      "! @parameter msg              | Description
      "! @parameter level            | Severity (see if_Aunit_Constants=>severity)
      "! @parameter quit             | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_table_contains
        IMPORTING VALUE(line)             TYPE any
                  !table                  TYPE ANY TABLE
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
        RETURNING VALUE(assertion_failed) TYPE abap_bool,

      "! Assert that data is not contained as line in internal table
      "!
      "! @parameter line             | Data Object that is typed like line of TABLE
      "! @parameter table            | Internal Table that must not contain LINE
      "! @parameter msg              | Description
      "! @parameter level            | Severity (see if_Aunit_Constants=>severity)
      "! @parameter quit             | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_table_not_contains
        IMPORTING VALUE(line)             TYPE any
                  !table                  TYPE ANY TABLE
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
        RETURNING VALUE(assertion_failed) TYPE abap_bool,

      "! Assert that text matches regular expression
      "!
      "! @parameter pattern          | Regular Expression - see also TA ABAPHELP
      "! @parameter text             | Text that is assumed to met the regular expression
      "! @parameter msg              | Description
      "! @parameter level            | Severity (see if_Aunit_Constants=>severity)
      "! @parameter quit             | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_text_matches
        IMPORTING VALUE(pattern)          TYPE csequence
                  VALUE(text)             TYPE csequence
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
        RETURNING VALUE(assertion_failed) TYPE abap_bool,

      "! Assert that a constraint is met by data object
      "!
      "! @parameter act              | Data object which should adhere to constraint EXP
      "! @parameter act_As_Text      | Description for ACT that is used in alert message text
      "! @parameter exp              | Constraint to which ACT needs to adhere
      "! @parameter msg              | Description
      "! @parameter level            | Severity (see if_Aunit_Constants=>severity)
      "! @parameter quit             | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_that
        IMPORTING VALUE(act)              TYPE data
                  VALUE(act_as_text)      TYPE csequence OPTIONAL
                  exp                     TYPE REF TO if_constraint
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
        RETURNING VALUE(assertion_failed) TYPE abap_bool,


      "! Assert that boolean equals ABAP_TRUE
      "!
      "! @parameter act              | Actual value
      "! @parameter msg              | Description
      "! @parameter level            | Severity (see if_Aunit_Constants=>severity)
      "! @parameter quit             | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter assertion_Failed | Condition was not met (and QUIT = NO)
      assert_true
        IMPORTING VALUE(act)              TYPE abap_bool
                  msg                     TYPE csequence OPTIONAL
                  level                   TYPE int1 DEFAULT severity-medium
                  quit                    TYPE int1 DEFAULT quit-test
        RETURNING VALUE(assertion_failed) TYPE abap_bool,

      "! Ensure that boolean equals ABAP_FALSE, skip test else
      "! Tests can use this method to ensure that the prerequisites of the test are met. If not,
      "! the test execution gets aborted without raising an assertion
      "!
      "! @parameter act              | Actual value
      "! @parameter msg              | Description
      assume_false
        IMPORTING VALUE(act) TYPE abap_bool
                  msg        TYPE csequence,

      "! Ensure specific value of return code
      "! Tests can use this method to ensure that the prerequisites of the test are met. If not,
      "! the test execution gets aborted without raising an assertion
      "!
      "! @parameter exp              | Expected return code
      "! @parameter act              | Actual return code
      "! @parameter msg              | Description
      "! @parameter symsg            | System message
      assume_return_code
        IMPORTING VALUE(exp) TYPE numeric
                  VALUE(act) TYPE numeric  DEFAULT sy-subrc
                  msg        TYPE csequence
                  symsg      TYPE symsg OPTIONAL,


      "! Ensure constraint is met data object
      "! Tests can use this method to ensure that the prerequisites of the test are met. If not,
      "! the test execution gets aborted without raising an assertion
      "!
      "! @parameter act              | Data object which should adhere to constraint EXP
      "! @parameter act_As_Text      | Description for ACT that is used in alert
      "! @parameter exp              | Constraint to which ACT shall adhere
      assume_that
        IMPORTING VALUE(act)         TYPE data
                  VALUE(act_as_text) TYPE csequence OPTIONAL
                  exp                TYPE REF TO if_constraint
                  msg                TYPE csequence OPTIONAL,


      "! Ensure that boolean equals ABAP_TRUE, skip test else
      "! Tests can use this method to ensure that the prerequisites of the test are met. If not,
      "! the test execution gets aborted without raising an assertion
      "!
      "! @parameter act              | Actual value
      "! @parameter msg              | Description
      assume_true
        IMPORTING VALUE(act) TYPE abap_bool
                  msg        TYPE csequence,

      "! Unconditional assertion
      "!
      "! @parameter msg    | Description
      "! @parameter level  | Severity (see if_Aunit_Constants=>severity)
      "! @parameter quit   | Alter control flow/ quit test (see if_Aunit_Constants=>quit)
      "! @parameter detail | Further Description
      fail
        IMPORTING msg    TYPE csequence OPTIONAL
                  level  TYPE int1 DEFAULT severity-medium
                  quit   TYPE int1 DEFAULT quit-test
                  detail TYPE csequence OPTIONAL
                    PREFERRED PARAMETER msg,


      "! Skip test due to missing prerequisites
      "! Test execution gets aborted without raising an assertion
      "! @parameter msg    | Description
      "! @parameter detail | More detailed description
      skip
        IMPORTING msg    TYPE csequence
                  detail TYPE csequence OPTIONAL.

ENDCLASS.



CLASS zcl_abap_unit_wrapper IMPLEMENTATION.

  METHOD assert_bound.
    assertion_failed = cl_abap_unit_assert=>assert_bound(
      act = act
      msg = msg
      level = level
      quit = quit ).
  ENDMETHOD.


  METHOD assert_char_cp.
    assertion_failed = cl_abap_unit_assert=>assert_char_cp(
     act = act
     exp = exp
     level = level
     msg = msg
     quit = quit ).
  ENDMETHOD.


  METHOD assert_char_np.
    assertion_failed = cl_abap_unit_assert=>assert_char_np(
      act = act
      exp = exp
      level = level
      msg = msg
      quit = quit ).
  ENDMETHOD.


  METHOD assert_differs.
    assertion_failed = cl_abap_unit_assert=>assert_differs(
      act = act
      exp = exp
      tol = tol
      level = level
      msg = msg
      quit = quit ).
  ENDMETHOD.


  METHOD assert_equals.
    assertion_failed = cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp
      tol = tol
      level = level
      msg = msg
      quit = quit
      ignore_hash_sequence = ignore_hash_sequence ).
  ENDMETHOD.


  METHOD assert_equals_float.
    assertion_failed = cl_abap_unit_assert=>assert_equals_float(
      act = act
      exp = exp
      rtol = rtol
      level = level
      msg = msg
      quit = quit ).
  ENDMETHOD.


  METHOD assert_false.
    assertion_failed = cl_abap_unit_assert=>assert_false(
      act = act
      level = level
      msg = msg
      quit = quit ).
  ENDMETHOD.


  METHOD assert_initial.
    assertion_failed = cl_abap_unit_assert=>assert_initial(
      act = act
      level = level
      msg = msg
      quit = quit ).
  ENDMETHOD.


  METHOD assert_not_bound.
    assertion_failed = cl_abap_unit_assert=>assert_not_bound(
      act = act
      level = level
      msg = msg
      quit = quit ).
  ENDMETHOD.


  METHOD assert_not_initial.
    assertion_failed = cl_abap_unit_assert=>assert_not_initial(
      act = act
      level = level
      msg = msg
      quit = quit ).
  ENDMETHOD.


  METHOD assert_number_between.
    assertion_failed = cl_abap_unit_assert=>assert_number_between(
      number = number
      lower = lower
      upper = upper
      level = level
      msg = msg
      quit = quit ).
  ENDMETHOD.


  METHOD assert_return_code.
    assertion_failed = cl_abap_unit_assert=>assert_return_code(
      act = act
      exp = exp
      level = level
      msg = msg
      quit = quit
      symsg = symsg ).
  ENDMETHOD.


  METHOD assert_subrc.
    assertion_failed = cl_abap_unit_assert=>assert_subrc(
      act = act
      exp = exp
      level = level
      msg = msg
      quit = quit
      symsg = symsg ).
  ENDMETHOD.


  METHOD assert_table_contains.
    assertion_failed = cl_abap_unit_assert=>assert_table_contains(
      line = line
      table = table
      level = level
      msg = msg
      quit = quit ).
  ENDMETHOD.


  METHOD assert_table_not_contains.
    assertion_failed = cl_abap_unit_assert=>assert_table_not_contains(
      line = line
      table = table
      level = level
      msg = msg
      quit = quit ).
  ENDMETHOD.


  METHOD assert_text_matches.
    assertion_failed = cl_abap_unit_assert=>assert_text_matches(
        text = text
        pattern = pattern
        level = level
        msg = msg
        quit = quit ).
  ENDMETHOD.


  METHOD assert_that.
    assertion_failed = cl_abap_unit_assert=>assert_that(
      act = act
      act_as_text = act_as_text
      exp = exp
      level = level
      msg = msg
      quit = quit ).
  ENDMETHOD.


  METHOD assert_true.
    assertion_failed = cl_abap_unit_assert=>assert_true(
      act = act
      level = level
      msg = msg
      quit = quit ).
  ENDMETHOD.


  METHOD assume_false.
    cl_abap_unit_assert=>assume_false(
      act = act
      msg = msg ).
  ENDMETHOD.


  METHOD assume_return_code.
    cl_abap_unit_assert=>assume_return_code(
      act = act
      exp = exp
      msg = msg
      symsg = symsg ).
  ENDMETHOD.


  METHOD assume_that.
    cl_abap_unit_assert=>assume_that(
      act = act
      act_as_text = act_as_text
      exp = exp
      msg = msg ).
  ENDMETHOD.


  METHOD assume_true.
    cl_abap_unit_assert=>assume_true(
      act = act
      msg = msg ).
  ENDMETHOD.


  METHOD fail.
    cl_abap_unit_assert=>fail(
      detail = detail
      level = level
      msg = msg
      quit = quit ).
  ENDMETHOD.


  METHOD skip.
    cl_abap_unit_assert=>skip(
      detail = detail
      msg = msg ).
  ENDMETHOD.


ENDCLASS.
