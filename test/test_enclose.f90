module test_mod_enclose
    use :: strings_enclose
    use :: testdrive, only:new_unittest, unittest_type, error_type, check
    use :: testdrive_util, only:occurred
    implicit none
    private
    public :: collect

contains
    !>constructing collection of tests.
    subroutine collect(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest('`enclose(string, open, close)` returns `open//string//close`', &
                                  test_enclose_open_close) &
                     ]
    end subroutine collect

    !>test the procedure `[[enclose]]` with the arguments `open` and `close`.
    !>
    !>This test is checking that
    !>
    !>- `enclose(string, open, close)` returns the string `open//string//close`
    !>
    subroutine test_enclose_open_close(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: str_enclosed

        str_enclosed = enclose("string", "[", "]")
        call check(error, str_enclosed, "[string]", &
                   message="expected value "//str_enclosed//" is not the actual value [string]")

        str_enclosed = enclose("string", "'[({", "})]'")
        call check(error, str_enclosed, "'[({string})]'", &
                   message="expected value "//str_enclosed//" is not the actual value '[({string})]'")

        str_enclosed = enclose("alphanumeric", "123abc", "123abc")
        call check(error, str_enclosed, "123abcalphanumeric123abc", &
                   message="expected value "//str_enclosed//" is not the actual value 123abcalphanumeric123abc")
    end subroutine test_enclose_open_close

end module test_mod_enclose

program test_enclose
    use :: test_mod_enclose
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)

    test_suites = [ &
                  new_testsuite("enclose", collect) &
                  ]
    call run_test(test_suites)
end program test_enclose
