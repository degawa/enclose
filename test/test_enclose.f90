module test_mod_enclose
    use :: strings_enclose
    use :: testdrive, only:new_unittest, unittest_type, error_type, check
    use :: testdrive_util, only:occurred
    implicit none
    private
    public :: collect

    character(*), parameter :: paired_opening_symbols = "(<[{)>]}"
        !! opening symbols having the paired closing symbols
    character(*), parameter :: paired_closing_symbols = ")>]}(<[{"
        !! closing symbols paired with `paired_opening_symbols`
    character(*), parameter :: non_paired_symbols = &
        "1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" &
        //'!@#$%^&*-_=+\|`~;:''",./? '
        !! symbols not having the paired closing symbols

contains
    !>constructing collection of tests.
    subroutine collect(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest('`enclose(string, open, close)` returns `open//string//close`', &
                                  test_enclose_open_close) &
                     , new_unittest('`enclose(string, bracket)` returns a string ' &
                                    //'enclosed by opening symbols `bracket` ' &
                                    //'and automatically determined closing symbols', &
                                    test_enclose_autoclose) &
                     , new_unittest('`enclose(string, bracket, .false.)` returns `bracket//string//bracket`', &
                                    test_enclose_autoclose_false) &
                     , new_unittest('`enclose` handles empty strings and empty brackets', &
                                    test_enclose_empty_arguments) &
                     , new_unittest('`get_closing_symbol(opening_symbol)` returns the closing symbol ' &
                                    //'paired with an opening symbol', &
                                    test_get_closing_symbol) &
                     , new_unittest('`get_closing_symbol(opening_symbol)` returns the same symbol ' &
                                    //'when the `opening_symbol` does not have the paired closing symbol', &
                                    test_get_closing_symbol_non_paired_opening_symbol) &
                     , new_unittest('`get_closing_brackets(opening_brackets)` returns the closing brackets ' &
                                    //'paired with the opening symbols in the reverse order of the opening brackets', &
                                    test_get_closing_brackets) &
                     , new_unittest('`get_closing_brackets(opening_brackets)` returns the closing brackets ' &
                                    //'that are the same as the opening ones in the reverse order of the opening brackets', &
                                    test_get_closing_brackets_non_paired_opening_symbol) &
                     ]
    end subroutine collect

    !>checks that `actual` equals `expected` and
    !>constructs an error message containing both values on failure.
    subroutine check_str(error, actual, expected)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler
        character(*), intent(in) :: actual
            !! a value returned by the procedure under test
        character(*), intent(in) :: expected
            !! the expected value

        call check(error, actual, expected, &
                   message="actual value is "//actual//", but expected value is "//expected)
    end subroutine check_str

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

        call check_str(error, enclose("string", "[", "]"), "[string]")
        if (occurred(error)) return

        call check_str(error, enclose("string", "'[({", "})]'"), "'[({string})]'")
        if (occurred(error)) return

        ! opening and closing symbols are used as passed,
        ! even if they are not paired
        call check_str(error, enclose("string", "(", "]"), "(string]")
        if (occurred(error)) return

        call check_str(error, enclose("alphanumeric", "123abc", "123abc"), "123abcalphanumeric123abc")
        if (occurred(error)) return
    end subroutine test_enclose_open_close

    !>test the procedure `[[enclose]]` with the argument `bracket`.
    !>
    !>This test is checking that
    !>
    !>- `enclose(string, bracket)` returns a string
    !>enclosed by opening symbols `bracket`
    !>and automatically determined closing symbols
    !>- passing `autoclose=.true.` explicitly does not change the result
    !>
    subroutine test_enclose_autoclose(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check_str(error, enclose("string", "["), "[string]")
        if (occurred(error)) return

        call check_str(error, enclose("string", "'[({"), "'[({string})]'")
        if (occurred(error)) return

        ! paired and non-paired symbols can be mixed
        call check_str(error, enclose("string", "<a["), "<a[string]a>")
        if (occurred(error)) return

        call check_str(error, enclose("alphanumeric", "123abc"), "123abcalphanumericcba321")
        if (occurred(error)) return

        ! `autoclose=.true.` is the default behavior
        call check_str(error, enclose("string", "'[({", autoclose=.true.), "'[({string})]'")
        if (occurred(error)) return
    end subroutine test_enclose_autoclose

    !>test the procedure `[[enclose]]` with the arguments `bracket` and `autoclose`.
    !>
    !>This test is checking that
    !>
    !>- `enclose(string, bracket, autoclose=.false.)` returns `bracket//string//bracket`
    !>
    subroutine test_enclose_autoclose_false(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check_str(error, enclose("string", "[", .false.), "[string[")
        if (occurred(error)) return

        call check_str(error, enclose("string", "'[({", .false.), "'[({string'[({")
        if (occurred(error)) return

        call check_str(error, enclose("alphanumeric", "123abc", .false.), "123abcalphanumeric123abc")
        if (occurred(error)) return
    end subroutine test_enclose_autoclose_false

    !>test the procedure `[[enclose]]` with empty arguments.
    !>
    !>This test is checking that
    !>
    !>- `enclose` accepts a zero-length string to be enclosed
    !>- `enclose` accepts zero-length brackets
    !>
    subroutine test_enclose_empty_arguments(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        ! enclosing an empty string returns only the brackets
        call check_str(error, enclose("", "["), "[]")
        if (occurred(error)) return

        call check_str(error, enclose("", "[", "]"), "[]")
        if (occurred(error)) return

        ! enclosing with empty brackets returns the string unchanged
        call check_str(error, enclose("string", ""), "string")
        if (occurred(error)) return

        call check_str(error, enclose("string", "", ""), "string")
        if (occurred(error)) return

        call check_str(error, enclose("", ""), "")
        if (occurred(error)) return
    end subroutine test_enclose_empty_arguments

    !>test the procedure `[[get_closing_symbol]]`.
    !>
    !>This test is checking that
    !>
    !>- `get_closing_symbol(opening_symbol)` returns the closing symbol paired with an opening symbol
    !>if the closing symbol is defined.
    !>
    subroutine test_get_closing_symbol(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer :: pos

        do pos = 1, len(paired_opening_symbols)
            call check_str(error, &
                           get_closing_symbol(paired_opening_symbols(pos:pos)), &
                           paired_closing_symbols(pos:pos))
            if (occurred(error)) return
        end do
    end subroutine test_get_closing_symbol

    !>test the procedure `[[get_closing_symbol]]`.
    !>
    !>This test is checking that
    !>
    !>- `get_closing_symbol(opening_symbol)` returns the same symbol as an opening symbol
    !>as the closing symbol if the closing symbol is not defined.
    !>
    subroutine test_get_closing_symbol_non_paired_opening_symbol(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer :: pos

        do pos = 1, len(non_paired_symbols)
            call check_str(error, &
                           get_closing_symbol(non_paired_symbols(pos:pos)), &
                           non_paired_symbols(pos:pos))
            if (occurred(error)) return
        end do
    end subroutine test_get_closing_symbol_non_paired_opening_symbol

    !>test the procedure `[[get_closing_brackets]]` with the argument `opening_brackets`.
    !>
    !>This test is checking that
    !>
    !>- `get_closing_brackets(opening_brackets)` returns a string of
    !>closing symbols paired with opening ones
    !>in the reverse order of the opening brackets.
    !>
    subroutine test_get_closing_brackets(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check_str(error, get_closing_brackets("["), "]")
        if (occurred(error)) return

        call check_str(error, get_closing_brackets("[({<"), ">})]")
        if (occurred(error)) return

        ! paired and non-paired symbols can be mixed
        call check_str(error, get_closing_brackets("<a["), "]a>")
        if (occurred(error)) return

        ! an empty string returns an empty string
        call check_str(error, get_closing_brackets(""), "")
        if (occurred(error)) return
    end subroutine test_get_closing_brackets

    !>test the procedure `[[get_closing_brackets]]` with the argument `opening_brackets`.
    !>
    !>This test is checking that
    !>
    !>- `get_closing_brackets(opening_brackets)` returns a string of
    !>closing symbols that are the same as the opening ones
    !>in the reverse order of the opening brackets.
    !>
    subroutine test_get_closing_brackets_non_paired_opening_symbol(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check_str(error, &
                       get_closing_brackets("abcdefghijklmnopqrstuvwxyz"), &
                       "zyxwvutsrqponmlkjihgfedcba")
        if (occurred(error)) return

        call check_str(error, get_closing_brackets("1234567890"), "0987654321")
        if (occurred(error)) return

        call check_str(error, &
                       get_closing_brackets('!@#$%^&*-_=+\|`~;:"'//"',./?"), &
                       "?/.,'"//'":;~`|\+=_-*&^%$#@!')
        if (occurred(error)) return
    end subroutine test_get_closing_brackets_non_paired_opening_symbol
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
