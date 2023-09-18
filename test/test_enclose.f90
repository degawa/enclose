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
                     , new_unittest('`enclose(string, bracket)` returns a string ' &
                                    //'enclosed by opening symbols `bracket`' &
                                    //'and automatically determined closing symbols', &
                                    test_enclose_autoclose) &
                     , new_unittest('`enclose(string, bracket, .false.)` returns `bracket//string//bracket`', &
                                    test_enclose_autoclose_false) &
                     , new_unittest('`get_closing_symbol(opening_symbol)` returns the closing symbol ' &
                                    //'paired with an opening symbol', &
                                    test_get_closing_symbol) &
                     , new_unittest('`get_closing_symbol(opening_symbol)` returns the same symbol ' &
                                    //'when the `opening_symbol` does not have the paired opening symbol', &
                                    test_get_closing_symbol_non_paired_opening_symbol) &
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
        if (occurred(error)) return

        str_enclosed = enclose("string", "'[({", "})]'")
        call check(error, str_enclosed, "'[({string})]'", &
                   message="expected value "//str_enclosed//" is not the actual value '[({string})]'")
        if (occurred(error)) return

        str_enclosed = enclose("alphanumeric", "123abc", "123abc")
        call check(error, str_enclosed, "123abcalphanumeric123abc", &
                   message="expected value "//str_enclosed//" is not the actual value 123abcalphanumeric123abc")
        if (occurred(error)) return
    end subroutine test_enclose_open_close

    !>test the procedure `[[enclose]]` with the argument `bracket`.
    !>
    !>This test is checking that
    !>
    !>- `enclose(string, bracket)` returns returns a string
    !>enclosed by opening symbols `bracket`
    !>and automatically determined closing symbols
    !>
    subroutine test_enclose_autoclose(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: str_enclosed

        str_enclosed = enclose("string", "[")
        call check(error, str_enclosed, "[string]", &
                   message="expected value "//str_enclosed//" is not the actual value [string]")
        if (occurred(error)) return

        str_enclosed = enclose("string", "'[({")
        call check(error, str_enclosed, "'[({string})]'", &
                   message="expected value "//str_enclosed//" is not the actual value '[({string})]'")
        if (occurred(error)) return

        str_enclosed = enclose("alphanumeric", "123abc")
        call check(error, str_enclosed, "123abcalphanumericcba321", &
                   message="expected value "//str_enclosed//" is not the actual value 123abcalphanumericcba321")
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

        character(:), allocatable :: str_enclosed

        str_enclosed = enclose("string", "[", .false.)
        call check(error, str_enclosed, "[string[", &
                   message="expected value "//str_enclosed//" is not the actual value [string[")
        if (occurred(error)) return

        str_enclosed = enclose("string", "'[({", .false.)
        call check(error, str_enclosed, "'[({string'[({", &
                   message="expected value "//str_enclosed//" is not the actual value '[({string'[({")
        if (occurred(error)) return

        str_enclosed = enclose("alphanumeric", "123abc", .false.)
        call check(error, str_enclosed, "123abcalphanumeric123abc", &
                   message="expected value "//str_enclosed//" is not the actual value 123abcalphanumeric123abc")
        if (occurred(error)) return
    end subroutine test_enclose_autoclose_false

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

        character(:), allocatable :: closing_symbol

        closing_symbol = get_closing_symbol("(")
        call check(error, closing_symbol, ")", &
                   message="expected value "//closing_symbol//" is not the actual value )")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("<")
        call check(error, closing_symbol, ">", &
                   message="expected value "//closing_symbol//" is not the actual value >")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("[")
        call check(error, closing_symbol, "]", &
                   message="expected value "//closing_symbol//" is not the actual value ]")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("{")
        call check(error, closing_symbol, "}", &
                   message="expected value "//closing_symbol//" is not the actual value }")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol(")")
        call check(error, closing_symbol, "(", &
                   message="expected value "//closing_symbol//" is not the actual value (")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol(">")
        call check(error, closing_symbol, "<", &
                   message="expected value "//closing_symbol//" is not the actual value <")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("]")
        call check(error, closing_symbol, "[", &
                   message="expected value "//closing_symbol//" is not the actual value [")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("}")
        call check(error, closing_symbol, "{", &
                   message="expected value "//closing_symbol//" is not the actual value {")
        if (occurred(error)) return
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

        character(:), allocatable :: closing_symbol

        closing_symbol = get_closing_symbol("1")
        call check(error, closing_symbol, "1", &
                   message="expected value "//closing_symbol//" is not the actual value 1")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("2")
        call check(error, closing_symbol, "2", &
                   message="expected value "//closing_symbol//" is not the actual value 2")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("3")
        call check(error, closing_symbol, "3", &
                   message="expected value "//closing_symbol//" is not the actual value 3")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("4")
        call check(error, closing_symbol, "4", &
                   message="expected value "//closing_symbol//" is not the actual value 4")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("5")
        call check(error, closing_symbol, "5", &
                   message="expected value "//closing_symbol//" is not the actual value 5")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("6")
        call check(error, closing_symbol, "6", &
                   message="expected value "//closing_symbol//" is not the actual value <")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("7")
        call check(error, closing_symbol, "7", &
                   message="expected value "//closing_symbol//" is not the actual value 7")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("8")
        call check(error, closing_symbol, "8", &
                   message="expected value "//closing_symbol//" is not the actual value 8")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("9")
        call check(error, closing_symbol, "9", &
                   message="expected value "//closing_symbol//" is not the actual value 9")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("0")
        call check(error, closing_symbol, "0", &
                   message="expected value "//closing_symbol//" is not the actual value 0")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("a")
        call check(error, closing_symbol, "a", &
                   message="expected value "//closing_symbol//" is not the actual value a")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("b")
        call check(error, closing_symbol, "b", &
                   message="expected value "//closing_symbol//" is not the actual value b")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("c")
        call check(error, closing_symbol, "c", &
                   message="expected value "//closing_symbol//" is not the actual value c")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("d")
        call check(error, closing_symbol, "d", &
                   message="expected value "//closing_symbol//" is not the actual value d")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("e")
        call check(error, closing_symbol, "e", &
                   message="expected value "//closing_symbol//" is not the actual value e")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("f")
        call check(error, closing_symbol, "f", &
                   message="expected value "//closing_symbol//" is not the actual value f")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("g")
        call check(error, closing_symbol, "g", &
                   message="expected value "//closing_symbol//" is not the actual value g")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("h")
        call check(error, closing_symbol, "h", &
                   message="expected value "//closing_symbol//" is not the actual value h")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("i")
        call check(error, closing_symbol, "i", &
                   message="expected value "//closing_symbol//" is not the actual value i")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("j")
        call check(error, closing_symbol, "j", &
                   message="expected value "//closing_symbol//" is not the actual value j")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("k")
        call check(error, closing_symbol, "k", &
                   message="expected value "//closing_symbol//" is not the actual value k")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("l")
        call check(error, closing_symbol, "l", &
                   message="expected value "//closing_symbol//" is not the actual value l")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("m")
        call check(error, closing_symbol, "m", &
                   message="expected value "//closing_symbol//" is not the actual value m")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("n")
        call check(error, closing_symbol, "n", &
                   message="expected value "//closing_symbol//" is not the actual value n")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("o")
        call check(error, closing_symbol, "o", &
                   message="expected value "//closing_symbol//" is not the actual value o")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("p")
        call check(error, closing_symbol, "p", &
                   message="expected value "//closing_symbol//" is not the actual value p")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("q")
        call check(error, closing_symbol, "q", &
                   message="expected value "//closing_symbol//" is not the actual value q")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("r")
        call check(error, closing_symbol, "r", &
                   message="expected value "//closing_symbol//" is not the actual value r")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("s")
        call check(error, closing_symbol, "s", &
                   message="expected value "//closing_symbol//" is not the actual value s")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("t")
        call check(error, closing_symbol, "t", &
                   message="expected value "//closing_symbol//" is not the actual value t")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("u")
        call check(error, closing_symbol, "u", &
                   message="expected value "//closing_symbol//" is not the actual value u")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("v")
        call check(error, closing_symbol, "v", &
                   message="expected value "//closing_symbol//" is not the actual value v")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("w")
        call check(error, closing_symbol, "w", &
                   message="expected value "//closing_symbol//" is not the actual value w")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("x")
        call check(error, closing_symbol, "x", &
                   message="expected value "//closing_symbol//" is not the actual value x")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("y")
        call check(error, closing_symbol, "y", &
                   message="expected value "//closing_symbol//" is not the actual value y")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("z")
        call check(error, closing_symbol, "z", &
                   message="expected value "//closing_symbol//" is not the actual value z")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("!")
        call check(error, closing_symbol, "!", &
                   message="expected value "//closing_symbol//" is not the actual value !")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("@")
        call check(error, closing_symbol, "@", &
                   message="expected value "//closing_symbol//" is not the actual value @")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("#")
        call check(error, closing_symbol, "#", &
                   message="expected value "//closing_symbol//" is not the actual value #")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("$")
        call check(error, closing_symbol, "$", &
                   message="expected value "//closing_symbol//" is not the actual value $")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("%")
        call check(error, closing_symbol, "%", &
                   message="expected value "//closing_symbol//" is not the actual value %")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("^")
        call check(error, closing_symbol, "^", &
                   message="expected value "//closing_symbol//" is not the actual value ^")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("&")
        call check(error, closing_symbol, "&", &
                   message="expected value "//closing_symbol//" is not the actual value &")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("*")
        call check(error, closing_symbol, "*", &
                   message="expected value "//closing_symbol//" is not the actual value *")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("-")
        call check(error, closing_symbol, "-", &
                   message="expected value "//closing_symbol//" is not the actual value -")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("_")
        call check(error, closing_symbol, "_", &
                   message="expected value "//closing_symbol//" is not the actual value _")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("=")
        call check(error, closing_symbol, "=", &
                   message="expected value "//closing_symbol//" is not the actual value =")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("+")
        call check(error, closing_symbol, "+", &
                   message="expected value "//closing_symbol//" is not the actual value +")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("|")
        call check(error, closing_symbol, "|", &
                   message="expected value "//closing_symbol//" is not the actual value |")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("`")
        call check(error, closing_symbol, "`", &
                   message="expected value "//closing_symbol//" is not the actual value `")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("~")
        call check(error, closing_symbol, "~", &
                   message="expected value "//closing_symbol//" is not the actual value ~")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol(";")
        call check(error, closing_symbol, ";", &
                   message="expected value "//closing_symbol//" is not the actual value ;")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol(":")
        call check(error, closing_symbol, ":", &
                   message="expected value "//closing_symbol//" is not the actual value :")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("'")
        call check(error, closing_symbol, "'", &
                   message="expected value "//closing_symbol//" is not the actual value '")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol('"')
        call check(error, closing_symbol, '"', &
                   message="expected value "//closing_symbol//' is not the actual value "')
        if (occurred(error)) return

        closing_symbol = get_closing_symbol(",")
        call check(error, closing_symbol, ",", &
                   message="expected value "//closing_symbol//" is not the actual value ,")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol(".")
        call check(error, closing_symbol, ".", &
                   message="expected value "//closing_symbol//" is not the actual value .")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("/")
        call check(error, closing_symbol, "/", &
                   message="expected value "//closing_symbol//" is not the actual value /")
        if (occurred(error)) return

        closing_symbol = get_closing_symbol("?")
        call check(error, closing_symbol, "?", &
                   message="expected value "//closing_symbol//" is not the actual value ?")
        if (occurred(error)) return
    end subroutine test_get_closing_symbol_non_paired_opening_symbol
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
