module strings_enclose
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private
    public :: enclose
    public :: get_closing_symbol
    public :: get_closing_brackets

    interface enclose
        procedure :: enclose_open_close
        procedure :: enclose_autoclose
    end interface

    character(*), parameter :: bracket_open  = "(<[{)>]}" !&
        !! opening symbols
    character(*), parameter :: bracket_close = ")>]}(<[{"
        !! closing symbols paired with the opening ones

    logical, parameter :: autoclose_default_value = .true.
        !! the default value of the optional argument `autoclose`

contains
    !>Returns string enclosed by `open` and `close` symbols.
    !>
    !>```Fortran
    !>print *, enclose_open_close("string", "[", "]") ! "[string]"
    !>print *, enclose_open_close("string", "[[", "]]") ! "[[string]]"
    !>```
    pure function enclose_open_close(str, open, close) result(str_enclosed)
        implicit none
        !&<
        character(*), intent(in) :: str
            !! A string to be enclosed
        character(*), intent(in) :: open
            !! Opening bracket symbols
        character(*), intent(in) :: close
            !! Closing bracket symbols
        !&>
        character(:), allocatable :: str_enclosed
            !! A string enclosed by opening and closing symbols

        str_enclosed = open//str//close
    end function enclose_open_close

    !>Returns string enclosed by `bracket` symbols.
    !>
    !>`bracket` symbol specifies **opening** bracket symbols.
    !>Closing symbols are automatically determined.
    !>
    !>When optional argument `autoclose` is `.false.`,
    !>closing symbols are the same as the opening symbols.
    !>
    !>The paired opening and closing symbols are as follows:
    !>
    !>| opening symbol | closing symbol |
    !>| :------------: | :------------: |
    !>|      `(`       |      `)`       |
    !>|      `<`       |      `>`       |
    !>|      `[`       |      `]`       |
    !>|      `{`       |      `}`       |
    !>|      `)`       |      `(`       |
    !>|      `>`       |      `<`       |
    !>|      `]`       |      `[`       |
    !>|      `}`       |      `{`       |
    !>
    !>For other symbols, including alphanumeric characters,
    !>the same symbols are chosen as the closing symbols.
    !>
    !>```Fortran
    !>print *, enclose_autoclose("string", "'")                       ! "'string'"
    !>print *, enclose_autoclose("string", "([<{")                    ! "([<{string}>])"
    !>print *, enclose_autoclose("string", "([<{", autoclose=.true.)  ! "([<{string}>])"
    !>print *, enclose_autoclose("string", "([<{", autoclose=.false.) ! "([<{string([<{"
    !>print *, enclose_autoclose("string", "abc")                     ! "abcstringcba"
    !>print *, enclose_autoclose("string", "abc", autoclose=.false.)  ! "abcstringabc"
    !>```
    pure function enclose_autoclose(str, bracket, autoclose) result(str_enclosed)
        implicit none
        !&<
        character(*), intent(in)            :: str
            !! A string to be enclosed
        character(*), intent(in)            :: bracket
            !! Opening bracket symbols
        logical     , intent(in), optional  :: autoclose
            !! Automatically determining closing symbols
            !! as the pair of opening symbols
        !&>
        character(:), allocatable :: str_enclosed
            !! A string enclosed by opening and closing symbols

        logical :: autoclosing

        autoclosing = autoclose_default_value
        if (present(autoclose)) autoclosing = autoclose

        if (autoclosing) then
            ! enclose using `bracket` as the opening symbols and
            ! auto-determined closing symbols
            str_enclosed = enclose_open_close(str, bracket, get_closing_brackets(bracket))
        else
            ! enclose using `bracket` as the opening and closing symbols
            str_enclosed = enclose_open_close(str, bracket, bracket)
        end if
    end function enclose_autoclose

    !>Returns the closing brackets paired with the opening brackets.
    !>The closing brackets are in reverse order of the opening brackets.
    !>
    !>The paired opening and closing symbols are the same as those
    !>described in [[get_closing_symbol]].
    !>
    !>For other symbols, including alphanumeric characters,
    !>the same symbols are chosen as the closing symbols.
    pure function get_closing_brackets(opening_brackets) result(closing_brackets)
        implicit none
        character(*), intent(in) :: opening_brackets
            !! Opening brackets
        character(:), allocatable :: closing_brackets
            !! Closing brackets

        integer(int32) :: pos
            !! position (array index) of an opening bracket
            !! in `opening_brackets`
        integer(int32) :: len_brackets
            !! length of `opening_brackets`

        len_brackets = len(opening_brackets)

        ! Each opening symbol maps to exactly one closing symbol,
        ! so the result length is known in advance.
        allocate (character(len_brackets) :: closing_brackets)

        ! The closing brackets are in reverse order of the opening brackets.
        ! ex) When `opening_brackets` is "({<", the result must be ">})", not ")}>".
        do pos = 1, len_brackets
            closing_brackets(len_brackets - pos + 1:len_brackets - pos + 1) = &
                get_closing_symbol(opening_brackets(pos:pos))
        end do
    end function get_closing_brackets

    !>Returns the closing symbol paired with an opening symbol.
    !>
    !>The paired opening and closing symbols are as follows:
    !>
    !>| opening symbol | closing symbol |
    !>| :------------: | :------------: |
    !>|      `(`       |      `)`       |
    !>|      `<`       |      `>`       |
    !>|      `[`       |      `]`       |
    !>|      `{`       |      `}`       |
    !>|      `)`       |      `(`       |
    !>|      `>`       |      `<`       |
    !>|      `]`       |      `[`       |
    !>|      `}`       |      `{`       |
    !>
    !>For other symbols, including alphanumeric characters,
    !>the same symbols are chosen as the closing symbols.
    pure function get_closing_symbol(opening_symbol) result(closing_symbol)
        implicit none
        character, intent(in) :: opening_symbol
            !! An opening symbol
        character :: closing_symbol
            !! A closing symbol

        integer(int32) :: pos

        ! If an opening symbol is in the parameter `bracket_open` at `pos`,
        ! the closing symbol is in the parameter `bracket_close` at `pos`.
        ! If not, the opening symbol is used as the closing symbol.
        pos = index(bracket_open, opening_symbol)
        if (pos >= 1) then ! if `substring` is not in `string`, `index(string, substring)` returns 0
            closing_symbol = bracket_close(pos:pos)
        else
            closing_symbol = opening_symbol
        end if
    end function get_closing_symbol
end module strings_enclose
