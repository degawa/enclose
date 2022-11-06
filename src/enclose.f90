module strings_enclose
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: enclose

    interface enclose
        procedure :: enclose_open_close
        procedure :: enclose_autoclose
    end interface

    character(*), private, parameter :: bracket_open =  "(<[{)>]}" !&
        !! opening symbols
    character(*), private, parameter :: bracket_close = ")>]}(<[{"
        !! closing symbols paired with the opening ones

    logical, private, parameter :: autoclose_default_value = .true.
        !! the default value of the optional argument `autoclose`

contains
    !>Returns string enclosed by `open` and `close` symbols.
    !>
    !>```Fortran
    !>print *, enclose_open_close("string", "[", "]") ! "[string]"
    !>print *, enclose_open_close("string", "[[", "]]") ! "[[string]]"
    !>```
    function enclose_open_close(str, open, close) result(str_enclosed)
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
            !! An string enclosed by opening and closing symbols

        str_enclosed = open//str//close
    end function enclose_open_close

    function enclose_autoclose(str, bracket, autoclose) result(str_enclosed)
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
            !! An string enclosed by opening and closing symbols

        logical :: autoclosing

        autoclosing = autoclose_default_value
        if (present(autoclose)) autoclosing = autoclose

        block
            if (.not. autoclosing) then
                str_enclosed = enclose_open_close(str, bracket, bracket)
                return
            end if
        end block

        block
            character(:), allocatable :: close
                !! closing symbols
            integer(int32) :: pos
                !! position (array index) of a opening bracket
                !! in `bracket_open`

            close = ""

            do pos = len(bracket), 1, -1
                close = close//get_closing_symbol(bracket(pos:pos))
            end do

            str_enclosed = enclose_open_close(str, bracket, close)
            return
        end block
    contains
        function get_closing_symbol(char) result(close)
            implicit none
            character, intent(in) :: char
                !! An opening symbol
            character :: close
                !! A closing symbol

            integer(int32) :: pos
            pos = index(bracket_open, char)
            if (pos >= 1) then ! if `substring` is not in `string`, `index(string, substring)` returns 0
                close = bracket_close(pos:pos)
            else
                close = char
            end if
        end function get_closing_symbol
    end function enclose_autoclose
end module strings_enclose
