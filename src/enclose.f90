module strings_enclose
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: enclose

    interface enclose
        procedure :: enclose_open_close
    end interface

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

end module strings_enclose
