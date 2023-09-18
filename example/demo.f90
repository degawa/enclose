program demo
    use, intrinsic :: iso_fortran_env
    use :: strings_enclose
    implicit none

    real(real64), allocatable :: u(:, :)
    character(:), allocatable :: variable_name

    allocate (u(0:10, -1:9))

    block
        variable_name = "`u`"
        print *, variable_name//" range [["// &
            to_string(lbound(u, 1))//", "// &
            to_string(ubound(u, 1))//"], ["// &
            to_string(lbound(u, 2))//", "// &
            to_string(ubound(u, 2))//"]]"
    end block

    block
        variable_name = enclose("u", "`")
        print *, variable_name//" range "// &
            enclose( &
            enclose(to_string(lbound(u, 1))//", "// &
                    to_string(ubound(u, 1)), "[") &
            //", "// &
            enclose(to_string(lbound(u, 2))//", "// &
                    to_string(ubound(u, 2)), "["), &
            "[")
    end block

contains
    function to_string(i) result(str)
        implicit none
        integer(int32), intent(in) :: i
        character(:), allocatable :: str

        character(14) :: buf
        write (buf, '(I0)') i
        str = trim(buf)
    end function to_string
end program demo
