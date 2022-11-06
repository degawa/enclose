program demo
    use, intrinsic :: iso_fortran_env
    use :: strings_enclose
    use :: stdlib_logger
    use :: stdlib_strings
    implicit none

    real(real64), allocatable :: u(:, :)
    character(:), allocatable :: variable_name

    allocate (u(0:10, -1:9))

    block
        variable_name = "`u`"
        call global_logger%log_information(variable_name//" allocation status "//to_string(allocated(u)))
        call global_logger%log_information(variable_name//" range [["// &
                                           to_string(lbound(u, 1))//", "// &
                                           to_string(ubound(u, 1))//"], ["// &
                                           to_string(lbound(u, 2))//", "// &
                                           to_string(ubound(u, 2))//"]]")
    end block

    block
        variable_name = enclose("u", "`")
        call global_logger%log_information(variable_name//" allocation status "//to_string(allocated(u)))
        call global_logger%log_information(variable_name//" range "// &
                                           enclose( &
                                           enclose(to_string(lbound(u, 1))//", "// &
                                                   to_string(ubound(u, 1)), "[") &
                                           //", "// &
                                           enclose(to_string(lbound(u, 2))//", "// &
                                                   to_string(ubound(u, 2)), "["), &
                                           "["))
    end block
end program demo
