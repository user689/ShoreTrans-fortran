!> @brief main program
!! used to call all other functions
!! @param arg - command line arguments (directory name)
program shoretrans
#ifdef STANDALONE
    use st_defaults
    use st_parameter_reading
    use st_initialization
    use st_helper
#endif
    use st_translate_profile


    implicit none
    real :: start_time, end_time
    character(len=charlen) :: msg

    start_time = get_time()

    call get_dirname() ! get directory name
    ! create logfile

    call logger(-2, "Program start...")
    call read_parameters(dir_name)
    call setup ! initial configurations (initialize)

    ! calculate the new profiles
    call main_loop()



    ! calculate run time
    end_time = get_time() - start_time
    write(msg, "(A, F10.3, A)") "Program finished in ", end_time, &
                                 " seconds."
    call logger(-1, adj(msg))
    print *, adj(msg)
end program shoretrans