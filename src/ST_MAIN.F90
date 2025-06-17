!> @brief main program
!! used to call all other functions
!! @param arg - command line arguments (directory name)
program shoretrans
   use st_defaults
   use st_parameter_reading
   use st_initialization
   use st_helper
   use st_translate_profile


   implicit none
   real(kind=8):: start_time, end_time
   character(len=charlen) :: msg

   start_time = get_time()

   call get_dirname() ! get directory name
   ! create logfile

   call logger(-2, "Program start...")
   call read_parameters(dir_name)
   call setup_shoretrans ! initial configurations (initialize)

   ! calculate the new profiles
   call translate_profile()

   ! save the profile in the output file
   call logger(2, 'saving final profile to: '//adj(dir_name)// &
      '/z_final.out')
   ! todo: write a check for output variables
   call write_to('z_final.out', x, z_final)
   call write_to('initial_profile.out', x, z)
   ! calculate run time
   end_time = get_time() - start_time
   write(msg, "(A, F10.3, A)") "Program finished in ", end_time, &
      " seconds."
   call logger(-1, adj(msg))
end program shoretrans
