FC=gfortran
FFLAGS=-O3 -Wall -Wextra -fdefault-real-8 -fdefault-integer-8 -fimplicit-none -std=f2003 -fall-intrinsics -cpp -DSTANDALONE
SRC=ST_DEFAULTS.f90 ST_HELPER.f90 ST_SLUMP.f90 ST_PARAMETER_READING.f90 ST_ERROR_CHECKING.f90 ST_INITIALIZATION.f90 ST_TRANSLATE_PROFILE.f90 ST_MAIN.f90
OBJ=$(SRC:.f90=.o)
DEPFLAGS=-M -cpp
SHORETRANS_EXE = ../../../../bin/shoretrans.exe
ifdef OS # only for windows
   RM=@del /f
else # other systems
   RM =@rm -f
endif

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

main: $(OBJ)
	$(FC) $(FFLAGS)  -o $(SHORETRANS_EXE) $(OBJ)

clean:
	$(RM) *.o *.mod ..\bin\*.exe