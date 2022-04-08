FC=gfortran
FFLAGS=-O3 -Wall -Wextra -fdefault-integer-8 -fimplicit-none -std=f2003 -fall-intrinsics -cpp -DSTANDALONE
SRC=ST_DEFAULTS.F90 ST_HELPER.F90 ST_SLUMP.F90 ST_PARAMETER_READING.F90 ST_ERROR_CHECKING.F90 ST_INITIALIZATION.F90 ST_TRANSLATE_PROFILE.F90 ST_MAIN.F90
OBJ=$(SRC:.F90=.o)
DEPFLAGS=-M -cpp
SHORETRANS_EXE = ../../../../bin/shoretrans.exe
ifdef OS # only for windows
   RM=@del /f
else # other systems
   RM =@rm -f
endif

%.o: %.F90
	$(FC) $(FFLAGS) -c $< -o $@

exe: $(OBJ)
	$(FC) $(FFLAGS)  -o $(SHORETRANS_EXE) $(OBJ)

clean:
	$(RM) *.o *.mod