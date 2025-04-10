# Compiler and flags
FC = gfortran
FFLAGS = -Wall -Wextra -fdefault-integer-8 -fimplicit-none -fall-intrinsics -O2 -std=f2008 \
         -fbounds-check -ffpe-trap='invalid','zero','overflow','underflow','denormal' \
         -cpp -DSTANDALONE -Jbuild

# Directories
SRC_DIR = src
OBJ_DIR = build
BIN_DIR = bin

# Executable (no .exe suffix unless you really need it)
SHORETRANS_EXE = $(BIN_DIR)/shoretrans

# Explicitly ordered source files (Fortran modules must be compiled in correct order)
SRC = \
	$(SRC_DIR)/ST_DEFAULTS.F90 \
	$(SRC_DIR)/ST_HELPER.F90 \
	$(SRC_DIR)/ST_SLUMP.F90 \
	$(SRC_DIR)/ST_WALL_VOLUME.F90 \
	$(SRC_DIR)/ST_PARAMETER_READING.F90 \
	$(SRC_DIR)/ST_ERROR_CHECKING.F90 \
	$(SRC_DIR)/ST_INITIALIZATION.F90 \
	$(SRC_DIR)/ST_TRANSLATE_PROFILE.F90 \
	$(SRC_DIR)/ST_MAIN.F90

# Map .F90 ? build/*.o
OBJ = $(patsubst $(SRC_DIR)/%.F90, $(OBJ_DIR)/%.o, $(SRC))

# Default target
all: exe

# Link executable
$(SHORETRANS_EXE): $(OBJ) | $(BIN_DIR)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

# Compile each .F90 to build/*.o
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.F90 | $(OBJ_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Create build and bin directories if missing
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

# Public target
exe: $(SHORETRANS_EXE)

# Cleanup
clean:
	rm -f $(OBJ_DIR)/*.o $(OBJ_DIR)/*.mod $(SHORETRANS_EXE)

# Declare targets as phony to avoid accidental collisions
.PHONY: all exe clean $(OBJ_DIR) $(BIN_DIR)