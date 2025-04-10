# Compiler & flags
FC = gfortran
FFLAGS = -Wall -Wextra -fdefault-integer-8 -fimplicit-none -fall-intrinsics -O2 -std=f2008 \
		 -fbounds-check -ffpe-trap='invalid','zero','overflow','underflow','denormal' \
		 -cpp -DSTANDALONE

# Directories
SRC_DIR = src
OBJ_DIR = build
BIN_DIR = bin

# Explicitly ordered source files (important for module dependencies!)
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

# Object files
OBJ = $(patsubst $(SRC_DIR)/%.F90, $(OBJ_DIR)/%.o, $(SRC))

# Final executable path
SHORETRANS_EXE = $(BIN_DIR)/shoretrans.exe

# Detect OS
ifdef OS
	RM = @del /f
	MKDIR = @mkdir
else
	RM = @rm -f
	MKDIR = @mkdir -p
endif

# Prevent deletion of intermediate files
.SECONDARY:

# Compile rule: src/%.F90 ? build/%.o
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.F90 | $(OBJ_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Link final executable
$(SHORETRANS_EXE): $(OBJ) | $(BIN_DIR)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

# Default target
exe: $(SHORETRANS_EXE)

# Create build/ and bin/ directories if they don't exist
$(OBJ_DIR):
	$(MKDIR) $(OBJ_DIR)

$(BIN_DIR):
	$(MKDIR) $(BIN_DIR)

# Clean rule
clean:
	$(RM) $(OBJ_DIR)/*.o $(OBJ_DIR)/*.mod $(SHORETRANS_EXE)
