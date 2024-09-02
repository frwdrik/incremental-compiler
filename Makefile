# Define variables for the compiler and tools
CC = foo
AS = as
LD = ld
CFLAGS =
ASFLAGS =
LDFLAGS = -o

# Detect the system architecture
UNAME_S := $(shell uname -s)

# Set the appropriate architecture flag
ifeq ($(UNAME_S), Darwin)
	ARCH = -arch x86_64
else
	ARCH = -m64
endif

# Define the source and output files
SRC = program.asm
OBJ = program.o

# Assemble and link the program
out: $(OBJ)
	as $(ARCH) -o $@ $<
	ld -o $@ $@

# Target to run the output
run: $(OUTPUT)
	./$(OUTPUT)

# Clean the build files
clean:
	rm -f $(OBJ) $(OUTPUT)

.PHONY: all run clean
