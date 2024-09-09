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
endif

# Assemble and link the program
compile:
	$(ARCH)	gcc runtime.c output.s -o output

run: compile
	@./output

clean:
	rm -f output

.PHONY: all run clean
