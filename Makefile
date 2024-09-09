.DEFAULT_GOAL := run

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
	ARCH = -arch -x86_64
endif

SCHEME_ENTRY := scheme_entry
ifeq ($(UNAME_S), Darwin)
	SCHEME_ENTRY = _scheme_entry
endif

# Assemble and link the program
compile:
	sed -i -e 's/\([^_]scheme_entry\)\|\(^scheme_entry\)/ $(SCHEME_ENTRY)/g' output.s
	$(ARCH)	gcc runtime.c output.s -o output

run: compile
	@./output

clean:
	rm -f output

.PHONY: all run clean
