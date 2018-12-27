
# Invoke `make` to build, `make clean` to clean up, etc.

.PHONY: default all utop test clean

default: all

# Build one library and one standalone executable that implements
# multiple subcommands and uses the library.
# The library can be loaded in utop for interactive testing.
all:
	dune build @install

deps:
	dune external-lib-deps --missing @install
	dune external-lib-deps --missing @runtest

# Launch utop such that it finds our library.
utop: all
	dune utop src/lib

# Build and run tests
test: all
	dune runtest

# Clean up
clean:
# Remove files produced by dune.
	dune clean
