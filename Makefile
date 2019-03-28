
# Invoke `make` to build, `make clean` to clean up, etc.

.PHONY: default build utop test clean

default: build

build:
	dune build @install

deps:
	dune external-lib-deps --missing @install
	dune external-lib-deps --missing @runtest

# Launch utop such that it finds our library.
utop:
	dune utop src

# Build and run tests
test: build
	dune runtest

# Clean up
clean:
# Remove files produced by dune.
	dune clean
