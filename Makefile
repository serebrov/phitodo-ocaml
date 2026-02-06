.PHONY: all build test clean install-deps run

all: build

# Install dependencies using opam
install-deps:
	opam install . --deps-only --with-test -y

# Build the project
build:
	dune build

# Run tests
test:
	dune runtest

# Run the application
run:
	dune exec phitodo

# Clean build artifacts
clean:
	dune clean

# Format code
fmt:
	dune fmt

# Create opam switch for this project (if needed)
switch:
	opam switch create . 5.1.0 --no-install
	opam install dune -y
