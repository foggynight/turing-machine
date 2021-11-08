.PHONY: all
all:
	csc -o turing-machine -O4 -d0 src/*.scm

.PHONY: static
static:
	csc -o turing-machine -O4 -d0 -static src/*.scm && rm src/*.link

.PHONY: debug
debug:
	csc -o turing-machine -d3 src/*.scm

.PHONY: remove
remove:
	rm turing-machine
