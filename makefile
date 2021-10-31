.PHONY: all
all:
	csc -o turing-machine src/*.scm

.PHONY: static
static:
	csc -o turing-machine -static src/*.scm && rm src/*.link

.PHONY: remove
remove:
	rm turing-machine
