.PHONY: all
all:
	csc -o turing-machine -O4 -d0 src/*.scm

.PHONY: debug
debug:
	csc -o turing-machine -d3 src/*.scm

.PHONY: eggs
eggs:
	chicken-install -s srfi-1 vector-lib

.PHONY: remove
remove:
	rm turing-machine
