all:
	csc -o turing-machine -include-path src src/cli.scm || rm src/*.c

remove:
	rm turing-machine
