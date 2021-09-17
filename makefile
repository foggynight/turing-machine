all:
	csc -o turing-machine src/*

remove:
	rm turing-machine

test:
	csc -o turing-machine src/*
	./turing-machine res/example-program.txt
	rm turing-machine
