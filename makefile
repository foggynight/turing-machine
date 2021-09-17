define compile
	csc -o turing-machine src/*.scm || rm src/*.c
endef

define remove
	rm turing-machine
endef

all:
	$(call compile)

remove:
	$(call remove)

test:
	$(call compile)
	./turing-machine res/example-program.txt
	$(call remove)
