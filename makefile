define compile
	csc -o turing-machine src/*.scm || rm src/*.c
endef

define execute
	./turing-machine res/example-program.txt
endef

define remove
	rm turing-machine
endef

all:
	$(call compile)

execute:
	$(call execute)

remove:
	$(call remove)

test:
	$(call compile)
	bash -c "trap 'make remove' INT ; make execute"
	$(call remove)
