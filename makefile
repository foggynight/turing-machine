define compile-cli
	csc -o turing-machine -include-path src src/cli.scm || rm src/*.c
endef

define remove
	rm turing-machine
endef

all:
	$(call compile-cli)

remove:
	$(call remove)
