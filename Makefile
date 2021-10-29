SRC=$(wildcard *.ml *.mll)

.PHONY: clean

all: main.native

main.native: $(SRC)
	ocamlbuild -verbose 0 -lib str -pp camlp4o $@

clean:
	ocamlbuild -clean
	find tests -type f -not -name "*.vsl" -not -name "*.test_in" -delete

tests_comilation:
	for f in ./*/*/*.vsl ; do echo "\nProcessing $$f file..\n" ; ./compile $$f ; echo "\n$$f done\n" ; done