SRC=$(wildcard *.ml *.mll)

.PHONY: clean

all: main.native

main.native: $(SRC)
	ocamlbuild -verbose 0 -lib str -pp camlp4o $@

clean:
	ocamlbuild -clean
	find tests -type f -not -name "*.vsl" -not -name "*.test_in" -delete

tests_unit:
	for f in ./tests/perso/*.vsl ; do echo "\nProcessing $$f file.." ; ./compile $$f ; echo "$$f done\n" ; done

tests_level1:
	for f in ./tests/testlevel1/*.vsl ; do echo "\nProcessing $$f file.." ; ./compile $$f ; echo "$$f done\n" ; done

tests_level2:
	for f in ./tests/testlevel2/*.vsl ; do echo "\nProcessing $$f file.." ; ./compile $$f ; echo "$$f done\n" ; done

tests_level3:
	for f in ./tests/testlevel3/*.vsl ; do echo "\nProcessing $$f file.." ; ./compile $$f ; echo "$$f done\n" ; done

tests_level4:
	for f in ./tests/testlevel4/*.vsl ; do echo "\nProcessing $$f file.." ; ./compile $$f ; echo "$$f done\n" ; done

tests_error:
	for f in ./tests/testlevelerror/*.vsl ; do echo "\nProcessing $$f file.." ; ./compile $$f ; echo "$$f done\n" ; done

test : tests_unit tests_level1 tests_level2 tests_level3 tests_level4 tests_error