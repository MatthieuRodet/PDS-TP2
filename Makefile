SRC=$(wildcard *.ml *.mll)

.PHONY: clean

all: main.native

main.native: $(SRC)
	ocamlbuild -verbose 0 -lib str -pp camlp4o $@

clean:
	ocamlbuild -clean
	find tests -type f -not -name "*.vsl" -not -name "*.test_in" -delete


tests_unit:
	@for f in ./tests/unit/*.vsl ; do echo "Processing $$f file..\e[1;38;5;11m" ; ./compile $$f ; echo "\e[0m$$f done\n\n" ; done

tests_unit_prettyprint:
	@for f in ./tests/unit/*.vsl ; do echo "Processing $$f file..\e[1;38;5;11m" ; ./main.native < $$f ; echo "\e[0m$$f done\n\n" ; done
	

tests_threads:
	@for f in ./tests/threads/*.vsl ; do echo "Processing $$f file..\e[1;38;5;11m" ; ./compile $$f ; echo "\e[0m$$f done\n\n" ; done

tests_threads_prettyprint:
	@for f in ./tests/threads/*.vsl ; do echo "Processing $$f file..\e[1;38;5;11m" ; ./main.native < $$f ; echo "\e[0m$$f done\n\n" ; done


tests_level1:
	@for f in ./tests/testlevel1/*.vsl ; do echo "Processing $$f file..\e[1;38;5;11m" ; ./compile $$f ; echo "\e[0m$$f done\n\n" ; done

tests_level1_prettyprint:
	@for f in ./tests/testlevel1/*.vsl ; do echo "Processing $$f file..\e[1;38;5;11m" ; ./main.native < $$f ; echo "\e[0m$$f done\n\n" ; done


tests_level2:
	@for f in ./tests/testlevel2/*.vsl ; do echo "Processing $$f file..\e[1;38;5;11m" ; ./compile $$f ; echo "\e[0m$$f done\n\n" ; done

tests_level2_prettyprint :
	@for f in ./tests/testlevel2/*.vsl ; do echo "Processing $$f file..\e[1;38;5;11m" ; ./main.native < $$f ; echo "\e[0m$$f done\n\n" ; done


tests_level3:
	@for f in ./tests/testlevel3/*.vsl ; do echo "Processing $$f file..\e[1;38;5;11m" ; ./compile $$f ; echo "\e[0m$$f done\n\n" ; done

tests_level3_prettyprint :
	@for f in ./tests/testlevel3/*.vsl ; do echo "Processing $$f file..\e[1;38;5;11m" ; ./main.native <$$f ; echo "\e[0m$$f done\n\n" ; done


tests_level4:
	@for f in ./tests/testlevel4/*.vsl ; do echo "Processing $$f file..\e[1;38;5;11m" ; ./compile $$f ; echo "\e[0m$$f done\n\n" ; done

tests_level4_prettyprint :
	@for f in ./tests/testlevel4/*.vsl ; do echo "Processing $$f file..\e[1;38;5;11m" ; ./main.native < $$f ; echo "\e[0m$$f done\n\n" ; done


tests_error:
	@for f in ./tests/testlevelerror/*.vsl ; do echo "Processing $$f file..\e[1;38;5;11m" ; ./compile $$f ; echo "\e[0m$$f done\n\n" ; done


tests : tests_unit tests_threads tests_level1 tests_level2 tests_level3 tests_level4 tests_error

test_prettyprint : tests_unit_prettyprint tests_threads_prettyprint tests_level1_prettyprint tests_level2_prettyprint tests_level3_prettyprint tests_level4_prettyprint

test : tests


experiment : SHELL := /bin/bash # Execute the recipe in bash instead of shell
experiment : # May take some time to execute. (around 500 seconds)
	if [[ ! -d "stats" ]]; then mkdir stats; fi
	for _ in {1..100}; do start_time=$$(date +%s.%3N) ; for _ in {1..1000}; do ./tests/threads/test_unit_map_hard ; done ; end_time=$$(date +%s.%3N) ; elapsed=$$(echo "scale=3; $$end_time - $$start_time" | bc) ; echo "$$elapsed"; done > stats/stats_map.txt
	for _ in {1..100}; do start_time=$$(date +%s.%3N) ; for _ in {1..1000}; do ./tests/threads/test_unit_not_map_hard ; done ; end_time=$$(date +%s.%3N) ; elapsed=$$(echo "scale=3; $$end_time - $$start_time" | bc) ; echo "$$elapsed"; done > stats/stats_not_map.txt
	for _ in {1..100}; do start_time=$$(date +%s.%3N) ; for _ in {1..1000}; do ./tests/threads/test_unit_thread ; done ; end_time=$$(date +%s.%3N) ; elapsed=$$(echo "scale=3; $$end_time - $$start_time" | bc) ; echo "$$elapsed"; done > stats/stats_thread.txt
	for _ in {1..100}; do start_time=$$(date +%s.%3N) ; for _ in {1..1000}; do ./tests/threads/test_unit_not_thread ; done ; end_time=$$(date +%s.%3N) ; elapsed=$$(echo "scale=3; $$end_time - $$start_time" | bc) ; echo "$$elapsed"; done > stats/stats_not_thread.txt
