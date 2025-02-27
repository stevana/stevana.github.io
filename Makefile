SRC_FILES := $(wildcard data/talk/*.md)

all: mkdirs $(patsubst data/talk/%.md, dist/talk/%.html, $(SRC_FILES))
	cabal run exe -- data/content.yaml dist
	sed -i \
		-e 's#00-introduction.md#introduction_to_simulation_testing.html#g' \
		-e 's#02-maelstrom-testing-echo-example.md#using_maelstrom_to_test_distributed_systems.html#g' \
		-e 's#03-simulation-testing-echo-example.md#sketching_how_to_simulation_test_distributed_systems.html#g' \
		-e 's#04-simulation-testing-main-loop.md#the_main_test_loop_of_simulation_testing.html#g' \
          	dist/introduction_to_simulation_testing.html \
		dist/using_maelstrom_to_test_distributed_systems.html \
		dist/sketching_how_to_simulation_test_distributed_systems.html \
		dist/the_main_test_loop_of_simulation_testing.html

mkdirs:
	mkdir -p dist/talk/

dist/talk/%.html: data/talk/%.md
	pandoc --from gfm --to html5 --template data/talk.template \
		--lua-filter template-functions.lua \
		$< >> $@

nodl:
	@cabal run exe -- data/content.yaml dist --skip-download

clean:
	rm -f dist/*.html
	rm -f dist/talk/*.html

.PHONY: all mkdirs clean
