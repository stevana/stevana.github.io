SRC_FILES := $(wildcard data/talk/*.md)

all: mkdirs $(patsubst data/talk/%.md, dist/talk/%.html, $(SRC_FILES))
	cabal run exe -- data/content.yaml dist

mkdirs:
	pandoc --version
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
