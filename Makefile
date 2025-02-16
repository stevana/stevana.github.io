SRC_FILES := $(wildcard data/talk/*.md)

all: mkdirs $(patsubst data/talk/%.md, dist/talk/%.html, $(SRC_FILES))
	cabal run exe -- data/content.yaml dist

mkdirs:
	mkdir -p dist/talk/

dist/talk/%.html: data/talk/%.md
	title="$(shell awk -F ': ' '/title:/ { print $$2 }' $<)" \
	date="$(shell awk -F ': ' '/date:/ { print $$2 }' $<)" \
	place="$(shell awk -F ': ' '/place:/ { print $$2 }' $<)" \
	url="$(shell awk -F ': ' '/url:/ { print $$2 }' $<)" \
		./data/header.sh > $@
	pandoc --from gfm --to html5 \
		$< >> $@
	cat data/footer.html >> $@

nodl:
	@cabal run exe -- data/content.yaml dist --skip-download

clean:
	rm -f dist/*.html
	rm -f dist/talk/*.html

.PHONY: all mkdirs clean
