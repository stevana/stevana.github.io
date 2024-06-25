all:
	@mkdir -p /tmp/notes
	@cabal run exe -- data/content.yaml /tmp/notes

nodl:
	@cabal run exe -- data/content.yaml /tmp/notes --skip-download
