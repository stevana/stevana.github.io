all:
	@mkdir -p /tmp/notes
	@cabal run exe -- data/content.yaml /tmp/notes
