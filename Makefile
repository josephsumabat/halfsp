build: hpack
	cabal build

hpack:
	hpack .

ghcid:
	ghcid -c "cabal repl"

.PHONY: build hpack ghcid
