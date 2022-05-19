build: hpack
	cabal build

hpack:
	hpack .

.PHONY: build hpack
