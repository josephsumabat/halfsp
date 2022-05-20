GHCID_GHC_OPTIONS = --ghc-options "-osuf dyn_o -hisuf dyn_hi -fwrite-ide-info -hiedir .hiefiles"

build: hpack
	cabal build

hpack:
	hpack .

ghcid: hpack
	ghcid \
		--command='cabal repl lib $(GHCID_GHC_OPTIONS)' \
		--test='Lib.indexInGhcid' \
		--test-message='Indexing .hiefiles...'

hie-index:
	hiedb -D .hiedb index .hiefiles

ghcid-test: hpack
	ghcid \
		--command='cabal repl halfsp-test'

test: hpack
	cabal test

format:
	find app/ lib/ test/ -name "*.hs" -exec ormolu -m inplace {} +

clean:
	cabal clean

.PHONY: build hpack ghcid ghcid-test hie-index test format clean
