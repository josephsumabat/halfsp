GHCID_GHC_OPTIONS = --ghc-options "-osuf dyn_o -hisuf dyn_hi -fwrite-ide-info -hiedir .hiefiles -Wno-deprecations"

build: hpack
	cabal build

hpack:
	hpack .

ghcid:
	ghcid \
		--command='cabal repl lib $(GHCID_GHC_OPTIONS)' \
		--test='Lib.indexInGhcid' \
		--test-message='Indexing .hiefiles...'

hie-index:
	hiedb -D .hiedb index .hiefiles

.PHONY: build hpack ghcid hie-index
