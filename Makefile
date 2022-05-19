GHCID_GHC_OPTIONS = --ghc-options "-osuf dyn_o -hisuf dyn_hi -fwrite-ide-info -hiedir .hiefiles"

build: hpack
	cabal build

hpack:
	hpack .

ghcid:
	ghcid -c 'cabal repl lib $(GHCID_GHC_OPTIONS)'

hie-index:
	hiedb -D .hiedb index .hiefiles

.PHONY: build hpack ghcid hie-index
