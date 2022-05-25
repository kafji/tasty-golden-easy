all: build

gen_cabal:
	hpack

build: gen_cabal
	cabal build

test: gen_cabal
	cabal test
