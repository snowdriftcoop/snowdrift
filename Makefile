.PHONY: all initial deps database

all: initial

initial: deps database

deps:
	stack build --only-dependencies --install-ghc Snowdrift:test
	stack build yesod-bin foreign-store cabal-install

database:
	./sdb.hs start

Snowdrift:
	stack build

test:
	./build.sh test --fast --pedantic
