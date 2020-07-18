# Makefile

export DRAFTS

EXE=cabal new-run www-build --

build: clean
	$(EXE) build

watch: clean
	$(EXE) watch

clean:
	$(EXE) clean

deep-clean:
	git clean -dfx
