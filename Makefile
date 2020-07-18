# Makefile

export DRAFTS

EXE=cabal new-run www-build --

build: clean
	$(EXE) build

watch: clean
	$(EXE) watch

publish: clean
	DRAFTS=no $(MAKE) $(MAKEFLAGS) build && bash scripts/publish.bash

clean:
	$(EXE) clean

deep-clean:
	git clean -dfx

.PHONY: build watch publish clean deep-clean
