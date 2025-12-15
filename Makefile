.PHONY: run update-submodules all

all: update-submodules install

install:
	SCHEME=capy capy --fresh-auto-compile -L src,src/args/src -s src/main.scm -- install

update-submodules:
	git submodule update --init --recursive