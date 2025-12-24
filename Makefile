# Makefile for bootstrapping and installing `spkg`.
# Requires CapyScheme to be installed and available as `capy` in PATH.

.PHONY: update-submodules all install-capy test-capy install-gauche test-gauche

SPKG_HOME ?= $(HOME)/.spkg

all: update-submodules install-capy

update-submodules:
	git submodule update --init --recursive


install-capy:
	SPKG_HOME=$(SPKG_HOME) SCHEME=capy capy --fresh-auto-compile -L src,src/args/src -s src/main.scm -- install
	@mkdir -p $(SPKG_HOME)/bin
	@printf '%s\n' '# spkg environment (POSIX sh)' '# Source this from your shell rc file, e.g.:' '#   . "$(SPKG_HOME)/env"' '' 'case ":$$PATH:" in' '  *":$(SPKG_HOME)/bin:"*) ;;' '  *) PATH="$(SPKG_HOME)/bin:$$PATH" ;;' 'esac' 'export PATH' '' '# Default scheme for this installation' 'SCHEME=capy' 'export SCHEME' > $(SPKG_HOME)/env
	@printf '%s\n' '# spkg environment (fish)' '# Source this from fish config, e.g.:' '#   source $(SPKG_HOME)/env.fish' '' 'if type -q fish_add_path' '  fish_add_path -g "$(SPKG_HOME)/bin"' 'else' '  if not contains "$(SPKG_HOME)/bin" $$PATH' '    set -gx PATH "$(SPKG_HOME)/bin" $$PATH' '  end' 'end' '' '# Default scheme for this installation' 'set -gx SCHEME capy' > $(SPKG_HOME)/env.fish
	@printf '%s\n' '' 'Installation finished.' '' 'Next step: add this to your shell startup:' '  POSIX sh (bash/zsh/dash):  . "$(SPKG_HOME)/env"' '  fish:                    source $(SPKG_HOME)/env.fish' '' 'Tip: restart your shell or re-source the file to pick up PATH/SCHEME changes.'

test-capy:
	SCHEME=capy capy --fresh-auto-compile -L src,src/args/src -s src/main.scm -- test

install-gauche:
	SPKG_HOME=$(SPKG_HOME) SCHEME=gauche gosh -r7 -I src/args/src -I src src/main.scm install
	@mkdir -p $(SPKG_HOME)/bin
	@printf '%s\n' '# spkg environment (POSIX sh)' '# Source this from your shell rc file, e.g.:' '#   . "$(SPKG_HOME)/env"' '' 'case ":$$PATH:" in' '  *":$(SPKG_HOME)/bin:"*) ;;' '  *) PATH="$(SPKG_HOME)/bin:$$PATH" ;;' 'esac' 'export PATH' '' '# Default scheme for this installation' 'SCHEME=gauche' 'export SCHEME' > $(SPKG_HOME)/env
	@printf '%s\n' '# spkg environment (fish)' '# Source this from fish config, e.g.:' '#   source $(SPKG_HOME)/env.fish' '' 'if type -q fish_add_path' '  fish_add_path -g "$(SPKG_HOME)/bin"' 'else' '  if not contains "$(SPKG_HOME)/bin" $$PATH' '    set -gx PATH "$(SPKG_HOME)/bin" $$PATH' '  end' 'end' '' '# Default scheme for this installation' 'set -gx SCHEME gauche' > $(SPKG_HOME)/env.fish
	@printf '%s\n' '' 'Installation finished.' '' 'Next step: add this to your shell startup:' '  POSIX sh (bash/zsh/dash):  . "$(SPKG_HOME)/env"' '  fish:                    source $(SPKG_HOME)/env.fish' '' 'Tip: restart your shell or re-source the file to pick up PATH/SCHEME changes.'

test-gauche:
	SCHEME=gauche gosh -r7 -I src/args/src -I src src/main.scm test

