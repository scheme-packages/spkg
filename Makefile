# Makefile for bootstrapping and installing `spkg`.
# Requires CapyScheme to be installed and available as `capy` in PATH.

.PHONY: update-submodules all install-capy test-capy install-gauche test-gauche

all: update-submodules install-capy

update-submodules:
	git submodule update --init --recursive


install-capy:
	SCHEME=capy capy --fresh-auto-compile -L src,src/args/src -s src/main.scm -- install
	@mkdir -p $(HOME)/.spkg/bin
	@printf '%s\n' '# spkg environment (POSIX sh)' '# Source this from your shell rc file, e.g.:' '#   . "$$HOME/.spkg/env"' '' 'case ":$$PATH:" in' '  *":$$HOME/.spkg/bin:"*) ;;' '  *) PATH="$$HOME/.spkg/bin:$$PATH" ;;' 'esac' 'export PATH' > $(HOME)/.spkg/env
	@printf '%s\n' '# spkg environment (fish)' '# Source this from fish config, e.g.:' '#   source ~/.spkg/env.fish' '' 'if type -q fish_add_path' '  fish_add_path -g "$$HOME/.spkg/bin"' 'else' '  if not contains "$$HOME/.spkg/bin" $$PATH' '    set -gx PATH "$$HOME/.spkg/bin" $$PATH' '  end' 'end' > $(HOME)/.spkg/env.fish

test-capy:
	SCHEME=capy capy --fresh-auto-compile -L src,src/args/src -s src/main.scm -- test

install-gauche:
	SCHEME=gauche gosh -r7 -I src/args/src -I src src/main.scm install
	@mkdir -p $(HOME)/.spkg/bin
	@printf '%s\n' '# spkg environment (POSIX sh)' '# Source this from your shell rc file, e.g.:' '#   . "$$HOME/.spkg/env"' '' 'case ":$$PATH:" in' '  *":$$HOME/.spkg/bin:"*) ;;' '  *) PATH="$$HOME/.spkg/bin:$$PATH" ;;' 'esac' 'export PATH' > $(HOME)/.spkg/env
	@printf '%s\n' '# spkg environment (fish)' '# Source this from fish config, e.g.:' '#   source ~/.spkg/env.fish' '' 'if type -q fish_add_path' '  fish_add_path -g "$$HOME/.spkg/bin"' 'else' '  if not contains "$$HOME/.spkg/bin" $$PATH' '    set -gx PATH "$$HOME/.spkg/bin" $$PATH' '  end' 'end' > $(HOME)/.spkg/env.fish

test-gauche:
	SCHEME=gauche gosh -r7 -I src/args/src -I src src/main.scm test

