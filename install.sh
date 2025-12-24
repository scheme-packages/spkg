#!/usr/bin/env sh

set -eu

REPO_URL_DEFAULT="https://github.com/scheme-packages/spkg"
SPKG_HOME_DEFAULT="$HOME/.spkg"

say() { printf '%s\n' "$*"; }
warn() { printf '%s\n' "warning: $*" >&2; }
say_err() { printf '%s\n' "$*" >&2; }
die() { printf '%s\n' "error: $*" >&2; exit 1; }

need_cmd() {
  command -v "$1" >/dev/null 2>&1
}

usage() {
  cat <<'USAGE'
Usage: install.sh [options]

Options:
  --repo URL              Git repo URL to clone (default: https://github.com/playX18/spkg)
  --dir DIR               Destination dir (default: a temporary directory)
  --scheme capy|gauche    Skip prompts and force scheme
  --spkg-home DIR         Set SPKG_HOME (default: ~/.spkg)
  --non-interactive       Fail instead of prompting
  --dry-run               Print what would run, but don't run it
  -h, --help              Show help

Notes:
  - Requires: git
  - Requires either:
      * CapyScheme runner: capy
    or
      * Gauche runner: gosh
USAGE
}

REPO_URL="$REPO_URL_DEFAULT"
DEST_DIR=""
FORCE_SCHEME=""
NON_INTERACTIVE=0
DRY_RUN=0

# If 1, keep the temp checkout directory (useful for debugging).
SPKG_KEEP_TMP="${SPKG_KEEP_TMP:-0}"

is_temp_dest=0
cleanup_dest_dir() {
  # Only remove temp dirs we created, and only when not in dry-run mode.
  if [ "$DRY_RUN" -eq 1 ]; then
    return 0
  fi
  if [ "$SPKG_KEEP_TMP" = "1" ]; then
    return 0
  fi
  if [ "${is_temp_dest:-0}" -eq 1 ] && [ -n "${DEST_DIR:-}" ] && [ -d "${DEST_DIR:-}" ]; then
    rm -rf "$DEST_DIR" || true
  fi
}

# Allow user to pre-set SPKG_HOME via environment.
SPKG_HOME="${SPKG_HOME:-$SPKG_HOME_DEFAULT}"

while [ "$#" -gt 0 ]; do
  case "$1" in
    --repo)
      [ "$#" -ge 2 ] || die "--repo requires a value"
      REPO_URL="$2"; shift 2 ;;
    --dir)
      [ "$#" -ge 2 ] || die "--dir requires a value"
      DEST_DIR="$2"; shift 2 ;;
    --scheme)
      [ "$#" -ge 2 ] || die "--scheme requires a value"
      FORCE_SCHEME="$2"; shift 2 ;;
    --spkg-home)
      [ "$#" -ge 2 ] || die "--spkg-home requires a value"
      SPKG_HOME="$2"; shift 2 ;;
    --non-interactive)
      NON_INTERACTIVE=1; shift ;;
    --dry-run)
      DRY_RUN=1; shift ;;
    -h|--help)
      usage; exit 0 ;;
    *)
      die "unknown argument: $1" ;;
  esac
done

setup_dest_dir() {
  if [ -n "${DEST_DIR:-}" ]; then
    is_temp_dest=0
    return 0
  fi

  if [ "$DRY_RUN" -eq 1 ]; then
    DEST_DIR="/tmp/spkg.XXXXXX (dry-run)"
    is_temp_dest=1
    return 0
  fi

  DEST_DIR=$(mktemp -d "${TMPDIR:-/tmp}/spkg.XXXXXX")
  is_temp_dest=1

  trap cleanup_dest_dir EXIT INT TERM
}

choose_spkg_home() {
  # If user already set it (env or flag), keep it.
  if [ -n "${SPKG_HOME:-}" ]; then
    # Prompt only if it's still the default and we are interactive.
    if [ "$NON_INTERACTIVE" -eq 0 ] && [ "$SPKG_HOME" = "$SPKG_HOME_DEFAULT" ]; then
      ans=$(prompt "SPKG_HOME directory (default: $SPKG_HOME_DEFAULT): ") || true
      if [ -n "${ans:-}" ]; then
        SPKG_HOME="$ans"
      fi
    fi
    return 0
  fi

  SPKG_HOME="$SPKG_HOME_DEFAULT"
}

write_env_files() {
  # $1: scheme (capy|gauche)
  scheme="$1"
  spkg_home="$SPKG_HOME"
  env_sh="$spkg_home/env"
  env_fish="$spkg_home/env.fish"

  say "Writing environment files to $spkg_home"
  run "mkdir -p '$spkg_home' '$spkg_home/bin'"

  # POSIX sh/zsh/bash
  if [ "$DRY_RUN" -eq 1 ]; then
    say "+ write $env_sh"
  else
    cat >"$env_sh" <<'EOF'
# spkg environment (POSIX sh)
# Source this from your shell rc file, e.g.:
#   . "$HOME/.spkg/env"

case ":$PATH:" in
  *":$HOME/.spkg/bin:"*) ;;
  *) PATH="$HOME/.spkg/bin:$PATH" ;;
esac
export PATH
EOF

    # Add install-specific defaults (don't rely on heredoc interpolation).
    {
      printf '\n%s\n' '# Default scheme for this installation'
      printf '%s\n' "SCHEME=$scheme" 'export SCHEME'
    } >>"$env_sh"

    # Replace hard-coded $HOME/.spkg with SPKG_HOME, but only within this file.
    # This keeps the nice user-facing comments while honoring --spkg-home.
    tmp="$env_sh.tmp"
    sed "s|\$HOME/.spkg|$spkg_home|g" "$env_sh" >"$tmp" && mv "$tmp" "$env_sh"
  fi

  # fish shell
  if [ "$DRY_RUN" -eq 1 ]; then
    say "+ write $env_fish"
  else
    cat >"$env_fish" <<'EOF'
# spkg environment (fish)
# Source this from fish config, e.g.:
#   source ~/.spkg/env.fish

if type -q fish_add_path
  fish_add_path -g "$HOME/.spkg/bin"
else
  if not contains "$HOME/.spkg/bin" $PATH
    set -gx PATH "$HOME/.spkg/bin" $PATH
  end
end
EOF

    {
      printf '\n%s\n' '# Default scheme for this installation'
      printf '%s\n' "set -gx SCHEME $scheme"
    } >>"$env_fish"

    tmp="$env_fish.tmp"
    sed "s|~/.spkg|$spkg_home|g; s|\$HOME/.spkg|$spkg_home|g" "$env_fish" >"$tmp" && mv "$tmp" "$env_fish"
  fi
}

post_install_message() {
  cat >&2 <<EOF

spkg env files created:
  - $SPKG_HOME/env        (bash/zsh/posix sh)
  - $SPKG_HOME/env.fish   (fish)

To enable ``spkg`` in new shells:
  - bash/zsh:  add this line to ~/.profile or ~/.bashrc:
    . "$SPKG_HOME/env"
  - fish:      add this line to ~/.config/fish/config.fish:
    source "$SPKG_HOME/env.fish"
EOF
}

run() {
  if [ "$DRY_RUN" -eq 1 ]; then
    say "+ $*"
    return 0
  fi
  # shellcheck disable=SC2086
  sh -c "$*"
}

prompt() {
  # $1: prompt text
  if [ "$NON_INTERACTIVE" -eq 1 ]; then
    return 1
  fi
  if [ ! -r /dev/tty ]; then
    return 1
  fi
  printf '%s' "$1" >&2
  IFS= read -r ans </dev/tty
  printf '%s' "$ans"
  return 0
}

choose_scheme() {
  # honor forced scheme
  if [ -n "$FORCE_SCHEME" ]; then
    case "$FORCE_SCHEME" in
      capy|gauche) say "$FORCE_SCHEME"; return 0 ;;
      *) die "invalid --scheme: $FORCE_SCHEME (expected capy or gauche)" ;;
    esac
  fi

  have_capy=0
  have_gauche=0
  if need_cmd capy; then have_capy=1; fi
  if need_cmd gosh; then have_gauche=1; fi

  if [ "$have_capy" -eq 0 ] && [ "$have_gauche" -eq 0 ]; then
    die "neither 'capy' nor 'gosh' was found in PATH"
  fi

  if [ "$have_capy" -eq 1 ] && [ "$have_gauche" -eq 0 ]; then
    say "capy"; return 0
  fi
  if [ "$have_capy" -eq 0 ] && [ "$have_gauche" -eq 1 ]; then
    say "gauche"; return 0
  fi

  say_err "Both CapyScheme (capy) and Gauche (gosh) were found."
  ans=$(prompt "Choose scheme [capy/gauche] (default: capy): ") || die "non-interactive and multiple schemes available; pass --scheme"
  if [ -z "$ans" ]; then ans="capy"; fi
  case "$ans" in
    capy|gauche) say "$ans"; return 0 ;;
    *) die "invalid choice: $ans" ;;
  esac
}

ensure_checkout() {
  if [ -d "$DEST_DIR/.git" ]; then
    say "Using existing checkout: $DEST_DIR"
  else
    need_cmd git || die "git is required"
    say "Cloning $REPO_URL into $DEST_DIR"
    run "git clone --depth 1 '$REPO_URL' '$DEST_DIR'"
  fi

  # init submodules (bootstrap depends on args)
  say "Updating git submodules"
  run "cd '$DEST_DIR' && git submodule update --init --recursive"
}

install_spkg() {
  scheme="$1"
  mkdir -p "$SPKG_HOME/bin"
  mkdir -p "$SPKG_HOME/lib"
  case "$scheme" in
    capy)
      need_cmd capy || die "capy not found"
      say "Bootstrapping via CapyScheme"
      run "cd '$DEST_DIR' && SPKG_HOME='$SPKG_HOME' SCHEME=capy capy --fresh-auto-compile -L src,src/args/src -s src/main.scm -- install"
      ;;
    gauche)
      need_cmd gosh || die "gosh not found"
      say "Bootstrapping via Gauche"
      run "cd '$DEST_DIR' && SPKG_HOME='$SPKG_HOME' SCHEME=gauche gosh -r7 -I src/args/src -I src src/main.scm install"
      ;;
    *)
      die "unknown scheme: $scheme" ;;
  esac

  write_env_files "$scheme"
  say "Done." 
  post_install_message
}

main() {
  choose_spkg_home
  setup_dest_dir
  scheme=$(choose_scheme)
  ensure_checkout
  install_spkg "$scheme"
}

main "$@"
