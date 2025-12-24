# spkg 

Scheme Package Manager & Build system inspired by Cargo. 

# Features

- Automatic download of dependencies based on manifest description of them
- Support for R7RS/R6RS Scheme systems 
- Support for dev dependencies in the manifest (`dev-dependencies`).
- Works with "raw" dependencies: just specify path to source directory. `spkg` will calculate checksum and lock the deps appropriately.

# Supported Schemes

| Scheme | Can run spkg? | Can run projects? |
| --- | --- | --- |
| CapyScheme | ✅ | ✅ |
| Gauche | ✅ | ✅ |
| Chibi | ❌ | ✅ |
| Guile | ❌ | ✅ |



## TODOs
- Build dependencies
- build scripts and `spkg:rerun-on-change` directive
- Tests and benchmarks out of the box (using [`brunch`](https://github.com/playx18/brunch-scm) and SRFI-64)
- Snowball package format support
- Akku package format support
- Better checksums, refactor the code around lockfiles


## OCI packages (GHCR / any registry)

`spkg` can treat an OCI artifact as a dependency type. This is implemented via the external `oras` CLI.

Notes:
- `url` is the registry/repository (e.g. `ghcr.io/my-org/my-lib`)
- `rev` is the tag (e.g. `1.2.3`)

### Install from an OCI registry

Add an `oci` dependency to `spkg.scm`:

```scheme
(dependencies
  (oci
	(name (my org lib))
	(url "ghcr.io/my-org/my-lib")
	(rev "1.2.3")))
```

Then run:

- `spkg update` to lock the dependency in `spkg.lock`
- `spkg fetch` (or any command that installs deps) to pull and use the lockfile

### Publish to GHCR (or any OCI registry)

1. Authenticate (example for GHCR):

	- `oras login ghcr.io`

2. Publish the current package:

	- `spkg publish --ref ghcr.io/my-org/my-lib`




## Dependencies

- `python3`: Used to easily generate checksums for dependencies, later on will be replaced by Scheme native solution
- `git`: Used to fetch Git dependencies
- `oras`: Required only for OCI dependencies (`(oci ...)`) and `spkg publish`
- [`capyscheme`](https://github.com/playx18/capyscheme) or Gauche: Right now the only Scheme implementations that can run spkg is CapyScheme and Gauche. Work on supporting Guile and Chibi is ongoing. 

## Installation/bootstrap

To bootstrap spkg you just have to run `make all`. It will update submodules and then install `spkg` using
`spkg` itself. During bootstrap `spkg` depends on [args](https://github.com/playx18/scm-args) library being in
`src/` directory, after bootstrap it automatically switches to using `args` managed by spkg itself. 

### Install script (curl-able)

If you already have either **CapyScheme** (`capy`) or **Gauche** (`gosh`) installed, you can bootstrap with the
included installer script. It auto-detects available Scheme runners and will ask which one to use.

```sh
curl -fsSL https://raw.githubusercontent.com/scheme-packages/spkg/refs/heads/master/install.sh | sh
```


### Shell environment (PATH)

The installer generates:

- `~/.spkg/env` for POSIX shells (bash/zsh/sh)
- `~/.spkg/env.fish` for fish

To make `spkg` available in new shells:

- bash/zsh: add to `~/.profile` or `~/.bashrc`:

	```sh
	. "$HOME/.spkg/env"
	```

- fish: add to `~/.config/fish/config.fish`:

	```fish
	source ~/.spkg/env.fish
	```

# Demo

Here's a small demo of spkg installing itself:

![Demo](assets/demo.gif)