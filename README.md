# spkg 

Scheme Package Manager & Build system inspired by Cargo. 

# Features

- Automatic download of dependencies based on manifest description of them
- Support for R7RS/R6RS Scheme systems 
- Support for dev/build dependencies and dev/build scripts (TODO).
- Works with "raw" dependencies: just specify path to source directory. `spkg` will calculate checksum and lock the deps appropriately.

## TODOs
- Dev/Build dependencies
- build scripts and `spkg:rerun-on-change` directive
- Tests and benchmarks out of the box (using [`brunch`](https://github.com/playx18/brunch-scm) and SRFI-64)
- Snowball package format support
- Akku package format support
- Better checksums, refactor the code around lockfiles


## Dependencies

- `python3`: Used to easily generate checksums for dependencies, later on will be replaced by Scheme native solution
- `git`: Used to fetch Git dependencies
- [`capyscheme`](https://github.com/playx18/capyscheme): Right now the only Scheme implementation that can run spkg is CapyScheme. Work on supporting Gauche and Guile is ongoing. 

## Installation/bootstrap

To bootstrap spkg you just have to run `make all`. It will update submodules and then install `spkg` using
`spkg` itself. During bootstrap `spkg` depends on [args](https://github.com/playx18/scm-args) library being in
`src/` directory, after bootstrap it automatically switches to using `args` managed by spkg itself. 