## opam-build

A sandboxed build environment for opam projects.

### Usage

Running `opam build` at the root of an opam project (i.e. a repo having an
`opam` file) will create:

- an `.opamconfig` file with project settings
- a `sources/` repo with everyting needed to compile the package
- a `build/` directory, which is valid (local) opam root, which
  uses only code packages in `sources/`

### Status

This is still very experimental, feedback and patches are very welcome.