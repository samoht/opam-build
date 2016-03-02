all:
	ocamlbuild -pkgs fmt.tty,unix,astring build.native -no-links
	@if [ ! -e opam-build ]; then ln -s _build/build.native opam-build; fi

clean:
	ocamlbuild -clean
	rm -rf _build/ opam-build
	rm -rf build/ sources/ .opamconfig
