
.PHONY: build
build: setup.data
	ocaml setup.ml -build

setup.data: setup.ml
	ocaml setup.ml -configure

setup.ml: _oasis
	oasis setup

.PHONY: clean
clean:
	ocaml setup.ml -clean

.PHONY: distclean
distclean: clean
	rm -f setup.log setup.data setup.ml _tags myocamlbuild.ml
