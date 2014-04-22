
.PHONY: build
build: setup.data
	ocaml setup.ml -build

setup.data: setup.ml
	ocaml setup.ml -configure

setup.ml: _oasis
	oasis setup

.PHONY: install
install: build
	mkdir -p ${BINDIR}
	install -m 755 newcli.native ${BINDIR}/xe || echo "Failed to install xe"

.PHONY: uninstall
uninstall:
	rm -f ${BINDIR}/xe

.PHONY: clean
clean:
	ocaml setup.ml -clean

.PHONY: distclean
distclean: clean
	rm -f setup.log setup.data setup.ml _tags myocamlbuild.ml
