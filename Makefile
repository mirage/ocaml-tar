include config.mk

.PHONY: build
build: setup.data
	ocaml setup.ml -build

test: build
	ocaml setup.ml -test

config.mk: configure.ml configure
	./configure

.PHONY: clean
clean:
	ocaml setup.ml -clean

.PHONY: distclean
distclean: clean
	rm config.mk setup.data setup.log

setup.data: setup.ml
	ocaml setup.ml -configure $(ENABLE_LWT_UNIX)

install: setup.data
	ocaml setup.ml -install $(INSTALLFLAGS)

uninstall: setup.data
	ocaml setup.ml -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	ocaml setup.ml -reinstall $(REINSTALLFLAGS)

