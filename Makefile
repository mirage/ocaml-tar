all:
	ocamlbuild -I src tar.cma tar.cmxa

clean:
	ocamlbuild -clean