MODULES=src/doublyLinkedList src/terminal src/panel src/key
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind
# PKGS=

build:
	$(OCAMLBUILD) $(OBJECTS)

term:
	$(OCAMLBUILD) src/terminal.native
