MODULES=src/doublyLinkedList
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind
# PKGS=

build:
	$(OCAMLBUILD) $(OBJECTS)
