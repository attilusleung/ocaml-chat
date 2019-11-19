MODULES=src/client/doublyLinkedList src/client/terminal src/client/panel src/client/key src/client/network src/server/server src/client/log src/client/client src/common/parser src/common/protocol src/server/chatlog
CLIENTLOC=src/client/
CLIENT=terminal.native
SERVELOC=src/server/
SERVE=server.native
OBJECTS=$(MODULES:=.cmo)
# TESTS=src/tests/testParser
# TESTOUTPUT=$(TESTS:=.native)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind
# PKGS=

build:
	$(OCAMLBUILD) $(OBJECTS)

term:
	$(OCAMLBUILD) $(CLIENTLOC)$(CLIENT) && ./$(CLIENT)

serve:
	$(OCAMLBUILD) $(SERVELOC)$(SERVE) && ./$(SERVE)

test:
	$(OCAMLBUILD) -tag debug src/tests/testParser.native && ./testParser.native
