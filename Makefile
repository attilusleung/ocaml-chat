MODULES=src/client/doublyLinkedList src/client/panel src/client/key src/client/client src/common/parser src/common/protocol src/client/network
MLS_WITHOUT_MLIS=src/client/terminal src/client/log src/server/server src/server/chatLog
MLS=$(MODULES:=.ml) $(MLS_WITHOUT_MLIS:=.ml) src/authors.ml
MLIS=$(MODULES:=.mli) src/authors.mli
CLIENTLOC=src/client/
CLIENT=terminal.native
SERVELOC=src/server/
SERVE=server.native
OBJECTS=$(MODULES:=.cmo) $(MLS_WITHOUT_MLIS:=.cmo)
# TESTS=src/tests/testParser
# TESTOUTPUT=$(TESTS:=.native)
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,oUnit,lwt,ANSITerminal,lwt.unix,lwt_ppx
CMIS=-I _build/src/client/ -I _build/src/common/ -I _build/src/server/

build:
	$(OCAMLBUILD) $(OBJECTS)

term:
	$(OCAMLBUILD) $(CLIENTLOC)$(CLIENT) && ./$(CLIENT)

remote:
	$(OCAMLBUILD) $(CLIENTLOC)$(CLIENT) && ./$(CLIENT) -a remote

serve:
	$(OCAMLBUILD) $(SERVELOC)$(SERVE) && ./$(SERVE)

test:
	$(OCAMLBUILD) -tag debug src/tests/test.native && ./test.native

docs: docs-private docs-public

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc $(CMIS) -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc $(CMIS) -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	$(OCAMLBUILD) -clean
	rm -rf doc.public doc.private src.zip
	$(OCAMLBUILD) $(OBJECTS)

zip:
	zip src.zip ./chatlogs/.placeholder $(MLS) $(MLIS) _tags INSTALL.md Makefile passwd.txt README.md