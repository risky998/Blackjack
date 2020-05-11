MODULES=authors command deck player main state ai
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

check:
	bash checkenv.sh && bash checktypes.sh

finalcheck: check
	bash checkzip.sh
	bash finalcheck.sh
	
test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	clear && $(OCAMLBUILD) $(MAIN) && ./$(MAIN)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private blackjack.zip

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,qcheck  \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,qcheck \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

zip:
	zip blackjack.zip *.ml* *.json _tags Makefile .merlin README.md install.txt