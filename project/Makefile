MODULES= scrabble main player board dictionary
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS= scrabble.mli player.mli board.mli dictionary.mli
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && \
	./$(TEST) -runner sequential

bisect: clean test
	bisect-ppx-report -I _build -html report bisect0001.out

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)
	
zip:
	zip scrabble.zip *.ml* *.json _tags *.txt Makefile dictionary/*.txt
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private scrabble.zip
