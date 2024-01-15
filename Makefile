.PHONY: all build format edit demo clean

src?=0
dst?=5
graph?=graph10.txt
game?=game1.txt

all: build

build:
	@echo "\n   🚨  COMPILING  🚨 \n"
	dune build src/ftest.exe
	ls src/*.exe > /dev/null && ln -fs src/*.exe .

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "\n   ⚡  EXECUTING  ⚡\n"
	./ftest.exe graphs/${graph} $(src) $(dst) outfile
	@echo "\n   🥁  RESULT (content of outfile)  🥁\n"
	@cat outfile
	dot -Tsvg outfile > outfile.svg

medium: 
	@echo "\n   🚨  COMPILING  🚨 \n"
	dune build src/teamfiletest.exe
	ls src/*.exe > /dev/null && ln -fs src/*.exe .

	@echo "\n   ⚡  EXECUTING  ⚡\n"
	./teamfiletest.exe games/${game} outfile
	@echo "\n   🥁  RESULT (content of outfile)  🥁\n"
	@cat outfile

clean:
	find -L . -name "*~" -delete
	rm -f *.exe
	dune clean
