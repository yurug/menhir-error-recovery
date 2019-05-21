.PHONY: all clean

all:
	dune build parse.exe
	ln -sf _build/default/parse.exe parse

clean:
	dune clean
