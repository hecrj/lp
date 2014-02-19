all: example0 example1

example0: example0.g
	antlr example0.g -gs
	dlg parser.dlg > scan.c
	gcc -o example0 example0.c err.c scan.c -I../pccts/h

example1: example1.g
	antlr -gt example1.g
	dlg -ci parser.dlg > scan.c
	g++ -o example1 example1.c err.c scan.c -I../pccts/h
