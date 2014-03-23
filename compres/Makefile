all: compres

compres: compres.g
	antlr compres.g -gt
	dlg parser.dlg > scan.c
	g++ -o compres compres.c err.c scan.c -w -DVERBOSE

clean:
	rm -f *.c *.dlg *.h
