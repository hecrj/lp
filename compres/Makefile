all: compres

compres: compres.c
	g++ -o compres compres.c err.c scan.c

compres_ni: compres.c
	g++ -o compres_ni compres.c err.c scan.c -DNO_INTERACTIVE

compres.c:
	antlr compres.g -gt
	dlg parser.dlg > scan.c

clean:
	rm -f *.c *.dlg *.h
