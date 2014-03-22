all: compres

compres: compres.g
	antlr compres.g -gs
	dlg parser.dlg > scan.c
	g++ -o compres compres.c err.c scan.c

clean:
	rm -f *.c *.dlg *.h
