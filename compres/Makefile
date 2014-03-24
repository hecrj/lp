all: compres

compres: compres.c
	g++ -o compres compres.c err.c scan.c -w

compres_ni: compres.c
	g++ -o compres_ni compres.c err.c scan.c -w -DNO_INTERACTIVE

compres_d: compres.c
	g++ -o compres_d compres.c err.c scan.c -w -DDEBUG

compres_d_ni: compres.c
	g++ -o compres_d_ni compres.c err.c scan.c -w -DNO_INTERACTIVE -DDEBUG

compres.c: compres.g
	antlr compres.g -gt
	dlg parser.dlg > scan.c

clean:
	rm -f *.c *.dlg *.h compres compres_ni compres_d compres_d_ni
