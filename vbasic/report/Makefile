all: report.pdf

.PHONY: report.pdf

report.pdf: report.hal
	hal report.hal > report.tex
	pdflatex report.tex
	pdflatex report.tex
	rm report.tex
