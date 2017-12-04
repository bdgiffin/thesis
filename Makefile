SHELL := /bin/bash
TEX    = $(shell find . -maxdepth 1 -name '*.tex')
BIB    = $(shell find . -maxdepth 1 -name '*.bib')
AUX    = $(TEX:.tex=.aux)

all : thesis.pdf

# rule to make thesis.pdf
thesis.pdf : $(TEX) $(BIB)
	latexmk -pdf thesis.tex

clean :
	rm -f thesis.pdf *.aux *.log *.lof *.lot *.toc *.blg *.fls *.fdb_latexmk *.bbl
