SHELL := /bin/bash
TEX    = $(shell find . -name '*.tex')
AUX    = $(TEX:.tex=.aux)

all : thesis.pdf

# rule to make thesis.pdf
thesis.pdf : $(TEX)
	latexmk -pdf thesis.tex

clean :
	rm -f thesis.pdf *.aux *.log *.lof *.lot *.toc *.blg *.fls *.fdb_latexmk *.bbl
