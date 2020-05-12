file=NLPreadability

all: Sweave latex

latex: ${file}.tex
	pdflatex ${file}.tex
	bibtex ${file}
	bibtex ${file}
	pdflatex ${file}.tex
	pdflatex ${file}.tex

Sweave:
	R CMD Sweave ${file}.Rnw | echo

clean:
	rm -f *.log *.out *.log *.blg *.bbl *.toc *.aux *.tex
