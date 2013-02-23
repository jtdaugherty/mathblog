
mathblog-manual.pdf: *.tex ch?/*.tex
	# Run it twice so the TOC gets generated properly
	pdflatex -halt-on-error -shell-escape mathblog-manual.tex
	pdflatex -halt-on-error -shell-escape mathblog-manual.tex

clean:
	rm -f *~ *.dvi *.pdf *.log *.aux *.toc *.out
	rm -f ch?/*.aux ch?/*~
