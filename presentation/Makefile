DOCBASE=1

all: $(DOCBASE).pdf

%.pdf: %.dot
	dot $< -Tps2 -o $@.ps
	ps2pdf $@.ps $@
	rm $@.ps

%.pdf: %.dia
	dia -t eps -e $@.eps $<
	epstopdf --outfile=$@ $@.eps
	rm $@.eps

$(DOCBASE).pdf: $(DOCBASE).tex
	pdflatex $(DOCBASE).tex
	pdflatex $(DOCBASE).tex
	#pdflatex $(DOCBASE).tex
	#pdflatex $(DOCBASE).tex

watch:
	./build.sh

clean:
	@rm -f $(DOCBASE).aux $(DOCBASE).log $(DOCBASE).nav $(DOCBASE).out $(DOCBASE).snm $(DOCBASE).toc *~

kill:
	rm -f *.pdf *.ps
