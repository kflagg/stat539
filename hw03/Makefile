filename=hw03

all: $(filename).pdf

clean:
	rm -rf *.aux *.out *.gz *.log *.tex .Rhistory *~ figure cache

$(filename).pdf: $(filename).tex
	pdflatex $(filename).tex
	Rscript --vanilla -e "require(extrafont);embed_fonts('$(filename).pdf')"

$(filename).tex: $(filename).rnw flagg_iwls.r
	Rscript --vanilla -e "require(knitr);knit('$(filename).rnw')"

flagg_iwls.r: Framingham.txt


Framingham.txt:
	wget http://www.math.montana.edu/shancock/courses/stat539/data/Framingham.txt

