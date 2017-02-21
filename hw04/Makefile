filename=hw04

all: $(filename).pdf

clean:
	rm -rf *.aux *.out *.gz *.log *.tex .Rhistory *~ figure cache

$(filename).pdf: $(filename).tex
	pdflatex $(filename).tex
	Rscript --vanilla -e "require(extrafont);embed_fonts('$(filename).pdf')"

$(filename).tex: $(filename).rnw SoreThroat.dat flagg_hw04_prob4.r
	Rscript --vanilla -e "require(knitr);knit('$(filename).rnw')"

SoreThroat.dat:
	wget http://stat.ufl.edu/~aa/glm/data/SoreThroat.dat

flagg_hw04_prob4.r: GillenRFunctions.R


GillenRFunctions.R:
	wget http://www.math.montana.edu/shancock/courses/stat539/r/GillenRFunctions.R
