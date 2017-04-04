filename=hw05

all: $(filename).pdf

clean:
	rm -rf *.aux *.out *.gz *.log *.tex .Rhistory *~ figure cache GillenRFunctions.R

$(filename).pdf: $(filename).tex
	pdflatex $(filename).tex
	Rscript --vanilla -e "require(extrafont);embed_fonts('$(filename).pdf')"

$(filename).tex: $(filename).rnw flagg_hw05_probs-2-3-5.r Alligator.csv Alligators3.dat
	Rscript --vanilla -e "require(knitr);knit('$(filename).rnw')"

flagg_hw05_probs-2-3-5.r: Alligator.csv Alligators3.dat GillenRFunctions.R


GillenRFunctions.R:
	wget http://www.math.montana.edu/shancock/courses/stat539/r/GillenRFunctions.R

Alligator.csv:
	wget http://www.math.montana.edu/shancock/courses/stat539/data/Alligator.csv

Alligators3.dat:
	wget http://www.stat.ufl.edu/~aa/glm/data/Alligators3.dat

