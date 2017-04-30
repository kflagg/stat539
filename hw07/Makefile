filename=hw07

all: $(filename).pdf

clean:
	rm -rf *.aux *.out *.gz *.log *.tex .Rhistory *~ figure cache

$(filename).pdf: $(filename).tex
	pdflatex $(filename).tex
	Rscript --vanilla -e "require(extrafont);embed_fonts('$(filename).pdf')"

$(filename).tex: $(filename).rnw flagg_$(filename).r
	Rscript --vanilla -e "require(knitr);knit('$(filename).rnw')"

$(filename).tex:

flagg_$(filename).r:

