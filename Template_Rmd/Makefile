DOC=template

$(DOC).html: $(DOC).Rmd
	R -e "library(rmarkdown); render('$(DOC).Rmd')"
