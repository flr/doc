SOURCES := $(wildcard *.Rmd)
FILES = $(SOURCES:%.Rmd=docs/%_files) $(SOURCES:%.Rmd=docs/pdf/%_files)
CACHE = $(SOURCES:%.Rmd=%_cache) $(SOURCES:%.Rmd=%_files)
HTMLS = $(SOURCES:%.Rmd=docs/%.html) 
RS = $(SOURCES:%.Rmd=docs/R/%.R)
PDFS = $(SOURCES:%.Rmd=docs/pdf/%.pdf)
PDFSTYLE = "rmarkdown::pdf_document"

.PHONY: all clean

all: main clean

main: $(HTMLS) $(RS) $(PDFS)

docs/%.html: %.Rmd _site.yml
	@echo "$< -> $@"
	@R --vanilla -e "rmarkdown::render_site('$<', envir=new.env())" -e "if('FLash' %in% loadedNamespaces()) detach(package:FLash)"

docs/pdf/%.pdf: %.Rmd
	@echo "$< -> $@"
	@R --vanilla -e "knitr::opts_chunk[['set']](dev = 'pdf')" -e "rmarkdown::render('$<', output_format='$(PDFSTYLE)', output_file='$@', clean=TRUE)"

docs/R/%.R: %.Rmd
	@echo "$< -> $@"
	@R --vanilla -e "knitr::purl('$<', output='$@')"

setup:
	R --vanilla -e "install.packages(c('captioner', 'printr', 'rmarkdown', 'knitr'))"

clean:
	rm -f *.html
	rm -rf $(CACHE)
	rm -f docs/R/ini.R
	rm -f docs/README docs/index.md docs/Makefile

cleanall: clean
	rm -rf $(FILES) $(HTMLS) $(RS) $(PDFS)
