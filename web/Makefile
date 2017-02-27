SOURCES := $(wildcard *.Rmd)
TARGETS = $(SOURCES:%.Rmd=docs/%.html) $(SOURCES:%.Rmd=docs/R/%.R) $(SOURCES:%.Rmd=docs/pdf/%.pdf)

.PHONY: all clean

all: main

main: docs

docs: $(SOURCES)
	@echo "$< -> $@"
	@R -e "rmarkdown::render_site()"

docs/%.html: %.Rmd
	@echo "$< -> $@"
	@R -e "rmarkdown::render_site('$<')"

docs/pdf/%.pdf: %.Rmd
	@echo "$< -> $@"
	@R -e "rmarkdown::render('$<', output_format='tufte::tufte_handout')"

dosc/R/%.R: %.Rmd
	@echo "$< -> $@"
	@R -e "knitr::purl('$<')"

clean:
	rm -rf $(TARGETS)
	rm -f *.html
