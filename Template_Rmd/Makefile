
SOURCES := $(wildcard *.Rmd)
TARGETS = $(SOURCES:%.Rmd=%.html) $(SOURCES:%.Rmd=%.R) $(SOURCES:%.Rmd=%.pdf)

.PHONY: all clean

all: main

main: $(TARGETS)

%.md: %.Rmd
	@echo "$< -> $@"
	@R -e "knitr::knit('$<')"

%.html: %.Rmd
	@echo "$< -> $@"
	@R -e "rmarkdown::render('$<')"
# render('template.Rmd', 'knitrBootstrap::bootstrap_document')

%.pdf: %.Rmd
	@echo "$< -> $@"
	@R -e "rmarkdown::render('$<', output_format='tufte::tufte_handout')"

%.R: %.Rmd
	@echo "$< -> $@"
	@R -e "knitr::purl('$<')"

default: $(TARGETS)

clean:
	rm -rf $(TARGETS)
