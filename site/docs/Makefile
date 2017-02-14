
SOURCES := $(wildcard *.Rmd)
TARGETS = $(SOURCES:%.Rmd=%.html) $(SOURCES:%.Rmd=R/%.R) $(SOURCES:%.Rmd=pdf/%.pdf)

.PHONY: all clean

all: main

main: $(TARGETS)

%.html: %.Rmd
	@R -e "rmarkdown::render_site()"

pdf/%.pdf: %.Rmd
	@echo "$< -> $@"
	@R -e "rmarkdown::render('$<', output_format='pdf_document', output_file='pdf/$<.pdf')"

R/%.R: %.Rmd
	@echo "$< -> $@"
	@R -e "knitr::purl('$<', output='R/$<.R')"

default: $(TARGETS)

clean:
	rm -rf $(TARGETS)
