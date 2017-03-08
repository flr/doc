SOURCES := $(wildcard *.Rmd)
FILES = $(SOURCES:%.Rmd=%_files)
CACHE = $(SOURCES:%.Rmd=%_cache)
TARGETS = $(SOURCES:%.Rmd=docs/%.html) $(SOURCES:%.Rmd=docs/R/%.R) # $(SOURCES:%.Rmd=docs/pdf/%.pdf)

.PHONY: all clean

all: main

main: $(TARGETS)

docs/%.html: %.Rmd
	@echo "$< -> $@"
	@R -e "rmarkdown::render_site('$<')"

docs/pdf/%.pdf: %.Rmd
	@echo "$< -> $@"
	@R -e "rmarkdown::render('$<', output_format='tufte::tufte_handout')"

docs/R/%.R: %.Rmd
	@echo "$< -> $@"
	@R -e "knitr::purl('$<')"

default: $(TARGETS)

clean:
	rm -rf $(TARGETS)

cleanall:
	rm -rf $(FILES) -rf $(CACHE)
