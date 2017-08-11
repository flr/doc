SOURCES := $(wildcard *.Rmd)
FILES = $(SOURCES:%.Rmd=docs/%_files) $(SOURCES:%.Rmd=docs/pdf/%_files)
CACHE = $(SOURCES:%.Rmd=%_cache) $(SOURCES:%.Rmd=%_files)
TARGETS = $(SOURCES:%.Rmd=docs/%.html) $(SOURCES:%.Rmd=docs/R/%.R) $(SOURCES:%.Rmd=docs/pdf/%.pdf)

.PHONY: all clean

all: main

main: $(TARGETS)

docs/%.html: %.Rmd
	@echo "$< -> $@"
	@R -e "rmarkdown::render_site('$<', envir=new.env())"

docs/pdf/%.pdf: %.Rmd
	@echo "$< -> $@"
	@R -e "rmarkdown::render('$<', output_format='tufte::tufte_handout', output_file='$@', clean=TRUE)"

docs/R/%.R: %.Rmd
	@echo "$< -> $@"
	@R -e "knitr::purl('$<', output='$@')"

default: $(TARGETS)

clean:
	rm *.html
	rm -rf $(CACHE)

cleanall: clean
	rm -rf $(FILES) $(TARGETS)
