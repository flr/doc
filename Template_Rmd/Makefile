
SOURCES := $(wildcard *.Rmd)
TARGETS = $(patsubst %.Rmd,%.html,$(SOURCES)) $(patsubst %.Rmd,%.R,$(SOURCES))

.PHONY: all clean

all: main

main: $(TARGETS)

%.md: %.Rmd
	@echo "$< -> $@"
	@R -e "knitr::knit('$<')"

%.html: %.Rmd
	@echo "$< -> $@"
	@R -e "rmarkdown::render('$<')"

%.pdf: %.Rmd
	@echo "$< -> $@"
	@R -e "rmarkdown::render('$<', output_format='tufte::tufte_handout')"

%.R: %.Rmd
	@echo "$< -> $@"
	@R -e "knitr::purl('$<')"

default: $(TARGETS)

clean:
	rm -rf $(TARGETS)
