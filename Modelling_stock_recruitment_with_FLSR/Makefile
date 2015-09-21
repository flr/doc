
SOURCES := $(wildcard *.Rmd)
TARGETS=$(SOURCES:%.Rmd=%.html)

%.html: %.Rmd
	@echo "$< -> $@"
	@R -e "rmarkdown::render('$<')"

%.pdf: %.Rmd
	@echo "$< -> $@"
	@R -e "rmarkdown::render('$<')"

default: $(TARGETS)

clean:
	rm -rf $(TARGETS)
