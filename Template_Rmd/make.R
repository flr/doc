# make.R - DESC
# make.R

# Copyright 2015 Iago Mosqueira. Distributed under the GPL 2.
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

library(knitr)
library(rmarkdown)

DOC <- list.files(pattern="*.Rmd")[1]

# html
rmarkdown::render(DOC, output_format="github_document")

# pdf
rmarkdown::render(DOC, output_format="tufte::tufte_handout")

# R
knitr::purl(DOC)
