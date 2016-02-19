# make.R - DESC
# make.R

# Copyright 2015 Iago Mosqueira. Distributed under the GPL 2.
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

library(knitr)
library(rmarkdown)

DOC  <- 'Modelling_stock_recruitment_with_FLSR.Rmd'

# html
rmarkdown::render(DOC, output_format='html_document')

# pdf
rmarkdown::render(DOC, output_format='pdf_document')

# R
knitr::purl(DOC)
