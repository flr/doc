# ini.R - DESC
# /ini.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# knitr
opts_chunk$set(fig.align='center', comment=NA,
  message=FALSE, warning=FALSE, echo=TRUE, cache=FALSE)

set.seed(1423)

# Figures cross-referencing
library(captioner)

fig_nums <- captioner(prefix = "Figure")

fign <- function(name) {
  fig_nums(name=name, display="cite")
}

knit_hooks$set(fig.cap = function(before, options, envir) {
  if(before) {
    invisible(fig_nums(name=options$label, caption=options$fig.cap))
  }
  options$fig.cap <- paste0("Figure", fig_nums(name=options$label), ".")

  return(options)
})

library(printr)
