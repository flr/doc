## ---- ini, echo=FALSE, results='hide', message=FALSE---------------------
# This chunk set the document environment, so it is hidden
library(knitr)
opts_chunk$set(fig.align='center',
  message=FALSE, warning=FALSE, echo=TRUE, cache=FALSE)
options(width=50)
set.seed(1423)

## ---- pkgs, message=FALSE------------------------------------------------
# This chunk loads all necessary packages, trims pkg messages
library(FLCore)

## ----figA----------------------------------------------------------------
# This is an example chunk for a figure
plot(FLQuant(rnorm(200), dim=c(2,20)))

