## ---- ini, echo=FALSE, results='hide', message=FALSE---------------------
library(knitr)
opts_chunk$set(fig.align='center',
  message=FALSE, warning=FALSE, echo=TRUE, cache=FALSE)
options(width=50)
set.seed(1423)

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("FLCore"), repos="http://flr-project.org/R")

## ---- pkgs---------------------------------------------------------------
library(FLCore)

## ---- helpFLQuant, results='hide', eval=FALSE----------------------------
## help(FLQuant)

## ---- FLQuant------------------------------------------------------------
FLQuant(rlnorm(20), dim=c(4,5), quant="age", units="kg")

## ---- dimnames-----------------------------------------------------------
FLQuant(rlnorm(20), units="kg",
  dimnames=list(age=0:3, year=2010:2014))

## ---- subset-------------------------------------------------------------
flq <- FLQuant(rlnorm(20), units="kg",
  dimnames=list(age=0:3, year=2010:2014))

flq[1,]

dim(flq[1,])

## ---- arith--------------------------------------------------------------
flq * 2

flq + (flq * 2)

## ----plotFLQuant---------------------------------------------------------
plot(FLQuant(rnorm(200), dim=c(2,20)))

