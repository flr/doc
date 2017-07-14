## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
set.seed(8765)

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("FLCore"), repos="http://flr-project.org/R")

## ---- pkgs, results="hide", message=FALSE, warnings=FALSE----------------
library(FLCore)

## ---- flquant------------------------------------------------------------
fq <- FLQuant(1.287, units="kg")
print(fq)

## ----flpar---------------------------------------------------------------
FLPar(Linf=50, K=0.5, t0=-0.2, units=c("cm", "1/y", "y"))

## ---- accessors----------------------------------------------------------
units(fq)

units(fq) <- "t"

summary(fq)

## ---- currency-----------------------------------------------------------
FLQuant(1.2e6, units="EUR")

## ---- arith--------------------------------------------------------------
FLQuant(9230, units="kg") + FLQuant(367, units="kg")

## ---- uom----------------------------------------------------------------
uom("+", "kg", "kg")

## ---- 101000-------------------------------------------------------------
FLQuant(c(467.34, 345.33), units="1000") * FLQuant(c(2.4, 3.5), units="100")

## ---- kg1000t------------------------------------------------------------
FLQuant(c(467.34, 345.33), units="1000") * FLQuant(c(2.4, 3.5), units="kg")

## ---- fmz----------------------------------------------------------------
FLQuant(runif(8, 0.1, 0.8), units="f") %+% FLQuant(0.2, units="m")

## ---- prodiv-------------------------------------------------------------
FLQuant(126, units="boat") * FLQuant(2350, units="EUR / boat")

## ---- prodivc------------------------------------------------------------
FLQuant(126, units="boat / d") * FLQuant(2350, units="EUR / boat / d")
FLQuant(2350, units="EUR / boat / d") * FLQuant(2350, units="boat / d")

## ---- unitless-----------------------------------------------------------
FLQuant(34170, units="t") / FLQuant(2.32, units="t")

## ---- missing------------------------------------------------------------
FLQuant(34170, units="t") / FLQuant(2.32, units="NA")

