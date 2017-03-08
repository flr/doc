## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("copula","triangle", "coda", "XML", "reshape2", "latticeExtra"))
## # from FLR
## install.packages(c("FLCore", "FLa4a"), repos="http://flr-project.org/R")

## ---- pkgs---------------------------------------------------------------
# This chunk loads all necessary packages, trims pkg messages
library(FLa4a)
library(XML)
library(reshape2)
library(latticeExtra)
# datasets
data(ple4)
data(ple4.indices)
data(ple4.index)
data(rfLen)

## ---- showClass_a4aM-----------------------------------------------------
showClass("a4aM")

## ---- m_02---------------------------------------------------------------
mod02 <- FLModelSim(model=~a, params=FLPar(a=0.2))
m1 <- a4aM(level=mod02)
m1

## ---- jensen_second_m----------------------------------------------------
shape2 <- FLModelSim(model=~exp(-age-0.5))
level2 <- FLModelSim(model=~1.5*k, params=FLPar(k=0.4))
m2 <- a4aM(shape=shape2, level=level2)
m2

## ---- gis_shape----------------------------------------------------------
shape_len <- FLModelSim(model=~K*(linf/len)^1.5, params=FLPar(linf=60, K=0.4))
m_len <- a4aM(shape=shape_len)

## ---- nao_m--------------------------------------------------------------
# Get NAO
nao.orig <- read.table("https://www.esrl.noaa.gov/psd/data/correlation/nao.data", skip=1, nrow=62, na.strings="-99.90")
dnms <- list(quant="nao", year=1948:2009, unit="unique", season=1:12, area="unique")
# Build an FLQuant from the NAO data
nao.flq <- FLQuant(unlist(nao.orig[,-1]), dimnames=dnms, units="nao")
# Build covar by calculating mean over the first 3 months
nao <- seasonMeans(nao.flq[,,,1:3]) 
# Turn into Boolean
nao <- (nao>0)
# Constructor
trend3 <- FLModelSim(model=~1+b*nao, params=FLPar(b=0.5))
shape3 <- FLModelSim(model=~exp(-age-0.5))
level3 <- FLModelSim(model=~1.5*k, params=FLPar(k=0.4))
m3 <- a4aM(shape=shape3, level=level3, trend=trend3)
m3

## ---- mvrnorm_m----------------------------------------------------------
shape4 <- FLModelSim(model=~exp(-age-0.5))
level4 <- FLModelSim(model=~k^0.66*t^0.57, params=FLPar(k=0.4, t=10), vcov=array(c(0.002, 0.01,0.01, 1), dim=c(2,2)))
trend4 <- FLModelSim(model=~1+b*nao, params=FLPar(b=0.5), vcov=matrix(0.02))
m4 <- a4aM(shape=shape4, level=level4, trend=trend4)
# Call mvrnorm()
m4 <- mvrnorm(100, m4)
m4

## ---- mvrnorm_m1---------------------------------------------------------
m4@level

## ---- mvrnorm_m2---------------------------------------------------------
params(trend(m4))

## ---- mvrnorm_m3---------------------------------------------------------
params(shape(m4))

## ---- univariate_m-------------------------------------------------------
m4 <- a4aM(shape=shape4, level=mvrnorm(100, level4), trend=mvrnorm(100, trend4))

## ---- gis_copula---------------------------------------------------------
linf <- 60
k <- 0.4
# vcov matrix (make up some values)
mm <- matrix(NA, ncol=2, nrow=2)
# 10% cv
diag(mm) <- c((linf*0.1)^2, (k*0.1)^2)
# 0.2 correlation
mm[upper.tri(mm)] <- mm[lower.tri(mm)] <- c(0.05)
# a good way to check is using cov2cor
cov2cor(mm)
# create object
mgis2 <- FLModelSim(model=~k*(linf/len)^1.5, params=FLPar(linf=linf, k=k), vcov=mm)
# set the lower, upper and (optionally) centre of the parameters (without the centre, the triangle is symmetrical)
pars <- list(list(a=55,b=65), list(a=0.3, b=0.6, c=0.35))
mgis2 <- mvrtriangle(1000, mgis2, paramMargins=pars)
mgis2

## ---- plot_tri_gis_m, echo=FALSE, fig.cap="Parameter estimates for Gislason's second natural mortality model from using a triangle distribution."----
splom(t(params(mgis2)@.Data), par.settings=list(plot.symbol=list(pch=19, cex=0.1, col=1)))

## ---- plot_tri_gis_m_hist, echo=FALSE, fig.cap="Marginal distributions of the parameters for Gislason's second natural mortality model using a triangle distribution."----
par(mfrow=c(2,1))
hist(c(params(mgis2)["linf",]), main="Linf", xlab="")
hist(c(params(mgis2)["k",]), main="K", xlab="")

## ---- making_complicated_m-----------------------------------------------
m5 <- a4aM(shape=mgis2, level=level4, trend=trend4)
# or
m5 <- m4
shape(m5) <- mgis2

## ---- simple_m-----------------------------------------------------------
m1

## ---- simple_m1----------------------------------------------------------
range(m1)

## ---- simple_m2----------------------------------------------------------
m(m1)

## ---- simple_m3----------------------------------------------------------
rngquant(m1) <- c(0,7)			# set the quant range
rngyear(m1) <- c(2000, 2010)	# set the year range
range(m1)

## ---- simple_m4----------------------------------------------------------
m(m1)

## ---- m2-----------------------------------------------------------------
m2
rngquant(m2) <- c(0,7)
rngyear(m2) <- c(2000, 2003)
range(m2)
m(m2)

## ---- m2_1---------------------------------------------------------------
predict(level(m2))

## ---- m2_2---------------------------------------------------------------
m(m2)["0"]

## ---- m2_3---------------------------------------------------------------
rngmbar(m2)<- c(0,5)
range(m2)

## ---- m2_4---------------------------------------------------------------
m(m2)

## ---- m2_5---------------------------------------------------------------
quantMeans(m(m2)[as.character(0:5)])

## ---- m3_trend-----------------------------------------------------------
m(m3, nao=1)

## ---- m3_trend1----------------------------------------------------------
rngquant(m3) <- c(0,7)
m(m3, nao=0)

## ---- m3_trend2----------------------------------------------------------
rngyear(m3) <- c(2000, 2003)
m(m3, nao=as.numeric(nao[,as.character(2000:2003)]))

## ---- m4_uncertainty_m---------------------------------------------------
rngquant(m4) <- c(0,7)
rngyear(m4) <- c(2000, 2003)
flq <- m(m4, nao=as.numeric(nao[,as.character(2000:2003)]))
flq
dim(flq)

## ---- uncertain_m, echo=FALSE, fig.cap="Natural mortality with age and year trend."----
bwplot(data~factor(age)|year, data=flq, par.settings=list(plot.symbol=list(cex=0.2, col="gray50"), box.umbrella=list(col="gray40"), box.rectangle=list(col="gray30")), ylab="M", xlab="age (years)", scales=list(x=list(rot=90)))

