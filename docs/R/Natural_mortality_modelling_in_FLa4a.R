## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("copula","triangle", "coda", "XML", "reshape2", "latticeExtra"))
## # from FLR
## install.packages(c("FLCore", "FLa4a"), repos="http://flr-project.org/R")

## ---- pkgs---------------------------------------------------------------
# Load all necessary packages and datasets, trim pkg messages
library(FLa4a)
library(XML)
library(reshape2)
library(latticeExtra)
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
shape_len <- FLModelSim(model=~k*(linf/len)^1.5, params=FLPar(linf=60, k=0.4))
m_len <- a4aM(shape=shape_len)

## ---- nao_m--------------------------------------------------------------
# Get NAO
nao.orig <- read.table("https://www.esrl.noaa.gov/psd/data/correlation/nao.data", skip=1, nrow=62, na.strings="-99.90")
dnms <- list(quant="nao", year=1948:2009, unit="unique", season=1:12, area="unique")
# Build an FLQuant from the NAO data, with the season slot representing months
nao.flq <- FLQuant(unlist(nao.orig[,-1]), dimnames=dnms, units="nao")
# Build covar by calculating the mean over the first 3 months
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
# Create the object using shape, level and trend models
shape4 <- FLModelSim(model=~exp(-age-0.5))
level4 <- FLModelSim(model=~k^0.66*t^0.57, params=FLPar(k=0.4, t=10), vcov=array(c(0.002, 0.01, 0.01, 1), dim=c(2,2)))
trend4 <- FLModelSim(model=~1+b*nao, params=FLPar(b=0.5), vcov=matrix(0.02))
m4 <- a4aM(shape=shape4, level=level4, trend=trend4)
# Call mvrnorm()
m4 <- mvrnorm(100, m4)
m4
# Inspect the models (e.g. level)
level(m4) #can also be done with m4@level
# Note the variance in the parameters (e.g. trend) 
params(trend(m4))
# Note the shape model has no parameters and no uncertainty
params(shape(m4))

## ---- univariate_m-------------------------------------------------------
m4 <- a4aM(shape=shape4, level=mvrnorm(100, level4), trend=mvrnorm(100, trend4))

## ---- gis_copula---------------------------------------------------------
linf <- 60
k <- 0.4
# vcov matrix (make up some values)
mm <- matrix(NA, ncol=2, nrow=2)
# calculate variances assuming a 10% cv
diag(mm) <- c((linf*0.1)^2, (k*0.1)^2)
# calculate covariances assuming a correlation of 0.2
mm[upper.tri(mm)] <- mm[lower.tri(mm)] <- sqrt(prod(diag(mm)))*0.2
# a good way to check is using cov2cor
cov2cor(mm)
# create the FLModelSim object
mgis2 <- FLModelSim(model=~k*(linf/len)^1.5, params=FLPar(linf=linf, k=k), vcov=mm)
# set the lower (a), upper (b) and (optionally) centre (c) of the parameters linf and k (note, without the centre, the triangle is symmetrical)
pars <- list(list(a=55,b=65), list(a=0.3, b=0.6, c=0.35))
# generate 1000 sample sets using mvrtriangle
mgis2 <- mvrtriangle(1000, mgis2, paramMargins=pars)
mgis2

## ---- plot_tri_gis_m, echo=FALSE, fig.cap="Parameter estimates for Gislason's second natural mortality model based on a triangle distribution."----
splom(t(params(mgis2)@.Data), par.settings=list(plot.symbol=list(pch=19, cex=0.1, col=1)))

## ---- plot_tri_gis_m_hist, echo=FALSE, fig.cap="Marginal distributions of the parameters for Gislason's second natural mortality model using a triangle distribution."----
par(mfrow=c(2,1))
hist(c(params(mgis2)["linf",]), main="linf", xlab="")
hist(c(params(mgis2)["k",]), main="k", xlab="")

## ---- making_complicated_m-----------------------------------------------
#Using the constructor
m5 <- a4aM(shape=mgis2, level=level4, trend=trend4)
# or the set method for shape to change m4 previously created
m5 <- m4
shape(m5) <- mgis2

## ---- simple_m-----------------------------------------------------------
# Start with the simplest model
m1
# Check the range
range(m1) # no ages or years...
m(m1)     # confirms no ages or years
# Set the quant and year ranges
range(m1, c('min', 'max')) <- c(0,7)			 # set the quant range
range(m1, c('minyear', 'maxyear')) <- c(2000, 2010) # set the year range
range(m1)
# Show the object with the M estimates by age and year
# (note the name of the first dimension is 'quant')
m(m1)

## ---- m2-----------------------------------------------------------------
# Check the model and set the ranges
m2
range(m2, c('min', 'max')) <- c(0,7)			 # set the quant range
range(m2, c('minyear', 'maxyear')) <- c(2000, 2010) # set the year range
range(m2)
m(m2)
# Note that the level value is
c(predict(level(m2)))
# which is the same as
m(m2)["0"]
# This is because the mbar range is currently set to "0" and "0" (see above)
# and the mean natural mortality value over this range is given by the level model. 
# We can change the mbar range
range(m2, c('minmbar', 'maxmbar')) <- c(0,7)			 # set the quant range
range(m2)
# which rescales the the natural mortality at age
m(m2)
# Check that the mortality over the mean range is the same as the level model
quantMeans(m(m2)[as.character(0:5)])

## ---- m3_trend-----------------------------------------------------------
# Pass in a single nao value (only one year, because the trend model needs
# at least one value)
m(m3, nao=1)
# Set ages
range(m3, c('min', 'max')) <- c(0,7)			 # set the quant range
m(m3, nao=0)
# With ages and years - passing in the NAO data as numeric (1,0,1,0)
range(m3, c('minyear', 'maxyear')) <- c(2000, 2003) # set the year range
m(m3, nao=as.numeric(nao[,as.character(2000:2003)]))

## ---- m4_uncertainty_m---------------------------------------------------
range(m4, c('min', 'max')) <- c(0,7)			 # set the quant range
range(m4, c('minyear', 'maxyear')) <- c(2000, 2003)
flq <- m(m4, nao=as.numeric(nao[,as.character(2000:2003)]))
flq
dim(flq)

## ---- uncertain_m, echo=FALSE, fig.cap="Natural mortality with age and year trend."----
bwplot(data~factor(age)|year, data=flq, par.settings=list(plot.symbol=list(cex=0.2, col="gray50"), box.umbrella=list(col="gray40"), box.rectangle=list(col="gray30")), ylab="M", xlab="age (years)", scales=list(x=list(rot=90)))

