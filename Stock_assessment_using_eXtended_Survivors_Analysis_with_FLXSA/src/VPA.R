# Copyright 2013 JRC FISHREG. Distributed under the GPL 2 or later
# Maintainer: JRC FISHREG
# Ispra, 18th - 22nd March, 2013

#####################################################################
# VPA and XSA models with FLAssess and FLXSA
#####################################################################
#====================================================================
# VPA
#====================================================================
library(FLAssess)
data(ple4)

#--------------------------------------------------------------------
# set final year and final age F values
#--------------------------------------------------------------------
harvest(ple4)[ac(range(ple4)["max"]), ] <- 1
harvest(ple4)[, ac(range(ple4)["maxyear"])] <- 1

#--------------------------------------------------------------------
# run
#--------------------------------------------------------------------
ple4.vpa <- VPA(ple4, fratio = 1, fit.plusgroup = T)
ple4.new <- ple4 + ple4.vpa
plot(FLStocks(ple4=ple4, vpa=ple4.new))

#--------------------------------------------------------------------
# sensitivity to final values
#--------------------------------------------------------------------

setFinal <- function(object, val){
  harvest(object)[ac(range(object)["max"]), ] <- val
	harvest(object)[, ac(range(object)["maxyear"])] <- val
	object
}

finals <- seq(0.5,1.5,0.1)
lst <- lapply(as.list(finals), setFinal, object=ple4)
lst <- lapply(lst, VPA, fratio=1)
stks <- lapply(lst, "+", ple4)
stks <- lapply(stks, window, start=2000)


plot(FLStocks(stks))

#--------------------------------------------------------------------
# Exercise
#--------------------------------------------------------------------
# run vpas with distinct options

#====================================================================
# XSA
#====================================================================

library(FLXSA)
data(ple4.indices)
plot(ple4.indices)

plot(ple4.indices[["SNS"]], type="ts")
#--------------------------------------------------------------------
# Select tuning fleets
#--------------------------------------------------------------------

ple4.tun.sel <- FLIndices(trim(ple4.indices[[1]],age=2:8),
													ple4.indices[[2]],
													trim(ple4.indices[[3]], year=1997:2003))
names(ple4.tun.sel) <- names(ple4.indices)

#--------------------------------------------------------------------
# Set plus group
#--------------------------------------------------------------------
# calculate the catch, landings and discards numbers in the plusgroup, as well as calculating the weights in the plusgroup, based on a weighted average of the ages in the plusgroup

ple4.sel <- setPlusGroup(ple4, plusgroup=10)

#--------------------------------------------------------------------
# Control file for XSA
#--------------------------------------------------------------------
# inspect, there are a default set of options, for full explanation of meaning read thoroghly the user manual of XSA and the help file
FLXSA.control()

# set
xsa.control <- FLXSA.control(tol = 1e-09, maxit = 30, min.nse = 0.3, fse = 2.0, rage = -1, qage = 6, shk.n = TRUE, shk.f = TRUE, shk.yrs = 5, shk.ages= 2, window = 100, tsrange = 99, tspower = 0)

#--------------------------------------------------------------------
# run
#--------------------------------------------------------------------
FLXSA method takes three arguments: FLStock object (catch-at-age matrix), FLIndices (tuning indices), and an FLXSA.control object (parameter settings for XSA)

xsa.results <- FLXSA(ple4.sel, ple4.tun.sel, xsa.control)


#--------------------------------------------------------------------
# now have a look at the XSA diagnostics
#--------------------------------------------------------------------
diagnostics(xsa.results)

# catchability Residuals by survey need to be extracted from the xsa.results 
# accessor is index.res, but there are no names so need to reassign them

names(index.res(xsa.results)) <- lapply(ple4.tun.sel,'name')
bubbles(age~year|qname, data=mcf(index.res(xsa.results)))

#--------------------------------------------------------------------
# incorporate results of XSA in FLQuant of ple4
#--------------------------------------------------------------------
ple4.sel <- ple4.sel + xsa.results
plot(ple4.sel)

#--------------------------------------------------------------------
# run a retrospective analysis
#--------------------------------------------------------------------
# what does it mean?
retro.years <- 2004:2008
ple4.retro <- tapply(retro.years, 1:length(retro.years), function(x){
	window(ple4,end=x)+FLXSA(window(ple4,end=x),ple4.indices)
})

# coerce into FLStocks object
ple4.retro <- FLStocks(ple4.retro)
# full retrospective summary plot
plot(ple4.retro)

#--------------------------------------------------------------------
#====================================================================
# Exercise on XSA
#====================================================================
library(FLa4a)
data(hake)

#Run your own XSA on hake, assess the levels in SSB and F and verify the retrospective patterns



#====================================================================
# Introducing uncertainty on stock assessment
#====================================================================
#--------------------------------------------------------------------
# bootstraping catchability residuals
# for simplicity we'll use one index only
#--------------------------------------------------------------------
# set nits and seed
set.seed(1234)
nits <- 25

# run xsa
data(ple4.index)
ple4.xsa <- FLXSA(ple4, ple4.index, FLXSA.control())

# sample with replacement by randomly selecting years
# this way correlations between ages are preserved.

x <- dims(ple4.index)$minyear:dims(ple4.index)$maxyear
size <- dims(ple4.index)$year*nits
mc.yrs <-sample(x, size, TRUE)

# then create an FLQuant for the residuals with the right dimensions
dmns <- dimnames(ple4.index@index)
dmns$iter<-1:nits
dev.index <- FLQuant(c(ple4.xsa@index.res[[1]][,ac(mc.yrs)]), dimnames=dmns)
# NOTE THE USAGE OF THE RECYCLING RULE ON OUR BENEFIT
plot(dev.index)

# bootstrap the index
ple4.index@index <- ple4.xsa@index.hat[[1]]*exp(dev.index)

# rerun the assessment 100 times and put results in stock
ple4.bxsa <- FLXSA(ple4, ple4.index, FLXSA.control(), diag.flag=F)
ple4.boot <- ple4 + ple4.bxsa

#plot bootstrap results wrt SSB timeseries
plot(ple4.boot[,as.character(2000:2008)])

#--------------------------------------------------------------------
# propagate into S/R
#--------------------------------------------------------------------

ple4SR <- fmle(as.FLSR(ple4.boot, model="ricker"))
params(ple4SR)
