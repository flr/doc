#####################################################################
# SCA
# Statistical catch-at-age framework for stock assessment
#####################################################################
# submodels
#	fmodel
#	qmodel
#	srmodel
#	vmodel
#	n1model

# fit types = "MP" or "assessment"

# fit methods simple = "sca" or advanced = "a4aSCA" 

#====================================================================
# Load
#====================================================================

library(FLa4a)
library(diagram)
data(ple4)
data(ple4.indices)
source("funs.R")

#====================================================================
# Quick and dirty
#====================================================================
library(FLa4a)
data(ple4)
data(ple4.indices)
fit <- sca(ple4, ple4.indices)
res <- residuals(fit, ple4, ple4.indices)
plot(res, main="Residuals")
bubbles(res)
qqmath(res)

stk <- ple4 + fit
plot(stk, main="Stock summary")

wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))

wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")

#====================================================================
# Data structures
#====================================================================

showClass("a4aFit")

plotS4("a4aFit", main="a4aFit class", lwd = 1, box.lwd = 2, cex.txt = 0.8, box.size = 0.1, box.type = "square", box.prop = 0.3)

showClass("a4aFitSA")

plotS4("a4aFitSA", main="a4aFitSA class", lwd = 1, box.lwd = 2, cex.txt = 0.8, box.size = 0.1, box.type = "square", box.prop = 0.3)

showClass("SCAPars")
showClass("a4aStkParams")
showClass("submodel")

plotS4("SCAPars", main="SCAPars class", lwd = 1, box.lwd = 2, cex.txt = 0.8, box.size = 0.1, box.type = "square", box.prop = 0.3)

plotS4("a4aStkParams", main="a4aStkParams class", lwd = 1, box.lwd = 2, cex.txt = 0.8, box.size = 0.1, box.type = "square", box.prop = 0.3)

plotS4("submodel", main="submodel class", lwd = 1, box.lwd = 2, cex.txt = 0.8, box.size = 0.1, box.type = "square", box.prop = 0.3)

#====================================================================
# The sca method - statistical catch-at-age
#====================================================================

#--------------------------------------------------------------------
# fishing mortality submodel
#--------------------------------------------------------------------

qmodel <- list(~ factor(age)) 

fmodel <- ~ factor(age) + factor(year)
fit <- sca(stock = ple4, indices = ple4.indices[1], fmodel=fmodel, qmodel=qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit)), drape = TRUE, screen = list(x = -90, y=-45))

fmodel <- ~ s(age, k=4) + s(year, k = 20)
fit1 <- sca(ple4, ple4.indices[1], fmodel, qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit1)), drape = TRUE, screen = list(x = -90, y=-45))

fmodel <- ~ s(age, k=4) + s(year, k = 20) + te(age, year, k = c(3,3))
fit2 <- sca(ple4, ple4.indices[1], fmodel, qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit2)), drape = TRUE, screen = list(x = -90, y=-45))

fmodel <- ~ te(age, year, k = c(4,20))
fit3 <- sca(ple4, ple4.indices[1], fmodel, qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit3)), drape = TRUE, screen = list(x = -90, y=-45))

fmodel <- ~ te(age, year, k = c(4,20)) + s(year, k = 5, by = as.numeric(age==1))
fit4 <- sca(ple4, ple4.indices[1], fmodel, qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit4)), drape = TRUE, screen = list(x = -90, y=-45))

#--------------------------------------------------------------------
# catchability submodel
#--------------------------------------------------------------------

sfrac <- mean(range(ple4.indices[[1]])[c("startf", "endf")])
fmodel <- ~ factor(age) + factor(year)

qmodel <- list(~ factor(age)) 
fit <- sca(ple4, ple4.indices[1], fmodel, qmodel)
Z <- (m(ple4) + harvest(fit))*sfrac # check M * sfrac
lst <- dimnames(fit@index[[1]])
lst$x <- stock.n(fit)*exp(-Z)
stkn <- do.call("trim", lst)
wireframe(data ~ age + year, data = as.data.frame(index(fit)[[1]]/stkn), drape = TRUE, screen = list(x = -90, y=-45))

qmodel <- list(~ s(age, k=4))
fit1 <- sca(ple4, ple4.indices[1], fmodel, qmodel)
Z <- (m(ple4) + harvest(fit1))*sfrac
lst <- dimnames(fit1@index[[1]])
lst$x <- stock.n(fit1)*exp(-Z)
stkn <- do.call("trim", lst)
wireframe(data ~ age + year, data = as.data.frame(index(fit1)[[1]]/stkn), drape = TRUE, screen = list(x = -90, y=-45))

qmodel <- list(~ te(age, year, k = c(3,40)))
fit2 <- sca(ple4, ple4.indices[1], fmodel, qmodel)
Z <- (m(ple4) + harvest(fit2))*sfrac
lst <- dimnames(fit2@index[[1]])
lst$x <- stock.n(fit2)*exp(-Z)
stkn <- do.call("trim", lst)
wireframe(data ~ age + year, data = as.data.frame(index(fit2)[[1]]/stkn), drape = TRUE, screen = list(x = -90, y=-45))

qmodel <- list( ~ s(age, k=4) + year)
fit3 <- sca(ple4, ple4.indices[1], fmodel, qmodel)
Z <- (m(ple4) + harvest(fit3))*sfrac
lst <- dimnames(fit3@index[[1]])
lst$x <- stock.n(fit3)*exp(-Z)
stkn <- do.call("trim", lst)
wireframe(data ~ age + year, data = as.data.frame(index(fit3)[[1]]/stkn), drape = TRUE, screen = list(x = -90, y=-45))

#--------------------------------------------------------------------
# stock-recruitment submodel
#--------------------------------------------------------------------

fmodel <- ~ s(age, k=4) + s(year, k = 20)
qmodel <- list(~ s(age, k=4))

srmodel <- ~ factor(year)
fit <- sca(ple4, ple4.indices[1], fmodel=fmodel, qmodel=qmodel, srmodel=srmodel) 
srmodel <- ~ s(year, k=20)
fit1 <- sca(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 
srmodel <- ~ ricker(CV=0.05)
fit2 <- sca(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 
srmodel <- ~ bevholt(CV=0.05)
fit3 <- sca(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 
srmodel <- ~ hockey(CV=0.05)
fit4 <- sca(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 
srmodel <- ~ geomean(CV=0.05)
fit5 <- sca(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 

flqs <- FLQuants(fac=stock.n(fit)[1], bh=stock.n(fit3)[1])

xyplot(data~year, groups=qname, data=flqs, type="l", main="Recruitment models", auto.key=list(points=FALSE, lines=TRUE, columns=3))

#====================================================================
# The a4aSCA method - statistical catch-at-age
#====================================================================

fmodel <- ~ s(age, k=4) + s(year, k = 20)
qmodel <- list( ~ s(age, k=4) + year)
srmodel <- ~s(year, k=20)
fit <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 

#--------------------------------------------------------------------
# N1 submodel
#--------------------------------------------------------------------

n1model <- ~s(age, k=4)
fit1 <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel, n1model) 
flqs <- FLQuants(smo=stock.n(fit1), fac=stock.n(fit))

xyplot(data~age|year, groups=qname, data=flqs, type="l", main="N1 models", auto.key=list(points=FALSE, lines=TRUE, columns=2))

#--------------------------------------------------------------------
# Variance submodel
#--------------------------------------------------------------------

vmodel <- list(~1, ~1)
fit <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel, n1model, vmodel) 
vmodel <- list(~ s(age, k=4), ~1)
fit1 <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel, n1model, vmodel) 
flqs <- FLQuants(cts=catch.n(fit), smo=catch.n(fit1))

xyplot(data~year|age, groups=qname, data=flqs, type="l", main="Variance models", scales=list(y=list(relation="free")), auto.key=list(points=FALSE, lines=TRUE, columns=2))

predict(fit)$vmodel$catch

wireframe(data ~ age + year, data = as.data.frame(predict(fit1)$vmodel$catch), drape = TRUE, screen = list(x = -90, y=-45))

#--------------------------------------------------------------------
# Working with covariates
#--------------------------------------------------------------------

nao <- read.table("http://www.cdc.noaa.gov/data/correlation/nao.data", skip=1, nrow=62, na.strings="-99.90")
dnms <- list(quant="nao", year=1948:2009, unit="unique", season=1:12, area="unique")
nao <- FLQuant(unlist(nao[,-1]), dimnames=dnms, units="nao")
nao <- seasonMeans(trim(nao, year=dimnames(stock.n(ple4))$year))
nao <- as.numeric(nao)

# force covars to be passed through covar argument

srmodel <- ~ nao
fit2 <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 
flqs <- FLQuants(fac=stock.n(fit)[1], cvar=stock.n(fit2)[1])

xyplot(data~year, groups=qname, data=flqs, type="l", main="Recruitment model with covariates")

srmodel <- ~ ricker(a=~nao, CV=0.1)
fit3 <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 
flqs <- FLQuants(fac=stock.n(fit)[1], cvar=stock.n(fit3)[1])

xyplot(data~year, groups=qname, data=flqs, type="l", main="Recruitment model with covariates")


#--------------------------------------------------------------------
# External weigthing of likelihood components
#--------------------------------------------------------------------

library(FLa4a)
data(ple4)
data(ple4.indices)

stk <- ple4
idx <- ple4.indices[1]
# variance of observed catches
varslt <- catch.n(stk)
varslt[] <- 1
catch.n(stk) <- FLQuantDistr(catch.n(stk), varslt) # show: remove var
# variance of observed indices
varslt <- index(idx[[1]])
varslt[] <- 0.05
index.var(idx[[1]]) <- varslt


# run
fit <- a4aSCA(ple4, ple4.indices[1], vmodel=list(~1, ~1))
fit1 <- a4aSCA(stk, idx, vmodel=list(~1, ~1)) 


flqs <- FLQuants(nowgt=stock.n(fit), extwgt=stock.n(fit1))

xyplot(data~year|age, groups=qname, data=flqs, type="l", main="Likelihood weighting", scales=list(y=list(relation="free")), auto.key=list(points=FALSE, lines=TRUE, columns=2))

#--------------------------------------------------------------------
# Assessing ADMB files
#--------------------------------------------------------------------

fit1 <- a4aSCA(stk, idx, fmodel, qmodel, srmodel, n1model, vmodel=list(~1, ~1), wkdir="mytest") 


#--------------------------------------------------------------------
# More models
#--------------------------------------------------------------------

breakpts <- function(var, breaks, ...) {
  if (min(var, na.rm = TRUE) < min(breaks)) breaks <- c(min(var, na.rm = TRUE) - 1, breaks)
  if (max(var, na.rm = TRUE) > max(breaks)) breaks <- c(breaks, max(var, na.rm = TRUE)) 
  label <- paste0("(",breaks[-length(breaks)], ",", breaks[-1], "]")     
  cut(var, breaks = breaks, label = label)  
}

# constant fishing mortality for ages older than 5
fmodel = ~ s(replace(age, age>5, 5), k=4) + s(year, k=20)
fit <- sca(ple4, ple4.indices, fmodel=fmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit)), drape = TRUE, screen = list(x = -90, y=-45))

# the same model to two periods
fmodel=~s(age, k = 3, by = breakpts(year, 2000))
fit <- sca(ple4, ple4.indices, fmodel=fmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit)), drape = TRUE, screen = list(x = -90, y=-45))

# trawl type
fmodel <- ~ trawl(plateau = 5, selectivity = "variable")
fit <- sca(ple4, ple4.indices, fmodel=fmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit)), drape = TRUE, screen = list(x = -90, y=-45))

# smoother for each age 
fmodel <- ~ factor(age) + s(year, k=10, by = breakpts(age, c(2:8)))
fit <- sca(ple4, ple4.indices, fmodel=fmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit)), drape = TRUE, screen = list(x = -90, y=-45))

#====================================================================
# Predict and simulate
#====================================================================

fmodel <- ~ s(age, k=4) + s(year, k = 20)
qmodel <- list( ~ s(age, k=4) + year)
srmodel <- ~s(year, k=20)
fit <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 

#--------------------------------------------------------------------
# Predict
#--------------------------------------------------------------------

fit.pred <- predict(fit)
lapply(fit.pred, names)

#--------------------------------------------------------------------
# Simulate
#--------------------------------------------------------------------

fits <- simulate(fit, 1000)
flqs <- FLQuants(sim=iterMedians(stock.n(fits)), det=stock.n(fit))

xyplot(data~year|age, groups=qname, data=flqs, type="l", main="Median simulations VS fit", scales=list(y=list(relation="free")))

stks <- ple4 + fits
plot(stks)

#====================================================================
# Model averaging
#====================================================================
weightedModelAverage <- function(mods, stock, FUN = AIC, nsim = 1000)
{

  FUN <- match.fun(FUN)

  # calculate weights
  ICs <- -1 * sapply(mods, FUN)
  eICs <- exp( 0.5 * (ICs - max(ICs)))
  weights <- eICs / sum(eICs)

  wt.table <- data.frame("weight (perc)" = round(weights * 100, 3))
  rownames(wt.table) <- names(mods)
  
  message("model weights are \n\t", paste(capture.output(wt.table), collapse = "\n\t"))

  # now sample from each model 1000 times and randomly select those to combine
  mod.sim <- sample(seq_along(mods), nsim, prob = weights, replace = TRUE)

  stock.sim <- propagate(stock, nsim)
  for (i in seq_along(mods)) {
    if (sum(mod.sim == i)) stock.sim[,,,,,mod.sim == i] <- stock.sim[,,,,,mod.sim == i] + mods[[i]]
  }
  # DONE !!

  stock.sim
}

#--------------------------------------------------------------------
# AIC,BIC weigthing
#--------------------------------------------------------------------

f1 <- sca(ple4, ple4.indices, fmodel=~ factor(age) + s(year, k=20), qmodel=list(~ s(age, k = 4), ~ s(age, k = 4), ~ s(age, k = 3)), fit = "assessment")
          
f2 <- sca(ple4, ple4.indices, fmodel=~ factor(age) + s(year, k=20), qmodel=list(~ s(age, k = 4)+year, ~ s(age, k = 4), ~ s(age, k = 3)), fit = "assessment")
 
stock.sim <- weightedModelAverage(list(f1, f2), ple4, AIC, nsim = 1000)

stks <- FLStocks(f1=ple4+f1, f2=ple4+f2, ma=ple4+stock.sim)
flqs <- lapply(stks, ssb)
flqs <- lapply(flqs, iterMedians)
xyplot(data~year, groups=qname, data=flqs, type="l")

plot(stks)

#--------------------------------------------------------------------
# M models
#--------------------------------------------------------------------

shape2 <- FLModelSim(model=~exp(-age-0.5))
level4 <- FLModelSim(model=~k^0.66*t^0.57, params = FLPar(k=0.4, t=10), vcov=matrix(c(0.002, 0.01,0.01, 1), ncol=2))
 
trend4 <- FLModelSim(model=~b, params=FLPar(b=0.5), vcov=matrix(0.02))
m4 <- a4aM(shape=shape2, level=level4, trend=trend4)
#m4 <- mvrnorm(25, m4)
range(m4)[] <- range(ple4)[]
range(m4)[c("minmbar","maxmbar")]<-c(1,1)
flq <- m(m4)[]
quant(flq) <- "age"
stk <- ple4
m(stk) <- flq

f3 <- sca(stk, ple4.indices, fmodel=~ factor(age) + s(year, k=20), qmodel=list(~ s(age, k = 4), ~ s(age, k = 4), ~ s(age, k = 3)), fit = "assessment")
          
stock.sim <- weightedModelAverage(list(f1, f3), ple4, AIC, nsim = 1000)

stks <- FLStocks(f1=ple4+f1, f2=ple4+f3, ma=ple4+stock.sim)
flqs <- lapply(stks, ssb)
flqs <- lapply(flqs, iterMedians)
xyplot(data~year, groups=qname, data=flqs, type="l")

#====================================================================
# Geeky stuff
#====================================================================

#--------------------------------------------------------------------
# Propagate M uncertainty
#--------------------------------------------------------------------

nits <- 25

shape2 <- FLModelSim(model=~exp(-age-0.5))
level4 <- FLModelSim(model=~k^0.66*t^0.57, params = FLPar(k=0.4, t=10), vcov=matrix(c(0.002, 0.01,0.01, 1), ncol=2))
 
trend4 <- FLModelSim(model=~b, params=FLPar(b=0.5), vcov=matrix(0.02))
m4 <- a4aM(shape=shape2, level=level4, trend=trend4)
m4 <- mvrnorm(nits, m4)
range(m4)[] <- range(ple4)[]
range(m4)[c("minmbar","maxmbar")]<-c(1,1)
flq <- m(m4)[]
quant(flq) <- "age"
stk <- propagate(ple4, nits)
m(stk) <- flq

f3 <- sca(stk, ple4.indices, fmodel=~ factor(age) + s(year, k=20), qmodel=list(~ s(age, k = 4), ~ s(age, k = 4), ~ s(age, k = 3)), fit = "assessment")

plot(ple4+f3)
x11()
plot(ple4+f1)

#--------------------------------------------------------------------
# WKSAM exercise
#--------------------------------------------------------------------
fits <- simulate(fit, 25)
stk <- ple4 + fits

fits2 <- a4aSCA(stk, ple4.indices[1], fmodel, qmodel, srmodel, fit="MP")  
flqs <- FLQuants(fit=stock.n(fit), repl=iterMedians(stock.n(fits2)))
xyplot(data~year|age, groups=qname, data=flqs, type="l", scales=list(y=list(relation="free")))


#ks <- seq(10,50,5)
#qmodel <- list( ~ s(age, k=4) + year)
#srmodel <- ~s(year, k=20)
#liks <- liks <- data.frame(k=ks, nlogl=NA)
#for(i in ks){
#	fmodel <- as.formula(substitute(~s(age, k=4) + s(year, k=x), list(x=i)))
#	liks[liks$k==i,2] <- as.numeric(BIC(a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel)))
#}



#fmodel <- ~ s(age, k=4) + s(year, k = 20)
#qmodel <- list( ~ s(age, k=4) + year)
#srmodel <- ~s(year, k=20)
#fit <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 
#fits <- simulate(fit, 25)
#stks <- ple4 + fits 
#idxs <- ple4.indices[1]
#index(idxs[[1]]) <- index(fits)[[1]]
#library(parallel)
#options(mc.cores=1)
#lst <- mclapply(split(1:25, 1:25), function(x){
#	fit <- a4aSCA(stks[,,,,,x], FLIndices(idxs[[1]][,,,,,x]), fmodel, qmodel, srmodel, fit="MP") 
#})
