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
library(latticeExtra)
# datasets
data(ple4)
data(ple4.indices)
data(ple4.index)

## ---- message=TRUE-------------------------------------------------------
data(ple4)
data(ple4.indices)
fit <- sca(ple4, ple4.indices)

## ---- summ, fig.cap="Stock summary for Plaice in ICES area IV, recruits, SSB (Stock Spawning Biomass), catch (catch and landings) and harvest (fishing mortality or F)."----
stk <- ple4 + fit
plot(stk)

## ---- F, fig.cap="3D contour plot of estimated fishing mortality at age and year"----
wireframe(harvest(fit), zlab="F")

## ---- N, fig.cap="Population abundance by age and year", echo=FALSE------
wireframe(stock.n(fit), zlab="N")

## ---- C, fig.cap="Catches in number of individuals by age and year", echo=FALSE----
wireframe(catch.n(fit), zlab="C")

## ------------------------------------------------------------------------
res <- residuals(fit, ple4, ple4.indices)

## ---- res, fig.cap="Standardized residuals for abundance indices (SNS, BTS Tridens and BTS Isis) and for catch numbers (catch.n). Each panel is coded by age class, dots represent standardized residuals and lines a simple smoother."----
plot(res)

## ---- bub, fig.cap="Bubbles plot of standardized residuals for abundance indices (SNS, BTS Tridens and BTS Isis) and for catch numbers (catch.n)."----
bubbles(res)

## ---- qq, fig.cap="Quantile-quantile plot of standardized residuals for abundance indices (SNS, BTS Tridens and BTS Isis) and for catch numbers (catch.n). Each panel is coded by age class, dots represent standardized residuals and lines the normal distribution quantiles."----
qqmath(res)

## ---- selplt, fig.cap="Predict and observed catch-at-age"----------------
plot(fit, ple4)

## ---- idxplt, fig.cap="Predict and observed abundance-at-age"------------
plot(fit, ple4.indices)

## ------------------------------------------------------------------------
fitSumm(fit)
AIC(fit)
BIC(fit)

## ------------------------------------------------------------------------
qmod <- list(~ factor(age))
fmod <- ~ factor(age) + factor(year)
srmod <- ~ factor(year)
fit <- sca(stock = ple4, indices = ple4.indices[1], fmodel=fmod, qmodel=qmod, srmodel=srmod)

## ---- sep1, echo=FALSE, fig.cap="Fishing mortality separable model"------
wireframe(harvest(fit), zlab="F")

## ------------------------------------------------------------------------
fmod <- ~ s(age, k=4) + s(year, k = 20)
# notice that you can specify the submodels without the argument, as an example you
# don't need fmodel=fmod, but the order should be respected...
fit <- sca(ple4, ple4.indices[1], fmod, qmod, srmod)

## ---- sep2, echo=FALSE, fig.cap="Fishing mortality smoothed separable model"----
wireframe(harvest(fit), zlab="F")

## ------------------------------------------------------------------------
fmod <- ~ te(age, year, k = c(4,20))
fit <- sca(ple4, ple4.indices[1], fmod, qmod, srmod)

## ---- te1, echo=FALSE, fig.cap="Fishing mortality smoothed non-separable model"----
wireframe(harvest(fit), zlab="F")

## ------------------------------------------------------------------------
fmod <- ~ te(age, year, k = c(4,20)) + s(year, k = 5, by = as.numeric(age==1))
fit <- sca(ple4, ple4.indices[1], fmod, qmod, srmod)

## ---- age1, echo=FALSE, fig.cap="Fishing mortality age-year interaction model with extra age 1 smoother."----
wireframe(harvest(fit), zlab="F")

## ------------------------------------------------------------------------
fmod <- ~ factor(age) + factor(year)
srmod <- ~ factor(year)

## ------------------------------------------------------------------------
qmod <- list(~ factor(age))
fit <- sca(ple4, ple4.indices[1], fmod, qmod, srmod)

## ------------------------------------------------------------------------
# compute N for the fraction of the year the survey is carried out
sfrac <- mean(range(ple4.indices[[1]])[c("startf", "endf")])
# fraction of total mortality up to that moment
Z <- (m(ple4) + harvest(fit))*sfrac
lst <- dimnames(fit@index[[1]])
# survivors
lst$x <- stock.n(fit)*exp(-Z)
stkn <- do.call("trim", lst)
qhat <- index(fit)[[1]]/stkn

## ---- dummyage, echo=FALSE, fig.cap="Catchability age independent model"----
wireframe(qhat, zlab="q")

## ------------------------------------------------------------------------
qmod <- list(~ s(age, k=4))
fit <- sca(ple4, ple4.indices[1], fmod, qmod, srmod)

# compute N for the fraction of the year the survey is carried out
Z <- (m(ple4) + harvest(fit))*sfrac
lst <- dimnames(fit@index[[1]])
lst$x <- stock.n(fit)*exp(-Z)
stkn <- do.call("trim", lst)
qhat <- index(fit)[[1]]/stkn

## ---- smoothage, echo=FALSE, fig.cap="Catchability smoother age model"----
wireframe(qhat, zlab="q")

## ------------------------------------------------------------------------
qmod <- list(~ te(age, year, k = c(3,40)))
fit <- sca(ple4, ple4.indices[1], fmod, qmod, srmod)

# compute N for the fraction of the year the survey is carried out
Z <- (m(ple4) + harvest(fit))*sfrac
lst <- dimnames(fit@index[[1]])
lst$x <- stock.n(fit)*exp(-Z)
stkn <- do.call("trim", lst)
qhat <- index(fit)[[1]]/stkn

## ---- te2, echo=FALSE, fig.cap="Catchability tensor product of age and year"----
wireframe(qhat, zlab="q")

## ------------------------------------------------------------------------
qmod <- list( ~ s(age, k=4) + year)
fit <- sca(ple4, ple4.indices[1], fmod, qmod, srmod)

# compute N for the fraction of the year the survey is carried out
Z <- (m(ple4) + harvest(fit))*sfrac
lst <- dimnames(fit@index[[1]])
lst$x <- stock.n(fit)*exp(-Z)
stkn <- do.call("trim", lst)
qhat <- index(fit)[[1]]/stkn

## ---- qtrend, echo=FALSE, fig.cap="Catchability with a linear trend in year"----
wireframe(qhat, zlab="q")

## ------------------------------------------------------------------------
# simulating a biomass index (note the name of the first dimension element) using 
# the ple4 biomass and an arbritary catchability of 0.001 plus a lognormal error.
dnms <- list(age="all", year=range(ple4)["minyear"]:range(ple4)["maxyear"])
bioidx <- FLIndexBiomass(FLQuant(NA, dimnames=dnms))
index(bioidx) <- stock(ple4)*0.001
index(bioidx) <- index(bioidx)*exp(rnorm(index(bioidx), sd=0.1))
range(bioidx)[c("startf","endf")] <- c(0,0)

# note the name of the first dimension element
index(bioidx)

# fitting the model
fit <- sca(ple4, FLIndices(bioidx), qmodel=list(~1))

## ---- resbio, fig.cap="Catchability residuals for a biomass index", echo=FALSE----
plot(residuals(fit, ple4, FLIndices(bioidx)))

## ------------------------------------------------------------------------
# creating the index
dnms <- list(age="all", year=range(ple4)["minyear"]:range(ple4)["maxyear"])
bioidx <- FLIndexBiomass(FLQuant(NA, dimnames=dnms))
# but now use only ages 2:4
index(bioidx) <- tsb(ple4[ac(2:4)])*0.001
index(bioidx) <- index(bioidx)*exp(rnorm(index(bioidx), sd=0.1))
range(bioidx)[c("startf","endf")] <- c(0,0)
# to pass this information to the model one needs to specify an age range
range(bioidx)[c("min","max")] <- c(2,4)

# fitting the model
fit <- sca(ple4, FLIndices(bioidx), qmodel=list(~1))

## ---- resbio2, fig.cap="Catchability residuals for a biomass index", echo=FALSE----
plot(residuals(fit, ple4, FLIndices(bioidx)))

## ------------------------------------------------------------------------
fit <- sca(ple4, FLIndices(ple4.index[1]), qmodel=list(~1))

## ---- resrec, fig.cap="Catchability residuals for a single age index", echo=FALSE----
plot(residuals(fit, ple4, FLIndices(ple4.index[1])))

## ------------------------------------------------------------------------
fmod <- ~ s(age, k=4) + s(year, k = 20)
qmod <- list(~ s(age, k=4))

## ------------------------------------------------------------------------
srmod <- ~ factor(year)
fit <- sca(ple4, ple4.indices[1], fmod, qmod, srmod)
srmod <- ~ s(year, k=20)
fit1 <- sca(ple4, ple4.indices[1], fmod, qmod, srmod)
srmod <- ~ ricker(CV=0.1)
fit2 <- sca(ple4, ple4.indices[1], fmod, qmod, srmod)
srmod <- ~ bevholt(CV=0.1)
fit3 <- sca(ple4, ple4.indices[1], fmod, qmod, srmod)
srmod <- ~ hockey(CV=0.2)
fit4 <- sca(ple4, ple4.indices[1], fmod, qmod, srmod)
srmod <- ~ geomean(CV=0.1)
fit5 <- sca(ple4, ple4.indices[1], fmod, qmod, srmod)

## ---- srmod, fig.cap="Stock-recruitment models fits", echo=FALSE---------
flqs <- FLQuants(factor=stock.n(fit)[1], smother=stock.n(fit1)[1], ricker=stock.n(fit2)[1], bevholt=stock.n(fit3)[1], hockey=stock.n(fit4)[1], geomean=stock.n(fit5)[1])
xyplot(data~year, groups=qname, data=flqs, type="l", auto.key=list(points=FALSE, lines=TRUE, columns=3), ylab="No. recruits")

## ------------------------------------------------------------------------
# the age effect
ageeffect <- ~ factor(age)

# the year effect
yeareffect <- ~ factor(year)

# the cohort
cohorteffect <- ~ factor(year-age)

# the fits
fit1 <- sca(ple4, ple4.indices, fmodel=yeareffect)
fit2 <- sca(ple4, ple4.indices, fmodel=ageeffect)
fit3 <- sca(ple4, ple4.indices, fmodel=cohorteffect)

## ---- majeffy, fig.cap="Major effects: the year effect (~ factor(year))", echo=FALSE----
wireframe(harvest(fit1), main='year effect')

## ---- majeffa, fig.cap="Major effects: the age effect (~ factor(age))", echo=FALSE----
wireframe(harvest(fit2), main='age effect')

## ---- majeffc, fig.cap="Major effects: the cohort effect (~ factor(year-age))", echo=FALSE----
wireframe(harvest(fit3), main='cohort effect')

## ------------------------------------------------------------------------
fit <- a4aSCA(ple4, ple4.indices[1])
submodels(fit)

## ------------------------------------------------------------------------
n1mod <- ~s(age, k=4)
fit1 <- a4aSCA(ple4, ple4.indices[1], n1model=n1mod)
flqs <- FLQuants(smother=stock.n(fit1)[,1], factor=stock.n(fit)[,1])

## ---- ny1, fig.cap="Nay=1 models", echo=FALSE----------------------------
xyplot(data~age, groups=qname, data=flqs, type="l",
       auto.key=list(points=FALSE, lines=TRUE, columns=2),
       par.settings=list(superpose.line=list(col=c("gray35", "black")),
       strip.background=list(col="gray90")), ylab="")

## ------------------------------------------------------------------------
vmod <- list(~1, ~1)
fit1 <- a4aSCA(ple4, ple4.indices[1], vmodel=vmod)
vmod <- list(~ s(age, k=4), ~1)
fit2 <- a4aSCA(ple4, ple4.indices[1], vmodel=vmod)
flqs <- FLQuants(cts=catch.n(fit1), smo=catch.n(fit2))

## ---- varmod, fig.cap="Population estimates using two different variance models", echo=FALSE----
xyplot(data~year|age, groups=qname, data=flqs, type="l",
       scales=list(y=list(relation="free", draw=FALSE)),
       auto.key=list(points=FALSE, lines=TRUE, columns=2),
       par.settings=list(superpose.line=list(col=c("gray35", "black")),
       strip.background=list(col="gray90")), ylab="")

## ------------------------------------------------------------------------
nao <- read.table("https://www.esrl.noaa.gov/psd/data/correlation/nao.data", skip=1,
                  nrow=62, na.strings="-99.90")
dnms <- list(quant="nao", year=1948:2009, unit="unique", season=1:12, area="unique")
nao <- FLQuant(unlist(nao[,-1]), dimnames=dnms, units="nao")
nao <- seasonMeans(trim(nao, year=dimnames(stock.n(ple4))$year))
nao <- as.numeric(nao)

## ------------------------------------------------------------------------
srmod <- ~ nao
fit2 <- sca(ple4, ple4.indices[1], qmodel=list(~s(age, k=4)), srmodel=srmod)
flqs <- FLQuants(simple=stock.n(fit)[1], covar=stock.n(fit2)[1])

## ---- naor, echo=FALSE, fig.cap="Recruitment model with covariates. Using the NAO index as a recruitment index."----
xyplot(data~year, groups=qname, data=flqs, type="l",
       auto.key=list(points=FALSE, lines=TRUE, columns=2),
       par.settings=list(superpose.line=list(col=c("gray35", "black")),
       strip.background=list(col="gray90")), ylab="")

## ------------------------------------------------------------------------
srmod <- ~ ricker(a=~nao, CV=0.1)
fit3 <- sca(ple4, ple4.indices[1], qmodel=list(~s(age, k=4)), srmodel=srmod)
flqs <- FLQuants(simple=stock.n(fit)[1], covar=stock.n(fit3)[1])

## ---- naor2, echo=FALSE, fig.cap="Recruitment model with covariates. Using the NAO index as a covariate for the stock-recruitment model parameters."----
xyplot(data~year, groups=qname, data=flqs, type="l",
       auto.key=list(points=FALSE, lines=TRUE, columns=2),
       par.settings=list(superpose.line=list(col=c("gray35", "black")),
       strip.background=list(col="gray90")), ylab="")

## ---- eval=FALSE---------------------------------------------------------
## fit1 <- a4aSCA(ple4, ple4.indices, wkdir="fit1run")

## ------------------------------------------------------------------------
fit <- sca(ple4, ple4.indices[1], fit="assessment")

## ------------------------------------------------------------------------
fit.pred <- predict(fit)
lapply(fit.pred, names)

## ------------------------------------------------------------------------
fits <- simulate(fit, 100)
flqs <- FLQuants(sim=iterMedians(stock.n(fits)), det=stock.n(fit))

## ---- sim, echo=FALSE, fig.cap="Median simulations VS fit"---------------
xyplot(data~year|age, groups=qname, data=flqs, type="l",
       scales=list(y=list(relation="free", draw=FALSE)),
       auto.key=list(points=FALSE, lines=TRUE, columns=2),
       par.settings=list(superpose.line=list(col=c("gray35", "black")),
       strip.background=list(col="gray90")), ylab="")

## ---- sim2, echo=FALSE, fig.cap="Stock summary of the simulated and fitted data"----
stks <- ple4 + fits
plot(stks)

## ------------------------------------------------------------------------
# ll
fit <- a4aSCA(ple4, ple4.indices)
fit <- simulate(fit, 1000)

## ------------------------------------------------------------------------
# mcmc
mc <- SCAMCMC()
# check the default pars
mc

## ------------------------------------------------------------------------
# fit the model
fitmc1 <- a4aSCA(ple4, ple4.indices, fit="MCMC", mcmc=mc)
# check acceptance rate
fitSumm(fitmc1)

## ---- ,eval=FALSE--------------------------------------------------------
## fitmc1.mc <- as.mcmc(fitmc1)
## plot(fitmc1.mc)

## ---- ,echo=FALSE--------------------------------------------------------
plot(ple4 + fitmc1)

## ------------------------------------------------------------------------
mc <- SCAMCMC(mcmc=10000, mcsave=200, mcprobe=0.4)
fitmc2 <- a4aSCA(ple4, ple4.indices, qmodel=list(~s(age, k=3), ~s(age, k=3),~s(age, k=3)), fit="MCMC", mcmc=mc)

## ---- ,echo=FALSE, fig.width=8, fig.height=10----------------------------
plot(FLStocks(ll=ple4 + fit, mc0=ple4 + fitmc1, mcalt=ple4 + fitmc2))

## ------------------------------------------------------------------------
fit <- sca(ple4, ple4.indices[1], fit="assessment")

## ------------------------------------------------------------------------
stk <- ple4
idx <- ple4.indices[1]
# variance of observed catches
varslt <- catch.n(stk)
varslt[] <- 0.4
catch.n(stk) <- FLQuantDistr(catch.n(stk), varslt)
# variance of observed indices
varslt <- index(idx[[1]])
varslt[] <- 0.1
index.var(idx[[1]]) <- varslt
# run
fit1 <- a4aSCA(stk, idx)
flqs <- FLQuants(nowgt=stock.n(fit), extwgt=stock.n(fit1))

## ---- likwgt, echo=FALSE, fig.cap="Stock summary of distinct likelihood weightings"----
xyplot(data~year|age, groups=qname, data=flqs, type="l",
       scales=list(y=list(relation="free", draw=FALSE)),
       auto.key=list(points=FALSE, lines=TRUE, columns=2),
       par.settings=list(superpose.line=list(col=c("gray35", "black")),
       strip.background=list(col="gray90")), ylab="")

## ------------------------------------------------------------------------
fmod <- ~ s(replace(age, age>5, 5), k=4) + s(year, k=20)
fit <- sca(ple4, ple4.indices, fmod)

## ---- rep, echo=FALSE, fig.cap="F-at-age fixed above age 5"--------------
wireframe(harvest(fit), zlab="F")

## ------------------------------------------------------------------------
fmod <- ~ I(1/(1+exp(-age)))
fit <- sca(ple4, ple4.indices, fmod)

## ---- logistic, echo=FALSE, fig.cap="F-at-age logistic"------------------
wireframe(harvest(fit), zlab="F")

## ------------------------------------------------------------------------
fmod <- ~s(age, k = 3, by = breakpts(year, 1990))
fit <- sca(ple4, ple4.indices, fmod)

## ---- brk, echo=FALSE, fig.cap="F-at-age in two periods using in both cases a thin plate spline with 3 knots"----
wireframe(harvest(fit), zlab="F")

## ------------------------------------------------------------------------
fmod <- ~ factor(age) + s(year, k=10, by = breakpts(age, c(2:8)))
fit <- sca(ple4, ple4.indices, fmod)

## ---- ageind, echo=FALSE, fig.cap="F-at-age as thin plate spline with 3 knots for each age"----
wireframe(harvest(fit), zlab="F")

## ------------------------------------------------------------------------
fmodel <- ~ s(age, k = 4) + s(pmax(year - age, 1957), k = 10) + s(year, k = 10)
fit <- sca(ple4, ple4.indices, fmodel=fmodel)

## ---- coh, echo=FALSE, fig.cap="F-at-age with a cohort effect."----------
wireframe(harvest(fit), zlab="F")

## ------------------------------------------------------------------------
data(ple4)
data(ple4.indices)
fit <- sca(ple4, ple4.indices)

## ------------------------------------------------------------------------
nits <- 25

shape <- FLModelSim(model=~exp(-age-0.5))
level <- FLModelSim(model=~k^0.66*t^0.57, params = FLPar(k=0.4, t=10),
                    vcov=matrix(c(0.002, 0.01,0.01, 1), ncol=2))
trend <- FLModelSim(model=~b, params=FLPar(b=0.5), vcov=matrix(0.02))

m4 <- a4aM(shape=shape, level=level, trend=trend)
m4 <- mvrnorm(nits, m4)
range(m4)[] <- range(ple4)[]
range(m4)[c("minmbar","maxmbar")]<-c(1,1)
flq <- m(m4)[]
quant(flq) <- "age"
stk <- propagate(ple4, nits)
m(stk) <- flq

## ------------------------------------------------------------------------
fit1 <- sca(stk, ple4.indices)

## ---- mprop, echo=FALSE, fig.cap="Stock summary for two M models", fig.width=8, fig.height=10----
plot(FLStocks("Jensen M with uncertainty"=propagate(ple4, 25)+fit1, "Jensen M"=ple4+fit), key=TRUE)

## ------------------------------------------------------------------------
# number of iters
nits <- 25
# fit the model
fit <- a4aSCA(ple4, ple4.indices[1])
# update the stock data
stk <- ple4 + fit
# simulate controlling the random seed
fits <- simulate(fit, nits, 1234)
# update stock and index data, now with iters
stks <- ple4 + fits
idxs <- ple4.indices[1]
index(idxs[[1]]) <- index(fits)[[1]]
# run assessments on each iter
sfit <- a4aSCA(stks, idxs, fit="MP")

## ---- wcsam, echo=FALSE, fig.cap="Replicating the stock assessment model (WCSAM approach)", fig.width=8, fig.height=10----
plot(FLStocks(original=stk, simulation=stks, "fit over simulation"=stks+sfit), key=TRUE)

## ------------------------------------------------------------------------
data(ple4)
data(ple4.indices)
nits <- 25
fit <- a4aSCA(ple4, ple4.indices[1])
stk <- ple4 + fit
fits <- simulate(fit, nits, 1234)
stks <- ple4 + fits
idxs <- ple4.indices[1]
index(idxs[[1]]) <- index(fits)[[1]]
library(parallel)
lst <- mclapply(split(1:nits, 1:nits), function(x){
	out <- try(a4aSCA(iter(stks, x), FLIndices(iter(idxs[[1]], x)), fit="MP"))
	if(is(out, "try-error")) NULL else out
})

stks2 <- stks
for(i in 1:nits){
	iter(catch.n(stks2), i) <- catch.n(lst[[i]])
	iter(stock.n(stks2), i) <- stock.n(lst[[i]])
	iter(harvest(stks2), i) <- harvest(lst[[i]])
}
catch(stks2) <- computeCatch(stks2)
stock(stks2) <- computeStock(stks2)
stks3 <- FLStocks(original=stk, simulation=stks, "fit over simulation"=stks2)

## ---- wcsampar, echo=FALSE, fig.cap="Replicating the stock assessment model (WCSAM approach) using parallel computing"----
plot(stks3, key=TRUE)

