## ----ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")


## ----eval=FALSE---------------------------------------------------------------
# install.packages(c("latticeExtra", "gridExtra", "ggplot2"))
# install.packages(c("FLCore", "ggplotFL", "FLa4a", "FLBRP", "FLasher"),
#   repos=c(FLR="https://flr.r-universe.dev", CRAN="https://cloud.r-project.org"))


## ----flcore-------------------------------------------------------------------
library(FLCore)
library(ggplotFL)


## ----flquant------------------------------------------------------------------
FLQuant(1:10)


## ----flquantage---------------------------------------------------------------
flq <- FLQuant(rlnorm(60), dimnames=list(age=1:4, year=2012:2017), units="t")

flq


## ----flquantinspect-----------------------------------------------------------
# A summary of structure and data
summary(flq)

# dimnames
dimnames(flq)

# dims
dim(flq)

# units
units(flq)


## ----flquantmodify------------------------------------------------------------
# Extract (by location) the first year
flq[, 1]

# in a R array, to get the same results, we'd need to do
flq[, 1,,,,]

# Extract (by name) year 2013
flq[, "2013"]

# Set catches on age 1 to zero
flq[1] <- 0
flq


## ----flquantarith-------------------------------------------------------------
# Product with scalar
flq * 10

# Addition with another FLQuant of the same dimensions
flq + (flq * 0.20)

# Sum along years
yearSums(flq)


## -----------------------------------------------------------------------------
data(ple4)
head(as.data.frame(ple4))
tail(as.data.frame(ple4))


## ----getfiles, message=FALSE--------------------------------------------------
dir <- tempdir()
download.file("http://www.flrproject.org/doc/src/ple4.csv.zip", file.path(dir, "ple4.csv.zip"))
unzip(file.path(dir, "ple4.csv.zip"), exdir=dir)


## ----loadple4-----------------------------------------------------------------
dat <- read.csv(file.path(dir, "ple4.csv"))
head(dat)


## ----subsetlandingsn----------------------------------------------------------
landn <- subset(dat, slot=="landings.n", select=-slot)


## ----convertlandingsn---------------------------------------------------------
landsn <- as.FLQuant(landn)


## ----plotlandings.n-----------------------------------------------------------
summary(landsn)

plot(landsn)


## ----convertple4--------------------------------------------------------------
nple4 <- as.FLStock(dat)

summary(nple4)


## ----nple4m-------------------------------------------------------------------
m(nple4) <- 0.1


## ----nple4spwn----------------------------------------------------------------
m.spwn(nple4) <- harvest.spwn(nple4) <- 0


## ----nple4mat-----------------------------------------------------------------
mat(nple4) <- c(0, 0.5, 0.5, rep(1, 7))


## ----nple4compute-------------------------------------------------------------
landings(nple4) <- computeLandings(nple4)
discards(nple4) <- computeDiscards(nple4)


## ----nple4catch---------------------------------------------------------------
catch(nple4) <- computeCatch(nple4, slot="all")


## ----nple4stockwt-------------------------------------------------------------
stock.wt(nple4) <- catch.wt(nple4)


## ----nple4range---------------------------------------------------------------
range(nple4, c("minfbar", "maxfbar")) <- c(2, 6)


## ----nple4--------------------------------------------------------------------
summary(nple4)

#plot(metrics(nple4, Catch=catch, Landings=landings))

plot(nple4)

data(ple4)


## ----xyplot-------------------------------------------------------------------
xyplot(data~year|age, harvest(ple4), xlab="", ylab="", type="b", cex=0.5, pch=19)


## ----plotstockn---------------------------------------------------------------
plot(stock.n(ple4))


## ----plotstocknplus-----------------------------------------------------------
plot(stock.n(ple4)) +
  # Add y label
  ylab("Biomass (t)") +
  # Draw rectangle between years 1990 and 2000
  annotate("rect", xmin = 1990, xmax = 2000, ymin = 0, ymax = Inf,
    # in semi-transparent red
    alpha = .2, fill='red')


## ----plotcatchn---------------------------------------------------------------
ggplot(data=catch.n(ple4), aes(x=year, y=data, group=age)) +
  geom_line(aes(colour=as.factor(age))) +
  ylab("Total catch (t)") + xlab("") + theme(legend.position="none")


## ----plotcatch----------------------------------------------------------------
plot(rlnorm(250, log(catch(ple4)), 0.5))


## ----loadple4index------------------------------------------------------------
data(ple4.index)


## ----summaryple4index---------------------------------------------------------
summary(ple4.index)

plot(ple4.index)


## ----ple4indexrange-----------------------------------------------------------
range(ple4.index)[c("startf", "endf")]


## ----fla4apkg-----------------------------------------------------------------
library(FLa4a)


## ----sca----------------------------------------------------------------------
fit <- sca(ple4, ple4.index)


## ----summarya4afit------------------------------------------------------------
summary(fit)


## ----ple4fit------------------------------------------------------------------
stk <- ple4 + fit
plot(stk)


## ----flsrcreate---------------------------------------------------------------
plsr <- as.FLSR(stk)


## ----flsrsummary--------------------------------------------------------------
summary(plsr)


## ----flsrmodel----------------------------------------------------------------
model(plsr) <- ricker()


## ----flsrfit, results="hide"--------------------------------------------------
plsr <- fmle(plsr)


## ----flsrprofile--------------------------------------------------------------
profile(plsr)


## ----flsrpredict--------------------------------------------------------------
predict(plsr, ssb=FLQuant(rnorm(10, 25e4, sd(ssb(plsr))), dimnames=list(age=1, year=2008:2017)))


## ----flrbp, warnings=FALSE----------------------------------------------------
library(FLBRP)
plrp <- FLBRP(stk, sr=plsr)
summary(plrp)


## ----brp----------------------------------------------------------------------
plrp <- brp(plrp)


## ----refpts-------------------------------------------------------------------
refpts(plrp)


## ----refptsextract------------------------------------------------------------
pmsy <- refpts(plrp)["msy", c("harvest", "ssb"), drop=TRUE]


## ----refptplot----------------------------------------------------------------
plot(ssb(stk) / pmsy["ssb"]) + geom_hline(aes(yintercept=1), linetype=2) +
  ylab(expression(SSB / SSB[MSY]))


## ----flash--------------------------------------------------------------------
library(FLasher)


## ----stf----------------------------------------------------------------------
proj <- fwdWindow(stk, end=2020)


## ----stfstockwt---------------------------------------------------------------
stock.wt(proj)[, ac(2016:2020)]


## ----fwdcontrol---------------------------------------------------------------
TAC <- 85000
Flevel <- fbar(stk)[,"2017"]
ctrl <- fwdControl(year=2018:2020, quant=c("catch", "f", "f"), value=c(TAC, Flevel, Flevel))


## ----fwd----------------------------------------------------------------------
proj <- fwd(proj, control=ctrl, sr=plsr) 


## ----fwdplot------------------------------------------------------------------
plot(proj) + geom_vline(aes(xintercept=2017.5), linetype=2)

