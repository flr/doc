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
harvest(ple4)[] <- NA
stock.n(ple4)[] <- NA
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
## Have a look in stock number ##
stock.n(ple4.vpa)[, ac(2002:range(ple4)["maxyear"])]
## Have a look in fishing mortality ##
harvest(ple4.vpa)[, ac(2002:range(ple4)["maxyear"])]
## Plot results ##
plot(FLStocks(ple4=ple4, vpa=ple4.new))
plot(FLStocks(vpa=ple4.new))

# Define Laurec-Sheperd function #

lsm <- function(stock, index, fratio = 1, fit.plusgroup = T) {
  harvest(stock)[, ac(range(stock)["maxyear"])] <- 0.5
  diff <- 1
  while (diff > 1e-06) {
    stock <- stock + VPA(stock, fratio = fratio)
    ages <- range(index)["min"]:range(index)["max"]
    yrs <- range(index)["minyear"]:range(index)["maxyear"]
    stk <- trim(stock, year = yrs, age = ages)
    Cp <- catch.n(index)/catch.n(stk)
    q <- sweep(Cp * harvest(stk), 2, effort(index), "/")
    gmq <- apply(q, 1, function(x) exp(mean(log(x), na.rm = T)))
    mFp <- gmq * c(apply(effort(index), 1, mean))
  Fr <- mFp * (apply(Cp, 1, mean, na.rm = T))^-1
    Fnew <- c(Fr, rep(Fr[ac(max(ages)), ], 2))
    diff <- sum(abs(harvest(stock)[, ac(range(stock)["maxyear"])] -
                        Fnew))
    harvest(stock)[, ac(range(stock)["maxyear"])] <- c(Fnew)
    }
  res <- VPA(stock, fratio = fratio, fit.plusgroup = fit.plusgroup)
  index.res(res) <- FLQuants(q)
  return(res)
  }


harvest(ple4)[] <- NA
stock.n(ple4)[] <- NA
ple4.LSvpa <- lsm(ple4, ple4.indices[[1]], fratio = 1, fit.plusgroup = T)
ple4.new2 <- ple4 + ple4.LSvpa
stock.n(ple4.LSvpa)[, ac(2002:range(ple4)["maxyear"])]
harvest(ple4.LSvpa)[, ac(2002:range(ple4)["maxyear"])]
#plot(FLStocks(ple4=ple4, vpa=ple4.new2))
plot(FLStocks(vpa=ple4.new2))

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
## Plot only one indices e.g. SNS
plot(ple4.indices[["SNS"]], type="ts")
#--------------------------------------------------------------------
# Select tuning fleets
#--------------------------------------------------------------------
## Using trim function you can select different "scenarios"
## indices [1] BTS-Isis trim on age2:8
## indices [2] BTS-TRidens
## indices [3] SNS trim on years 1997:2003

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
# FLXSA method takes three arguments: FLStock object (catch-at-age matrix), FLIndices (tuning indices), and an FLXSA.control object (parameter settings for XSA)

xsa.results <- FLXSA(ple4.sel, ple4.tun.sel, xsa.control)
ple4.xsa <- FLXSA(ple4.sel, ple4.tun.sel, xsa.control)
ple4.new <- ple4 + ple4.xsa
ple4.ssb <- ssb(ple4.new)
ple4.rec <- rec(ple4.new)
ple4.fbar <- fbar(ple4.new)
plot(ple4.new)
#--------------------------------------------------------------------
# now have a look at the XSA diagnostics
#--------------------------------------------------------------------
slot(slot(ple4.xsa, "control"), "maxit")

ple4.xsa2 <- FLXSA(trim(ple4, age = 1:4), ple4.indices[[3]],
                   xsa.control)
diagnostics(ple4.xsa2, sections = c(T, T, rep(F, 6)))

diagnostics(ple4.xsa2, sections = c(F, F, T, T, T, T, F, F))

diagnostics(ple4.xsa2, sections = c(F, F, F, F, F, F, T, T))

diagnostics(xsa.results)

# catchability Residuals by survey need to be extracted from the xsa.results 
# accessor is index.res, but there are no names so need to reassign them


names(ple4.xsa@index.res) <- names(ple4.indices)
pfun <- function(x, y, ...) {
  panel.xyplot(x, y, ...)
  panel.loess(x, y, ...)
  panel.abline(h = 0, col = "grey", lty = 2)
  }
plot(xyplot(data ~ year | ac(age) + qname, data = index.res(ple4.xsa),
              panel = pfun))

kk <- slot(ple4.xsa, "diagnostics")[is.element(slot(ple4.xsa,
                                                       "diagnostics")$year, 2008), ]
kk <- cbind(kk, w.scaled = kk$w/rep(tapply(kk$w, kk$yrcls, sum),
                                      c(table(kk$yrcls))))
nplot <-barchart(ac(yrcls) ~ nhat, groups = source, data = kk,
                    col = grey(c(0.1, 0.6, 0.3, 0.8)), main = "N Estimates",
                    ylab = "Year Class", key = list(x = 0.6, y = 0.2, text = list(legend =
                                                                                      rev(c("BTS-Isis",
"BTS-Tridens", "fshk", "SNS"))), rectangles = list(col =grey(rev(c(0.1,0.6, 0.3, 0.8))))))

wplot <- barchart(ac(yrcls) ~ w.scaled, groups = source, data = kk,
                    col = grey(c(0.1, 0.6, 0.3, 0.8)), main = "Scaled Weights",
                    ylab = "", xlab = "Relative Weight")
print(nplot, position = c(0, 0, 0.5, 1), more = TRUE)
print(wplot, position = c(0.5, 0, 1, 1))

fsevals <- seq(0.5, 2.5, by = 0.5)
res <- propagate(harvest(ple4), length(fsevals))
for (i in 1:length(fsevals)) {
  xsa.control <- FLXSA.control(fse = fsevals[i])
  iter(res, i) <- harvest(FLXSA(ple4, ple4.indices, xsa.control))
  } 
plot(xyplot(data ~ year | age, groups = iter, data = res, type = "l",
            col = "black", xlim = c(1990:2010)))
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
ple4.retro@names=ac(c(retro.years))###Add years to legend
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
# set.seed(1234)
# nits <- 25
# 
# # run xsa
# data(ple4.index)
# ple4.xsa <- FLXSA(ple4, ple4.index, FLXSA.control())
# 
# # sample with replacement by randomly selecting years
# # this way correlations between ages are preserved.
# 
# x <- dims(ple4.index)$minyear:dims(ple4.index)$maxyear
# size <- dims(ple4.index)$year*nits
# mc.yrs <-sample(x, size, TRUE)
# 
# # then create an FLQuant for the residuals with the right dimensions
# dmns <- dimnames(ple4.index@index)
# dmns$iter<-1:nits
# dev.index <- FLQuant(c(ple4.xsa@index.res[[1]][,ac(mc.yrs)]), dimnames=dmns)
# # NOTE THE USAGE OF THE RECYCLING RULE ON OUR BENEFIT
# plot(dev.index)
# 
# # bootstrap the index
# ple4.index@index <- ple4.xsa@index.hat[[1]]*exp(dev.index)
# 
# # rerun the assessment 100 times and put results in stock
# ple4.bxsa <- FLXSA(ple4, ple4.index, FLXSA.control(), diag.flag=F)
# 
# ple4.boot <- ple4 + ple4.bxsa
# 
# #plot bootstrap results wrt SSB timeseries
# plot(ple4.boot[,as.character(2000:2008)])
# 
# #--------------------------------------------------------------------
# # propagate into S/R
# #--------------------------------------------------------------------
# 
# ple4SR <- fmle(as.FLSR(ple4.boot, model="ricker"))
# params(ple4SR)
