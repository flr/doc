## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")


## ----echo=TRUE, eval=FALSE----------------------------------------------------
## install.packages(c("FLCore", "FLAssess", "FLXSA"), repos="http://flr-project.org/R")


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
# This chunk loads all necessary packages, trims pkg messages
library(FLCore)
library(FLAssess)
library(FLXSA)


## ----echo=FALSE, eval=TRUE, message = FALSE-----------------------------------
library(plyr)
library(ggplot2)

setMethod('plot', signature(x='FLXSA', y='missing'),function(x){
  
  ggplot(subset(x@diagnostics,age>0))+
    geom_point(aes(as.numeric(yrcls),nhat,size=0),shape=1,alpha=0)+
    geom_point(aes(as.numeric(yrcls),nhat,fill=factor(age),size=w),shape=21,
               data=subset(x@diagnostics,age>0&w>0))+
    geom_vline(aes(xintercept=range(x)["maxyear"]-x@control@shk.yrs-0-5))+
    scale_x_discrete(name="") +
    facet_grid(source~age)+
    theme_bw()+theme(legend.position="none")})


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
data(ple4)
data(ple4.indices)


## ----echo=TRUE, eval=FALSE----------------------------------------------------
## harvest(ple4)[] <- NA
## stock.n(ple4)[] <- NA


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
harvest(ple4)[ac(range(ple4)["max"]), ]     <- 1
harvest(ple4)[, ac(range(ple4)["maxyear"])] <- 1

ple4.vpa <- VPA(ple4, fratio = 1, fit.plusgroup = T)
ple4.new <- ple4 + ple4.vpa

## Have a look in stock number ##
stock.n(ple4.vpa)[, ac(2005:range(ple4)["maxyear"])]

## Have a look in fishing mortality ##
harvest(ple4.vpa)[, ac(2004:range(ple4)["maxyear"])]

## Plot results ##
plot(FLStocks(ple4=ple4, vpa=ple4.new))
plot(FLStocks(vpa=ple4.new))


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
# Define Laurec-Sheperd function #

lsm <- function(stock, index, fratio = 1, fit.plusgroup = T) {
  harvest(stock)[, ac(range(stock)["maxyear"])] <- 0.5
  diff <- 1
  while (diff > 1e-06) {
    stock <- stock + VPA(stock, fratio = fratio)
    ages  <- range(index)["min"]:range(index)["max"]
    yrs   <- range(index)["minyear"]:range(index)["maxyear"]
    stk   <- trim(stock, year = yrs, age = ages)
    Cp    <- catch.n(index)/catch.n(stk)
    q     <- sweep(Cp * harvest(stk), 2, effort(index), "/")
    gmq   <- apply(q, 1, function(x) exp(mean(log(x), na.rm = T)))
    mFp   <- gmq * c(apply(effort(index), 1, mean))
    Fr    <- mFp * (apply(Cp, 1, mean, na.rm = T))^-1
    Fnew  <- c(Fr, rep(Fr[ac(max(ages)), ], 1))
    diff  <- sum(abs(harvest(stock)[, ac(range(stock)["maxyear"])] -
                        Fnew))
    harvest(stock)[, ac(range(stock)["maxyear"])] <- c(Fnew)
    }
  res <- VPA(stock, fratio = fratio, fit.plusgroup = fit.plusgroup)
  index.res(res) <- FLQuants(q)
  return(res)
  }


## ----echo=TRUE, eval=TRUE-----------------------------------------------------

harvest(ple4)[] <- NA
stock.n(ple4)[] <- NA

ple4.LSvpa <- lsm(ple4, ple4.indices[[1]], fratio = 1, fit.plusgroup = T)

ple4.new2 <- ple4 + ple4.LSvpa

stock.n(ple4.LSvpa)[, ac(2005:range(ple4)["maxyear"])]
harvest(ple4.LSvpa)[, ac(2004:range(ple4)["maxyear"])]

# Compare the results with previous fits.
plot(FLStocks(vpa=ple4.new2))


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
  FLXSA.control()


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
  ctrl <- FLXSA.control(maxit = 50, qage = 8)
  ctrl <- FLXSA.control()
  slot(ctrl, 'qage')  <- as.integer(8)
  slot(ctrl, 'maxit') <- as.integer(50)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
 xsa.control <- FLXSA.control(maxit = 50, fse = 2.5)
 ple4.xsa    <- FLXSA(ple4, ple4.indices, xsa.control)
 ple4.xsa.t1 <- FLXSA(ple4, ple4.indices[[1]], xsa.control)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
ple4.new  <- ple4 + ple4.xsa
ple4.ssb  <- ssb(ple4.new)
ple4.rec  <- rec(ple4.new)
ple4.fbar <- fbar(ple4.new)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
 slot(slot(ple4.xsa, "control"), "maxit")


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
 ple4.xsa2 <- FLXSA(trim(ple4, age = 1:7), ple4.indices[[3]],
     xsa.control)
 diagnostics(ple4.xsa2, sections = c(T, T, rep(F, 6)))


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
 diagnostics(ple4.xsa2, sections = c(F, F, T, T, T, T, F, F))


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
 diagnostics(ple4.xsa2, sections = c(F, F, F, F, F, F, T, T))


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
 plot(ple4.xsa2)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
 names(ple4.xsa@index.res) <- names(ple4.indices)
 plot(xyplot(data ~ year | ac(age) + qname, data = index.res(ple4.xsa),
     panel = function(x, y, ...) {
     panel.xyplot(x, y, ...)
     panel.loess(x, y, ...)
     panel.abline(h = 0, col = "grey", lty = 2)
 }))


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
 diag <- slot(ple4.xsa, "diagnostics")[is.element(slot(ple4.xsa,
     "diagnostics")$year, 2008), ]
 diag <- cbind(diag, w.scaled = diag$w/rep(tapply(diag$w, diag$yrcls, sum),
     c(table(diag$yrcls))))
 nplot <- barchart(ac(yrcls) ~ nhat, groups = source, data = diag,
     col = grey(c(0.1, 0.6, 0.3, 0.8)), main = "N Estimates",
     ylab = "Year Class", key = list(x = 0.3, y = 0.25, text = list(legend =
rev(c("BTS-Isis",
         "BTS-Tridens", "fshk", "SNS"))), rectangles = list(col =
grey(rev(c(0.1,
         0.6, 0.3, 0.8))))))
 wplot <- barchart(ac(yrcls) ~ w.scaled, groups = source, data = diag,
     col = grey(c(0.1, 0.6, 0.3, 0.8)), main = "Scaled Weights",
     ylab = "", xlab = "Relative Weight")
 print(nplot, position = c(0, 0, 0.5, 1), more = TRUE)
 print(wplot, position = c(0.5, 0, 1, 1))


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
 fsevals <- seq(0.5, 2.5, by = 0.5)
 res <- propagate(harvest(ple4), length(fsevals))
 for (i in 1:length(fsevals)) {
     xsa.control <- FLXSA.control(fse = fsevals[i])
     iter(res, i) <- harvest(FLXSA(ple4, ple4.indices, xsa.control))
 }
 plot(xyplot(data ~ year | age, groups = iter, data = res, type = "l",
     col = "black", xlim = c(1990:2010)))


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
retro.years <- 2013:2017
ple4.retro <- tapply(retro.years, 1:length(retro.years), function(x){
	window(ple4,end=x)+FLXSA(window(ple4,end=x),ple4.indices)
})

# coerce into FLStocks object
ple4.retro <- FLStocks(ple4.retro)
# full retrospective summary plot
ple4.retro@names=ac(c(retro.years))###Add years to legend
plot(ple4.retro)

