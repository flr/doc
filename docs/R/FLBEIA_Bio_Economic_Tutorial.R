## ---- ini, echo=FALSE, results='hide', message=FALSE---------------------
# This chunk set the document environment, so it is hidden
library(knitr)
knitr::opts_chunk$set(fig.align="center",
                      message=FALSE, warning=FALSE, echo=TRUE, cache=FALSE)
options(width=50)
set.seed(1423)


## ----echo=FALSE, out.width='20%'-----------------------------------------
include_graphics('images/FLBEIA_logo.png')


## ---- eval=FALSE---------------------------------------------------------
## install.packages( c("ggplot2"))
## install.packages( c("FLCore", "FLFleet", "FLBEIA",
##                     "FLash", "FLAssess", "FLXSA"),
##                   repos="http://flr-project.org/R")


## ---- pkgs, results = "hide"---------------------------------------------
library(FLBEIA)
library(FLBEIAShiny)
library(ggplot2)


## ----echo=TRUE, eval=TRUE------------------------------------------------
dir <- tempdir()
# download.file("http://www.flr-project.org/doc/src/flbeia_bioeco.zip", 
#               file.path(dir, "flbeia_bioeco.zip"))
# unzip(file.path(dir, "flbeia_bioeco.zip"), exdir=dir)
unzip("src/flbeia_bioeco.zip", exdir=dir)


## ----echo=TRUE, eval=TRUE------------------------------------------------
load('./src/flbeia_bioeco/data.Rdata')
stknms <- names(biols)
main.ctrl$sim.years[] <- c(2017,2025)


## ----echo=TRUE, eval=TRUE------------------------------------------------
lapply(biols, function(x) dimnames(x@n)[[1]])


## ----echo=TRUE, eval=TRUE------------------------------------------------
lapply(fleets,function(x) names(x@metiers))


## ----echo=TRUE, eval=TRUE------------------------------------------------
t(stock.fleetInfo(fleets))


## ----echo=TRUE, eval=TRUE------------------------------------------------
n.flts.stks      <- sapply(sapply(fleets, catchNames), length) 
flts.stksnames   <- NULL
for(f in 1:length(fleets)){  
  nms <- c(names(flts.stksnames),
           paste(rep(names(fleets)[f], length(catchNames(fleets[[f]]))), 
                 catchNames(fleets[[f]]), sep = "_")) 
  flts.stksnames <- c(flts.stksnames, catchNames(fleets[[f]]))
  names(flts.stksnames) <- nms
  }




## ----echo=TRUE, eval=TRUE------------------------------------------------
catch.models <- rep("CobbDouglasAge", sum(n.flts.stks))
names(catch.models) <- flts.stksnames
catch.models[names(catch.models) == 'OTH'] <- "CobbDouglasBio"  


## ----echo=TRUE, eval=TRUE------------------------------------------------
price.models <-  rep("fixedPrice", sum(n.flts.stks))
names(price.models) <- flts.stksnames


## ----echo=TRUE, eval=TRUE------------------------------------------------
flq <- FLQuant(dimnames= list(year = 1980:2025, iter =  1))     


## ----echo=TRUE, eval=TRUE------------------------------------------------
capital.models <-  rep('fixedCapital', length(fleets))


## ----echo=TRUE, eval=TRUE------------------------------------------------
fleets.ctrl.CnEf <- create.fleets.ctrl(
                          fls = names(fleets),
                   n.fls.stks = n.flts.stks,
                fls.stksnames = flts.stksnames,
                effort.models = rep('SMFB', length(fleets)), 
                 price.models = price.models,
                 catch.models = catch.models,
                          flq = flq,
           restriction.NTR_SP = 'catch', restriction.PSX_SP = 'catch', 
           restriction.PGP_PT = 'catch', restriction.DTS_PT = 'catch', 
           restriction.DTS_SP = 'catch', restriction.PSX_PT = 'catch', 
           restriction.CAD_SP = 'catch')
for(f in names(fleets)) fleets.ctrl.CnEf[[f]][['effort.model']] <- 'fixedEffort'


## ----echo=TRUE, eval=TRUE------------------------------------------------
fleets.ctrl.trad <- create.fleets.ctrl(
                          fls = names(fleets), 
                   n.fls.stks = n.flts.stks, 
                fls.stksnames = flts.stksnames,
                effort.models = rep('SMFB', length(fleets)), 
                 price.models = price.models, 
                          flq = flq, 
                 catch.models = catch.models, 
               capital.models = capital.models,
           restriction.NTR_SP = 'catch', restriction.PSX_SP = 'catch', 
           restriction.PGP_PT = 'catch', restriction.DTS_PT = 'catch', 
           restriction.DTS_SP = 'catch', restriction.PSX_PT = 'catch', 
           restriction.CAD_SP = 'catch', 
           effort.restr.NTR_SP = 'HKE', effort.restr.PSX_SP = 'HOM', 
           effort.restr.PGP_PT = 'HKE', effort.restr.DTS_PT = 'HKE', 
           effort.restr.DTS_SP = 'HKE', effort.restr.PSX_PT = 'HOM', 
           effort.restr.CAD_SP = 'HKE') 


## ----echo=TRUE, eval=TRUE------------------------------------------------
effort.range <- lapply(fleets, function(x){ 
   res <- matrix(NA, length(x@metiers), 2, dimnames = list(names(x@metiers), c('min', 'max')))
   res[,1] <- 0#sapply(x@metiers, function(y) mean(y@effshare[, ac(2015:2017)]))*0.25
   res[,2] <- 1#sapply(x@metiers, function(y) mean(y@effshare[, ac(2015:2017)]))*1.75
   res[res>1] <- 1
   res
    })


## ----echo=TRUE, eval=TRUE------------------------------------------------
effort.models <- rep('SMFB', length(fleets))
names(effort.models) <- names(fleets)
effort.models[names(effort.models) %in% c('DTS_SP', 'NTR_SP')] <- 'MaxProfit'

fleets.ctrl.mxpr <- create.fleets.ctrl(
                          fls = names(fleets), 
                   n.fls.stks = n.flts.stks, 
                fls.stksnames = flts.stksnames,
                effort.models = effort.models, 
                 price.models = price.models, 
                          flq = flq, 
                 catch.models = catch.models, 
               capital.models = capital.models,
           restriction.NTR_SP = 'catch', restriction.PSX_SP = 'catch', 
           restriction.PGP_PT = 'catch', restriction.DTS_PT = 'catch', 
           restriction.DTS_SP = 'catch', restriction.PSX_PT = 'catch', 
           restriction.CAD_SP = 'catch', 
           effort.restr.NTR_SP = 'HKE', effort.restr.PSX_SP = 'HOM', 
           effort.restr.PGP_PT = 'HKE', effort.restr.DTS_PT = 'HKE', 
           effort.restr.DTS_SP = 'HKE', effort.restr.PSX_PT = 'HOM', 
           effort.restr.CAD_SP = 'HKE',
           effort.range.NTR_SP = effort.range[['NTR_SP']], 
           effort.range.DTS_SP = effort.range[['DTS_SP']])


## ----echo=TRUE, eval=TRUE------------------------------------------------
for(fl in  c('DTS_SP', 'NTR_SP')) fleets.ctrl.mxpr[[fl]][['efs.abs']] <- FALSE

fleets[['DTS_SP']]@capacity[] <- fleets[['NTR_SP']]@capacity[] <- 1e6


## ----echo=TRUE, eval=FALSE, results = 'hide'-----------------------------
## CnEf <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets,
##                   covars = covars, indices = NULL, advice = advice,
##                   main.ctrl = main.ctrl, biols.ctrl = biols.ctrl,
##                   fleets.ctrl = fleets.ctrl.CnEf,
##                   covars.ctrl = covars.ctrl, obs.ctrl = obs.ctrl,
##                   assess.ctrl = assess.ctrl, advice.ctrl = advice.ctrl.fixed)


## ----echo=TRUE, eval=FALSE, results = 'hide'-----------------------------
## trad <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets,
##                   covars = covars, indices = NULL, advice = advice,
##                   main.ctrl = main.ctrl, biols.ctrl = biols.ctrl,
##                   fleets.ctrl = fleets.ctrl.trad,
##                   covars.ctrl = covars.ctrl, obs.ctrl = obs.ctrl,
##                   assess.ctrl = assess.ctrl, advice.ctrl = advice.ctrl.fixed)


## ----echo=TRUE, eval=FALSE, results = 'hide'-----------------------------
## 
## mxpr <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets,
##                   covars = covars, indices = NULL, advice = advice,
##                   main.ctrl = main.ctrl, biols.ctrl = biols.ctrl,
##                   fleets.ctrl = fleets.ctrl.mxpr,
##                   covars.ctrl = covars.ctrl, obs.ctrl = obs.ctrl,
##                   assess.ctrl = assess.ctrl, advice.ctrl = advice.ctrl.fixed)


## ----echo=TRUE, eval=TRUE------------------------------------------------
price.models <- rep('fixedPrice', length(catch.models))
names(price.models) <- names(flts.stksnames)
price.models[substr(names(price.models),1,3) %in% c('NTR', 'DTS')] <- 'elasticPrice'

flq.HKE <- biols$HKE@n; flq.HKE[] <- NA
  
flq.LDB <- biols$LDB@n; flq.LDB[] <- NA

flq.HO8 <- biols$HO8@n; flq.HO8[] <- NA

flq.HOM <- biols$HOM@n; flq.HOM[] <- NA

flq.MEG <- biols$MEG@n; flq.MEG[] <- NA

flq.MON <- biols$MON@n; flq.MON[] <- NA

flq.MAC <- biols$MAC@n; flq.MAC[] <- NA

flq.WHB <- biols$WHB@n; flq.WHB[] <- NA

flq.OTH <- biols$OTH@n; flq.OTH[] <- NA




fleets.ctrl.mxpr.price <- create.fleets.ctrl(
                          fls = names(fleets), 
                   n.fls.stks = n.flts.stks, 
                fls.stksnames = flts.stksnames,
                effort.models = effort.models, 
                 price.models = price.models, 
                          flq = flq, 
                flq.HKE = flq.HKE,
                flq.HOM = flq.HOM,
                flq.HO8 = flq.HO8,
                flq.LDB = flq.LDB,
                flq.MEG = flq.MEG,
                flq.MON = flq.MON,
                flq.MAC = flq.MAC,
                flq.WHB = flq.WHB,
                flq.OTH = flq.OTH,
                 catch.models = catch.models, 
               capital.models = capital.models,
           restriction.NTR_SP = 'catch', restriction.PSX_SP = 'catch', 
           restriction.PGP_PT = 'catch', restriction.DTS_PT = 'catch', 
           restriction.DTS_SP = 'catch', restriction.PSX_PT = 'catch', 
           restriction.CAD_SP = 'catch', 
           effort.restr.NTR_SP = 'HKE', effort.restr.PSX_SP = 'HOM', 
           effort.restr.PGP_PT = 'HKE', effort.restr.DTS_PT = 'HKE', 
           effort.restr.DTS_SP = 'HKE', effort.restr.PSX_PT = 'HOM', 
           effort.restr.CAD_SP = 'HKE',
           effort.range.NTR_SP = effort.range[['NTR_SP']], 
           effort.range.DTS_SP = effort.range[['DTS_SP']])


for(fl in  c('DTS_SP', 'NTR_SP')) fleets.ctrl.mxpr.price[[fl]][['efs.abs']] <- FALSE



## ----echo=TRUE, eval=TRUE------------------------------------------------
plot(seq(0,5, 0.01), 4*(8/9)^(seq(0,5, 0.01)), type = 'l', ylim = c(2,7), lwd = 2,
      ylab = 'Euro/Kg', main = 'Price vs Elasticity', xlab = 'elasticity')
lines(seq(0,5, 0.01), 4*(10/9)^(seq(0,5, 0.01)), col  = 2, lwd = 2)
abline(h=4, lty = 2, col = 'grey')
legend(0,7,c('Ly > L0', 'Ly < L0', 'P0'), col = c('black', 'red', 'grey'), lwd = 2, 
       lty = c(1,1,2), bty = 'n' )


## ----echo=TRUE, eval=TRUE------------------------------------------------
fleets.ctrl.mxpr.price[['NTR_SP']][['HKE']][['pd.els']][] <- 
  fleets.ctrl.mxpr.price[['DTS_SP']][['HKE']][['pd.els']][] <- runif(prod(dim(flq.HKE)),0.1,0.4)

fleets.ctrl.mxpr.price[['NTR_SP']][['HOM']][['pd.els']][] <- 
  fleets.ctrl.mxpr.price[['DTS_SP']][['HOM']][['pd.els']][] <- 0.5

fleets.ctrl.mxpr.price[['DTS_SP']][['HO8']][['pd.els']][] <- 0.5

fleets.ctrl.mxpr.price[['DTS_SP']][['LDB']][['pd.els']][] <- 
  fleets.ctrl.mxpr.price[['DTS_SP']][['MEG']][['pd.els']][] <- 0.7

fleets.ctrl.mxpr.price[['NTR_SP']][['MON']][['pd.els']][] <- 
  fleets.ctrl.mxpr.price[['DTS_SP']][['MON']][['pd.els']][] <- 0.1

fleets.ctrl.mxpr.price[['DTS_SP']][['MAC']][['pd.els']][] <- 
  fleets.ctrl.mxpr.price[['DTS_SP']][['WHB']][['pd.els']][] <- 0.2

fleets.ctrl.mxpr.price[['NTR_SP']][['OTH']][['pd.els']][] <- runif(prod(dim(flq.OTH)),0,0.4)

fleets.ctrl.mxpr.price[['DTS_SP']][['OTH']][['pd.els']][] <- runif(prod(dim(flq.OTH)),0.2,0.4)


## ----echo=TRUE, eval=TRUE------------------------------------------------
fleets.ctrl.mxpr.price[['NTR_SP']][['HKE']][['pd.Pa0']][] <- 
  fleets.ctrl.mxpr.price[['DTS_SP']][['HKE']][['pd.Pa0']][] <- 
                    yearMeans(fleets[['DTS_SP']][[1]][['HKE']]@price)

fleets.ctrl.mxpr.price[['NTR_SP']][['HOM']][['pd.Pa0']][] <- 
  fleets.ctrl.mxpr.price[['DTS_SP']][['HOM']][['pd.Pa0']][] <-
                    yearMeans(fleets[['DTS_SP']][[1]][['HOM']]@price)

fleets.ctrl.mxpr.price[['DTS_SP']][['HO8']][['pd.Pa0']][]   <- 
                    yearMeans(fleets[['DTS_SP']][[2]][['HO8']]@price)

fleets.ctrl.mxpr.price[['DTS_SP']][['LDB']][['pd.Pa0']][]   <- 
                    yearMeans(fleets[['DTS_SP']][[1]][['LDB']]@price)

fleets.ctrl.mxpr.price[['DTS_SP']][['MEG']][['pd.Pa0']][]   <- 
                    yearMeans(fleets[['DTS_SP']][[1]][['MEG']]@price)

fleets.ctrl.mxpr.price[['NTR_SP']][['MON']][['pd.Pa0']][] <- 
  fleets.ctrl.mxpr.price[['DTS_SP']][['MON']][['pd.Pa0']][] <- 
                    yearMeans(fleets[['DTS_SP']][[1]][['MON']]@price)

fleets.ctrl.mxpr.price[['DTS_SP']][['MAC']][['pd.Pa0']][] <- 
                    yearMeans(fleets[['DTS_SP']][[2]][['MAC']]@price)

fleets.ctrl.mxpr.price[['DTS_SP']][['WHB']][['pd.Pa0']][] <- 
                    yearMeans(fleets[['DTS_SP']][[1]][['WHB']]@price)

fleets.ctrl.mxpr.price[['NTR_SP']][['OTH']][['pd.Pa0']][] <- 
                    yearMeans(fleets[['NTR_SP']][[2]][['OTH']]@price)


fleets.ctrl.mxpr.price[['DTS_SP']][['OTH']][['pd.Pa0']][] <- 
                    yearMeans(fleets[['DTS_SP']][[1]][['OTH']]@price)


## ----echo=TRUE, eval=TRUE------------------------------------------------
fleets.ctrl.mxpr.price[['NTR_SP']][['HKE']][['pd.La0']][] <- 
  fleets.ctrl.mxpr.price[['DTS_SP']][['HKE']][['pd.La0']][] <- yearMeans(landStock(fleets, 'HKE'))

fleets.ctrl.mxpr.price[['NTR_SP']][['HOM']][['pd.La0']][] <- 
  fleets.ctrl.mxpr.price[['DTS_SP']][['HOM']][['pd.La0']][] <- yearMeans(landStock(fleets, 'HOM'))

fleets.ctrl.mxpr.price[['DTS_SP']][['HO8']][['pd.La0']][] <- yearMeans(landStock(fleets, 'HO8'))

fleets.ctrl.mxpr.price[['DTS_SP']][['LDB']][['pd.La0']][] <- yearMeans(landStock(fleets, 'LDB'))

fleets.ctrl.mxpr.price[['DTS_SP']][['MEG']][['pd.La0']][] <- yearMeans(landStock(fleets, 'MEG'))

fleets.ctrl.mxpr.price[['NTR_SP']][['MON']][['pd.La0']][] <- 
  fleets.ctrl.mxpr.price[['DTS_SP']][['MON']][['pd.La0']][] <- yearMeans(landStock(fleets, 'MON'))

fleets.ctrl.mxpr.price[['DTS_SP']][['MAC']][['pd.La0']][] <- yearMeans(landStock(fleets, 'MAC'))

fleets.ctrl.mxpr.price[['DTS_SP']][['WHB']][['pd.La0']][] <- yearMeans(landStock(fleets, 'WHB'))

fleets.ctrl.mxpr.price[['NTR_SP']][['OTH']][['pd.La0']][] <- yearMeans(landStock(fleets, 'OTH'))

fleets.ctrl.mxpr.price[['DTS_SP']][['OTH']][['pd.La0']][] <- yearMeans(landStock(fleets, 'OTH'))


## ----echo=TRUE, eval=FALSE, results = 'hide'-----------------------------
## mxpr.price <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets,
##                   covars = covars, indices = NULL, advice = advice,
##                   main.ctrl = main.ctrl, biols.ctrl = biols.ctrl,
##                   fleets.ctrl = fleets.ctrl.mxpr.price,
##                   covars.ctrl = covars.ctrl, obs.ctrl = obs.ctrl,
##                   assess.ctrl = assess.ctrl, advice.ctrl = advice.ctrl.fixed)


## ----echo=TRUE, eval=TRUE, results = 'hide'------------------------------
capital.models      <- rep('fixedCapital', length(fleets))
names(capital.models) <- names(fleets)
capital.models[names(capital.models) %in% c('NTR_SP', 'DTS_SP')] <- 'SCD'

price.models        <- rep('fixedPrice', length(catch.models))
names(price.models) <- names(flts.stksnames)


fleets.ctrl.mxpr.capDyn <- create.fleets.ctrl(
                          fls = names(fleets), 
                   n.fls.stks = n.flts.stks, 
                fls.stksnames = flts.stksnames,
                effort.models = effort.models, 
                 price.models = price.models, 
                          flq = flq, 
                 catch.models = catch.models, 
               capital.models = capital.models,
           restriction.NTR_SP = 'catch', restriction.PSX_SP = 'catch', 
           restriction.PGP_PT = 'catch', restriction.DTS_PT = 'catch', 
           restriction.DTS_SP = 'catch', restriction.PSX_PT = 'catch', 
           restriction.CAD_SP = 'catch', 
           effort.restr.NTR_SP = 'HKE', effort.restr.PSX_SP = 'HOM', 
           effort.restr.PGP_PT = 'HKE', effort.restr.DTS_PT = 'HKE', 
           effort.restr.DTS_SP = 'HKE', effort.restr.PSX_PT = 'HOM', 
           effort.restr.CAD_SP = 'HKE',
           effort.range.NTR_SP = effort.range[['NTR_SP']], 
           effort.range.DTS_SP = effort.range[['DTS_SP']])

for(fl in  c('DTS_SP', 'NTR_SP')) fleets.ctrl.mxpr.capDyn[[fl]][['efs.abs']] <- FALSE


## ----echo=TRUE, eval=FALSE, results = 'hide'-----------------------------
## mxpr.capDyn <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets,
##                   covars = covars, indices = NULL, advice = advice,
##                   main.ctrl = main.ctrl, biols.ctrl = biols.ctrl,
##                   fleets.ctrl = fleets.ctrl.mxpr.capDyn,
##                   covars.ctrl = covars.ctrl, obs.ctrl = obs.ctrl,
##                   assess.ctrl = assess.ctrl, advice.ctrl = advice.ctrl.fixed)


## ----echo=TRUE, eval=FALSE, results = 'hide'-----------------------------
## 
## bio <- rbind(bioSumQ(bioSum(CnEf, scenario = 'CnEf', years = ac(2000:main.ctrl$sim.years[2]))),
##              bioSumQ(bioSum(trad, scenario = 'trad', years = ac(2000:main.ctrl$sim.years[2]))),
##              bioSumQ(bioSum(mxpr, scenario = 'MxPr', years = ac(2000:main.ctrl$sim.years[2]))),
##              bioSumQ(bioSum(mxpr.price, scenario = 'MxPr.Price',
##                             years = ac(2000:main.ctrl$sim.years[2]))),
##              bioSumQ(bioSum(mxpr.capDyn, scenario = 'MxPr.Capital',
##                             years = ac(2000:main.ctrl$sim.years[2]))))
## 
## flt <- rbind(fltSumQ(fltSum(CnEf, scenario = 'CnEf', flnms = c('DTS_SP', 'NTR_SP'),
##                             years = ac(2000:main.ctrl$sim.years[2]))),
##              fltSumQ(fltSum(trad, scenario = 'trad', flnms = c('DTS_SP', 'NTR_SP'),
##                             years = ac(2000:main.ctrl$sim.years[2]))),
##              fltSumQ(fltSum(mxpr, scenario = 'MxPr', flnms = c('DTS_SP', 'NTR_SP'),
##                             years = ac(2000:main.ctrl$sim.years[2]))),
##              fltSumQ(fltSum(mxpr.price, scenario = 'MxPr.Price',  flnms = c('DTS_SP', 'NTR_SP'),
##                             years = ac(2000:main.ctrl$sim.years[2]))),
##              fltSumQ(fltSum(mxpr.capDyn, scenario = 'MxPr.Capital', flnms = c('DTS_SP', 'NTR_SP'),
##                             years = ac(2000:main.ctrl$sim.years[2]))))
## 
## fltStk <- rbind(fltStkSumQ(fltStkSum(CnEf, scenario = 'CnEf', flnms = c('DTS_SP', 'NTR_SP'),
##                                      years = ac(2000:main.ctrl$sim.years[2]))),
##                 fltStkSumQ(fltStkSum(trad, scenario = 'trad', flnms = c('DTS_SP', 'NTR_SP'),
##                                      years = ac(2000:main.ctrl$sim.years[2]))),
##                 fltStkSumQ(fltStkSum(mxpr, scenario = 'MxPr', flnms = c('DTS_SP', 'NTR_SP'),
##                                      years = ac(2000:main.ctrl$sim.years[2]))),
##                 fltStkSumQ(fltStkSum(mxpr.price, scenario = 'MxPr.Price',
##                                      flnms = c('DTS_SP', 'NTR_SP'),
##                                      years = ac(2000:main.ctrl$sim.years[2]))),
##                 fltStkSumQ(fltStkSum(mxpr.capDyn, scenario = 'MxPr.Capital',
##                                      flnms = c('DTS_SP', 'NTR_SP'),
##                                      years = ac(2000:main.ctrl$sim.years[2]))))
## 
## mt <- rbind(mtSumQ(mtSum(CnEf, scenario = 'CnEf', flnms = c('DTS_SP', 'NTR_SP'),
##                          years = ac(2000:main.ctrl$sim.years[2]))),
##              mtSumQ(mtSum(trad, scenario = 'trad', flnms = c('DTS_SP', 'NTR_SP'),
##                           years = ac(2000:main.ctrl$sim.years[2]))),
##              mtSumQ(mtSum(mxpr, scenario = 'MxPr', flnms = c('DTS_SP', 'NTR_SP'),
##                           years = ac(2000:main.ctrl$sim.years[2]))),
##              mtSumQ(mtSum(mxpr.price, scenario = 'MxPr.Price', flnms = c('DTS_SP', 'NTR_SP'),
##                           years = ac(2000:main.ctrl$sim.years[2]))),
##              mtSumQ(mtSum(mxpr.capDyn, scenario = 'MxPr.Capital', flnms = c('DTS_SP', 'NTR_SP'),
##                           years = ac(2000:main.ctrl$sim.years[2]))))
## 
## mtStk <- rbind(mtStkSumQ(mtStkSum(CnEf, scenario = 'CnEf', flnms = c('DTS_SP', 'NTR_SP'),
##                                   years = ac(2000:main.ctrl$sim.years[2]))),
##                 mtStkSumQ(mtStkSum(trad, scenario = 'trad', flnms = c('DTS_SP', 'NTR_SP'),
##                                    years = ac(2000:main.ctrl$sim.years[2]))),
##                 mtStkSumQ(mtStkSum(mxpr, scenario = 'MxPr', flnms = c('DTS_SP', 'NTR_SP'),
##                                    years = ac(2000:main.ctrl$sim.years[2]))),
##                 mtStkSumQ(mtStkSum(mxpr.price, scenario = 'MxPr.Price',
##                                    flnms = c('DTS_SP', 'NTR_SP'),
##                                    years = ac(2000:main.ctrl$sim.years[2]))),
##                 mtStkSumQ(mtStkSum(mxpr.capDyn, scenario = 'MxPr.Capital',
##                                    flnms = c('DTS_SP', 'NTR_SP'),
##                                    years = ac(2000:main.ctrl$sim.years[2]))))
## 
## adv <- rbind(advSumQ(advSum(CnEf, scenario = 'CnEf', years = ac(2000:main.ctrl$sim.years[2]))),
##                 advSumQ(advSum(trad, scenario = 'trad', years = ac(2000:main.ctrl$sim.years[2]))),
##                 advSumQ(advSum(mxpr, scenario = 'MxPr', years = ac(2000:main.ctrl$sim.years[2]))),
##                 advSumQ(advSum(mxpr.price, scenario = 'MxPr.Price',
##                                years = ac(2000:main.ctrl$sim.years[2]))),
##                 advSumQ(advSum(mxpr.capDyn, scenario = 'MxPr.Capital',
##                                years = ac(2000:main.ctrl$sim.years[2]))))
## 
## flbeiaApp(bio = bio,
##             flt = flt,
##          fltStk = fltStk,
##              mt = mt,
##           mtStk = mtStk,
##             adv = adv,
##           years = ac(2000:main.ctrl$sim.years[2]),
##   calculate_npv = TRUE,
##          npv.y0 = '2016',
##         npv.yrs = ac(2017:main.ctrl$sim.years[2]))
## 
## 

