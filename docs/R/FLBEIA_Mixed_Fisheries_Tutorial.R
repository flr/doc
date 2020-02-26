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
library(ggplot2)


## ----echo=TRUE, eval=TRUE------------------------------------------------
tdir <- tempdir()
# download.file("http://www.flr-project.org/doc/src/flbeia_mixed_fisheries.zip", 
#               file.path(tdir, "flbeia_mixed_fisheries.zip"))
# unzip(file.path(tdir, "flbeia_mixed_fisheries.zip"), exdir=tdir)
unzip("src/flbeia_mixed_fisheries.zip", exdir=tdir)
tdir <- file.path(tdir,"flbeia_mixed_fisheries")


## ----echo=TRUE, eval=TRUE------------------------------------------------
load(file.path(tdir,'Smart_Cond_II.Rdata'))
stknms <- names(biols)


## ----echo=TRUE, eval=TRUE------------------------------------------------
flq1 <- FLQuant(1,dim = c(1,26), dimnames = list(quant = 'all', year = 2000:2025))


## ----echo=TRUE, eval=TRUE------------------------------------------------
hke.sr <- FLSRsim(rec = biols[['HKE']]@n[1,], ssb = ssb(biols[['HKE']]), uncertainty = flq1, 
                  proportion = flq1, model = 'segreg',name = 'HKE')


## ----echo=TRUE, eval=TRUE------------------------------------------------
hke.sr <- FLSRsim(rec = biols[['HKE']]@n[1,], ssb = ssb(biols[['HKE']]), uncertainty = flq1, 
                  proportion = flq1, model = 'geomean',name = 'HKE')
hke.sr@params[] <- 92476


## ----echo=TRUE, eval=TRUE------------------------------------------------
ldb.sr <- FLSRsim(rec = biols[['LDB']]@n[1,], ssb = ssb(biols[['LDB']]), uncertainty = flq1, 
                  proportion = flq1, model = 'geomean',name = 'LDB')
ldb.sr@params[] <- 53614
  
meg.sr <- FLSRsim(rec = biols[['MEG']]@n[1,], ssb = ssb(biols[['MEG']]), uncertainty = flq1, 
                  proportion = flq1, model = 'geomean',name = 'MEG')
meg.sr@params[] <- 3559

mon.sr <- FLSRsim(rec = biols[['MON']]@n[1,], ssb = ssb(biols[['MON']]), uncertainty = flq1, 
                  proportion = flq1, model = 'geomean',name = 'MON')
mon.sr@params[] <- 760


## ----echo=TRUE, eval=TRUE------------------------------------------------
SRs <- list(HKE = hke.sr, LDB = ldb.sr, MEG = meg.sr, MON = mon.sr)


## ----echo=TRUE, eval=TRUE------------------------------------------------
advice <-  list(TAC = FLQuant(NA, dimnames= list(stocks = stknms, year = 2000:2025)),
                quota.share = lapply(stknms, function(x){
                        FLQuant(NA, 
                            dimnames = list(fleets = names(fleets), year = 2000:2025))}))


## ----echo=TRUE, eval=TRUE------------------------------------------------
advice$TAC['HKE', ac(2018)] <- 9258
advice$TAC['LDB', ac(2018)] <- 1151
advice$TAC['MEG', ac(2018)] <- 236
advice$TAC['MON', ac(2018)] <- 1898


advice$TAC['HKE', ac(2019:2025)] <- 8281
advice$TAC['LDB', ac(2019:2025)] <- 1633 
advice$TAC['MEG', ac(2019:2025)] <- 399
advice$TAC['MON', ac(2019:2025)] <- 2153


## ----echo=TRUE, eval=TRUE------------------------------------------------
names(advice$quota.share) <- stknms

for(st in stknms){
  for(fl in names(fleets)){
    if(st %in% catchNames(fleets[[fl]])){ 
      advice$quota.share[[st]][fl,] <- quantSums(catchWStock.f(fleets[[fl]], st))/
                                       quantSums(catchWStock(fleets, st))
      advice$quota.share[[st]][fl,ac(2018:2025)] <- 
                              yearMeans(advice$quota.share[[st]][fl,ac(2015:2017)])
    }
    else advice$quota.share[[st]][fl,] <- 0
}}


## ----echo=TRUE, eval=TRUE------------------------------------------------
main.ctrl                  <- list()
main.ctrl$sim.years        <- c(initial = 2018, final = 2025)
main.ctrl$SimultaneousMngt <- FALSE


## ----echo=TRUE, eval=TRUE------------------------------------------------
growth.model     <- c(rep('ASPG',4))
biols.ctrl       <- create.biols.ctrl (stksnames=stknms,growth.model= growth.model)


## ----echo=TRUE, eval=TRUE------------------------------------------------
n.flts.stks      <- sapply(sapply(fleets, catchNames), length) 
flts.stksnames   <- NULL
for(f in 1:length(fleets))  flts.stksnames <- c(flts.stksnames, catchNames(fleets[[f]])) 

catch.models <- rep("CobbDouglasAge", sum(n.flts.stks))
names(catch.models) <- flts.stksnames

capital.models <-  rep('fixedCapital', length(fleets))

flq <- FLQuant(dimnames= list(year = 2000:2025, iter =  1))     


## ----echo=TRUE, eval=TRUE------------------------------------------------
fleets.ctrl.Esq <- create.fleets.ctrl(
                          fls = names(fleets),
                   n.fls.stks = n.flts.stks,
                fls.stksnames = flts.stksnames,
                effort.models = rep('SMFB', length(fleets)), 
                 price.models = rep("fixedPrice", sum(n.flts.stks)),
                 catch.models = catch.models,
                          flq = flq,
           restriction.SP_GNS = 'catch', restriction.PT_GNS = 'catch', 
           restriction.PT_GTR = 'catch', restriction.SP_GTR = 'catch', 
           restriction.SP_LLS = 'catch', restriction.PT_MIS = 'catch', 
           restriction.SP_MIS = 'catch', restriction.PT_OTB = 'catch', 
           restriction.SP_OTB = 'catch', restriction.SP_OTB_24m = 'catch', 
           restriction.OTH_OTH = 'catch',restriction.SP_PTB = 'catch')

for(fl in names(fleets)) fleets.ctrl.Esq[[fl]][['effort.model']] <- 'fixedEffort'


## ----echo=TRUE, eval=TRUE------------------------------------------------
fleets.ctrl.max <- create.fleets.ctrl(
                          fls = names(fleets), 
                   n.fls.stks = n.flts.stks, 
                fls.stksnames = flts.stksnames,
                effort.models = rep('SMFB', length(fleets)), 
                 price.models = rep("fixedPrice", sum(n.flts.stks)), 
                          flq = flq, 
                 catch.models = catch.models, 
               capital.models = capital.models,
           restriction.SP_GNS = 'catch', restriction.PT_GNS = 'catch', 
           restriction.PT_GTR = 'catch', restriction.SP_GTR = 'catch', 
           restriction.SP_LLS = 'catch', restriction.PT_MIS = 'catch', 
           restriction.SP_MIS = 'catch', restriction.PT_OTB = 'catch', 
           restriction.SP_OTB = 'catch', restriction.SP_OTB_24m = 'catch', 
           restriction.OTH_OTH = 'catch', restriction.SP_PTB = 'catch', 
           effort.restr.SP_GNS = 'max', effort.restr.PT_GNS = 'max', 
           effort.restr.PT_GTR = 'max', effort.restr.SP_GTR = 'max', 
           effort.restr.SP_LLS = 'max', effort.restr.PT_MIS = 'max', 
           effort.restr.SP_MIS = 'max', effort.restr.PT_OTB = 'max', 
           effort.restr.SP_OTB = 'max', effort.restr.SP_OTB_24m = 'max', 
           effort.restr.OTH_OTH = 'max', effort.restr.SP_PTB = 'max') 


## ----echo=TRUE, eval=TRUE------------------------------------------------
fleets.ctrl.min <- create.fleets.ctrl(
                          fls = names(fleets), 
                   n.fls.stks = n.flts.stks, 
                fls.stksnames = flts.stksnames,
                effort.models = rep('SMFB', length(fleets)), 
                 price.models = rep("fixedPrice", sum(n.flts.stks)), 
                          flq = flq, 
                 catch.models = catch.models, 
               capital.models = capital.models,
           restriction.SP_GNS = 'catch', restriction.PT_GNS = 'catch', 
           restriction.PT_GTR = 'catch', restriction.SP_GTR = 'catch', 
           restriction.SP_LLS = 'catch', restriction.PT_MIS = 'catch', 
           restriction.SP_MIS = 'catch', restriction.PT_OTB = 'catch', 
           restriction.SP_OTB = 'catch', restriction.SP_OTB_24m = 'catch', 
           restriction.OTH_OTH = 'catch', restriction.SP_PTB = 'catch', 
           effort.restr.SP_GNS = 'min', effort.restr.PT_GNS = 'min', 
           effort.restr.PT_GTR = 'min', effort.restr.SP_GTR = 'min', 
           effort.restr.SP_LLS = 'min', effort.restr.PT_MIS = 'min', 
           effort.restr.SP_MIS = 'min', effort.restr.PT_OTB = 'min', 
           effort.restr.SP_OTB = 'min', effort.restr.SP_OTB_24m = 'min', 
           effort.restr.OTH_OTH = 'min', effort.restr.SP_PTB = 'min') 


## ----echo=TRUE, eval=TRUE------------------------------------------------
fleets.ctrl.HKE <- create.fleets.ctrl(
                          fls = names(fleets), 
                   n.fls.stks = n.flts.stks, 
                fls.stksnames = flts.stksnames,
                effort.models = rep('SMFB', length(fleets)), 
                 price.models = rep("fixedPrice", sum(n.flts.stks)), 
                          flq = flq, 
                 catch.models = catch.models, 
               capital.models = capital.models,
           restriction.SP_GNS = 'catch', restriction.PT_GNS = 'catch', 
           restriction.PT_GTR = 'catch', restriction.SP_GTR = 'catch', 
           restriction.SP_LLS = 'catch', restriction.PT_MIS = 'catch', 
           restriction.SP_MIS = 'catch', restriction.PT_OTB = 'catch', 
           restriction.SP_OTB = 'catch', restriction.SP_OTB_24m = 'catch', 
           restriction.OTH_OTH = 'catch', restriction.SP_PTB = 'catch', 
           effort.restr.SP_GNS = 'HKE', effort.restr.PT_GNS = 'HKE', 
           effort.restr.PT_GTR = 'HKE', effort.restr.SP_GTR = 'HKE', 
           effort.restr.SP_LLS = 'HKE', effort.restr.PT_MIS = 'HKE', 
           effort.restr.SP_MIS = 'HKE', effort.restr.PT_OTB = 'HKE', 
           effort.restr.SP_OTB = 'HKE', effort.restr.SP_OTB_24m = 'HKE', 
           effort.restr.OTH_OTH = 'HKE', effort.restr.SP_PTB = 'HKE') 


## ----echo=TRUE, eval=TRUE------------------------------------------------
fleets.ctrl.LDB <- create.fleets.ctrl(
                          fls = names(fleets), 
                   n.fls.stks = n.flts.stks, 
                fls.stksnames = flts.stksnames,
                effort.models = rep('SMFB', length(fleets)), 
                 price.models = rep("fixedPrice", sum(n.flts.stks)), 
                          flq = flq, 
                 catch.models = catch.models, 
               capital.models = capital.models,
           restriction.SP_GNS = 'catch', restriction.PT_GNS = 'catch', 
           restriction.PT_GTR = 'catch', restriction.SP_GTR = 'catch', 
           restriction.SP_LLS = 'catch', restriction.PT_MIS = 'catch', 
           restriction.SP_MIS = 'catch', restriction.PT_OTB = 'catch', 
           restriction.SP_OTB = 'catch', restriction.SP_OTB_24m = 'catch', 
           restriction.OTH_OTH = 'catch', restriction.SP_PTB = 'catch', 
           effort.restr.SP_GNS = 'LDB', effort.restr.PT_GNS = 'LDB', 
           effort.restr.PT_GTR = 'LDB', effort.restr.SP_GTR = 'LDB', 
           effort.restr.SP_LLS = 'LDB', effort.restr.PT_MIS = 'LDB', 
           effort.restr.SP_MIS = 'LDB', effort.restr.PT_OTB = 'LDB', 
           effort.restr.SP_OTB = 'LDB', effort.restr.SP_OTB_24m = 'LDB', 
           effort.restr.OTH_OTH = 'LDB', effort.restr.SP_PTB = 'LDB') 


## ----echo=TRUE, eval=TRUE------------------------------------------------
fleets.ctrl.MON <- create.fleets.ctrl(
                          fls = names(fleets), 
                   n.fls.stks = n.flts.stks, 
                fls.stksnames = flts.stksnames,
                effort.models = rep('SMFB', length(fleets)), 
                 price.models = rep("fixedPrice", sum(n.flts.stks)), 
                          flq = flq, 
                 catch.models = catch.models, 
               capital.models = capital.models,
           restriction.SP_GNS = 'catch', restriction.PT_GNS = 'catch', 
           restriction.PT_GTR = 'catch', restriction.SP_GTR = 'catch', 
           restriction.SP_LLS = 'catch', restriction.PT_MIS = 'catch', 
           restriction.SP_MIS = 'catch', restriction.PT_OTB = 'catch', 
           restriction.SP_OTB = 'catch', restriction.SP_OTB_24m = 'catch', 
           restriction.OTH_OTH = 'catch', restriction.SP_PTB = 'catch', 
           effort.restr.SP_GNS = 'MON', effort.restr.PT_GNS = 'MON', 
           effort.restr.PT_GTR = 'MON', effort.restr.SP_GTR = 'MON', 
           effort.restr.SP_LLS = 'MON', effort.restr.PT_MIS = 'MON', 
           effort.restr.SP_MIS = 'MON', effort.restr.PT_OTB = 'MON', 
           effort.restr.SP_OTB = 'MON', effort.restr.SP_OTB_24m = 'MON', 
           effort.restr.OTH_OTH = 'MON', effort.restr.SP_PTB = 'MON') 


## ----echo=TRUE, eval=TRUE------------------------------------------------
fleets.ctrl.MEG <- create.fleets.ctrl(
                          fls = names(fleets), 
                   n.fls.stks = n.flts.stks, 
                fls.stksnames = flts.stksnames,
                effort.models = rep('SMFB', length(fleets)), 
                 price.models = rep("fixedPrice", sum(n.flts.stks)), 
                          flq = flq, 
                 catch.models = catch.models, 
               capital.models = capital.models,
           restriction.SP_GNS = 'catch', restriction.PT_GNS = 'catch', 
           restriction.PT_GTR = 'catch', restriction.SP_GTR = 'catch', 
           restriction.SP_LLS = 'catch', restriction.PT_MIS = 'catch', 
           restriction.SP_MIS = 'catch', restriction.PT_OTB = 'catch', 
           restriction.SP_OTB = 'catch', restriction.SP_OTB_24m = 'catch', 
           restriction.OTH_OTH = 'catch', restriction.SP_PTB = 'catch', 
           effort.restr.SP_GNS = 'MEG', effort.restr.PT_GNS = 'MEG', 
           effort.restr.PT_GTR = 'MEG', effort.restr.SP_GTR = 'MEG', 
           effort.restr.SP_LLS = 'MEG', effort.restr.PT_MIS = 'MEG', 
           effort.restr.SP_MIS = 'MEG', effort.restr.PT_OTB = 'MEG', 
           effort.restr.SP_OTB = 'MEG', effort.restr.SP_OTB_24m = 'MEG', 
           effort.restr.OTH_OTH = 'MEG', effort.restr.SP_PTB = 'MEG') 


## ----echo=TRUE, eval=TRUE------------------------------------------------
covars.ctrl <- NULL


## ----echo=TRUE, eval=TRUE------------------------------------------------
obs.ctrl         <- create.obs.ctrl(stksnames = stknms,  
                                    stkObs.models = rep('NoObsStock',length(stknms)))


## ----echo=TRUE, eval=TRUE------------------------------------------------
assess.ctrl      <- create.assess.ctrl(stksnames = stknms, 
                                       assess.models = rep('NoAssessment',length(stknms)))


## ----echo=TRUE, eval=TRUE------------------------------------------------
advice.ctrl   <- create.advice.ctrl(stksnames = stknms, 
                                    HCR.models = rep('fixedAdvice', length(stknms)))


## ----echo=TRUE, eval=TRUE------------------------------------------------
main.ctrl[[1]][2] <- 2020


## ----echo=TRUE, eval=TRUE------------------------------------------------
advice.hke <- advice
advice.hke$TAC['HKE', '2018'] <- 16709
advice.hke$TAC['HKE', '2019'] <- 8281 


## ----echo=TRUE, eval=TRUE, results = 'hide'------------------------------
for(fl in names(fleets)) fleets[[fl]]@capacity[] <- 1e12

hke <- FLBEIA(biols = biols, 
                SRs = SRs, 
                BDs = NULL, 
             fleets = fleets,
            indices = NULL, 
             advice = advice.hke, 
          main.ctrl = main.ctrl, 
         biols.ctrl = biols.ctrl, 
        fleets.ctrl = fleets.ctrl.HKE, 
        covars.ctrl = NULL, 
           obs.ctrl = obs.ctrl, 
        assess.ctrl = assess.ctrl,
        advice.ctrl = advice.ctrl) 


## ----echo=TRUE, eval=TRUE------------------------------------------------
hke.sum    <- bioSum(hke, years = ac(2000:2020), long = TRUE)


## ----echo=TRUE, eval=TRUE------------------------------------------------
ssb1719_wg_hke <- c(23885, 23904, 36104) 
f1718_wg_hke <- c(0.6,0.25)


## ----echo=TRUE, eval=TRUE------------------------------------------------
round(subset(hke.sum, year %in% 2018:2020 & indicator %in% c('ssb') & stock == 'HKE')$value/
        ssb1719_wg_hke,2)
round(subset(hke.sum, year %in% 2018:2019 & indicator %in% c('f') & stock == 'HKE')$value/
        f1718_wg_hke,2)


## ----echo=TRUE, eval=TRUE, results = 'hide'------------------------------
advice.ldb <- advice
advice.ldb$TAC['LDB', '2018'] <- 2159  # catch corresponding to Fsq
advice.ldb$TAC['LDB', '2019'] <- 1633  # TAC in 2019


ldb <- FLBEIA(biols = biols, 
                SRs = SRs, 
                BDs = NULL, 
             fleets = fleets, 
            indices = NULL, 
             advice = advice.ldb, 
          main.ctrl = main.ctrl, 
         biols.ctrl = biols.ctrl, 
        fleets.ctrl = fleets.ctrl.LDB, 
        covars.ctrl = NULL, 
           obs.ctrl = obs.ctrl, 
        assess.ctrl = assess.ctrl, 
        advice.ctrl = advice.ctrl) 

ldb.sum    <- bioSum(ldb, years = ac(2000:2020), long = TRUE)


## ----echo=TRUE, eval=TRUE------------------------------------------------
ssb1719_wg_ldb <- c(8821, 8836, 9286) 
f1718_wg_ldb <- c(0.28,0.193)

round(subset(ldb.sum, year %in% 2018:2020 & indicator %in% c('ssb') & stock == 'LDB')$value/
        ssb1719_wg_ldb,2)
round(subset(ldb.sum, year %in% 2018:2019 & indicator %in% c('f') & stock == 'LDB')$value/
        f1718_wg_ldb,2)


## ----echo=TRUE, eval=TRUE, results = 'hide'------------------------------
advice.meg <- advice
advice.meg$TAC['MEG', '2018'] <- 642  # catch corresponding to Fsq
advice.meg$TAC['MEG', '2019'] <- 399  # TAC in 2019


meg <- FLBEIA(biols = biols, 
                SRs = SRs, 
                BDs = NULL, 
             fleets = fleets, 
            indices = NULL, 
             advice = advice.meg, 
          main.ctrl = main.ctrl, 
         biols.ctrl = biols.ctrl,
        fleets.ctrl = fleets.ctrl.MEG, 
        covars.ctrl = NULL,
           obs.ctrl = obs.ctrl, 
        assess.ctrl = assess.ctrl, 
        advice.ctrl = advice.ctrl) 

meg.sum    <- bioSum(meg, years = ac(2000:2020), long = TRUE)

ssb1719_wg_meg <- c(2504, 2241, 2129) 
f1718_wg_meg   <- c(0.29,0.191)


## ----echo=TRUE, eval=TRUE------------------------------------------------
round(subset(meg.sum, year %in% 2018:2020 & indicator %in% c('ssb') & stock == 'MEG')$value/
        ssb1719_wg_meg,2)
round(subset(meg.sum, year %in% 2018:2019 & indicator %in% c('f') & stock == 'MEG')$value/
        f1718_wg_meg,2)


## ----echo=TRUE, eval=TRUE, results = 'hide'------------------------------
advice.mon <- advice
advice.mon$TAC['MON', '2018'] <- 1556 # catch corresponding to Fsq
advice.mon$TAC['MON', '2019'] <- 2153 # TAC 2019


mon <- FLBEIA(biols = biols, 
                SRs = SRs, 
                BDs = NULL, 
             fleets = fleets, 
            indices = NULL, 
             advice = advice.mon, 
          main.ctrl = main.ctrl, 
         biols.ctrl = biols.ctrl, 
        fleets.ctrl = fleets.ctrl.MON, 
        covars.ctrl = NULL, 
           obs.ctrl = obs.ctrl, 
        assess.ctrl = assess.ctrl, 
        advice.ctrl = advice.ctrl) 


mon.sum    <- bioSum(mon, years = ac(2000:2020), long =TRUE)


ssb1719_wg_mon <- c(11839, 11552, 10071) 
f1718_wg_mon   <- c(0.143,0.24)


## ----echo=TRUE, eval=TRUE------------------------------------------------
round(subset(mon.sum, year %in% 2018:2020 & indicator %in% c('ssb') & stock == 'MON')$value/
        ssb1719_wg_mon,2)
round(subset(mon.sum, year %in% 2018:2019 & indicator %in% c('f') & stock == 'MON')$value/
        f1718_wg_mon,2)


## ----echo=TRUE, eval=TRUE, results = 'hide'------------------------------
Esq <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets, 
              covars = NULL, indices = NULL, advice = advice, 
              main.ctrl = main.ctrl, biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl.Esq, 
              covars.ctrl = NULL, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl, 
              advice.ctrl = advice.ctrl) 


## ----echo=TRUE, eval=TRUE, results = 'hide'------------------------------
max <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets, 
              covars = NULL, indices = NULL, advice = advice, 
              main.ctrl = main.ctrl, biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl.max, 
              covars.ctrl = NULL, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl, 
              advice.ctrl = advice.ctrl) 


## ----echo=TRUE, eval=TRUE, results = 'hide'------------------------------
min <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets, 
              covars = NULL, indices = NULL, advice = advice, 
              main.ctrl = main.ctrl, biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl.min, 
              covars.ctrl = NULL, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl,
              advice.ctrl = advice.ctrl)


## ----echo=TRUE, eval=TRUE, results = 'hide'------------------------------
hke <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets, 
              covars = NULL, indices = NULL, advice = advice, 
              main.ctrl = main.ctrl, biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl.HKE, 
              covars.ctrl = NULL, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl, 
              advice.ctrl = advice.ctrl) 


## ----echo=TRUE, eval=TRUE, results = 'hide'------------------------------
ldb <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets, 
              covars = NULL, indices = NULL, advice = advice, 
              main.ctrl = main.ctrl, biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl.LDB, 
              covars.ctrl = NULL, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl, 
              advice.ctrl = advice.ctrl) 


## ----echo=TRUE, eval=TRUE, results = 'hide'------------------------------
meg <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets, 
              covars = NULL, indices = NULL, advice = advice, 
              main.ctrl = main.ctrl, biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl.MEG, 
              covars.ctrl = NULL, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl,
              advice.ctrl = advice.ctrl) 


## ----echo=TRUE, eval=TRUE, results = 'hide'------------------------------
mon <- FLBEIA(biols = biols, SRs = SRs, BDs = NULL, fleets = fleets, 
              covars = NULL, indices = NULL, advice = advice, 
              main.ctrl = main.ctrl, biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl.MON, 
              covars.ctrl = NULL, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl, 
              advice.ctrl = advice.ctrl) 


## ----echo=TRUE, eval=TRUE------------------------------------------------
bio <- rbind(bioSum(max, scenario = 'max', years = ac(2019), long = TRUE), 
             bioSum(min, scenario = 'min', years = ac(2019), long = TRUE),
             bioSum(hke, scenario = 'hke', years = ac(2019), long = TRUE), 
             bioSum(ldb, scenario = 'ldb', years = ac(2019), long = TRUE),
             bioSum(meg, scenario = 'meg', years = ac(2019), long = TRUE), 
             bioSum(mon, scenario = 'mon', years = ac(2019), long = TRUE),
             bioSum(Esq, scenario = 'Esq', years = ac(2019), long = TRUE))


## ----echo=TRUE, eval=TRUE, fig.height = 20/2.54--------------------------
nsc <- 7
nst <- 4
scnms <- unique(bio$scenario)

TAC <- c(8281,1633,399,2153)

catch_stock <- matrix(NA, nsc, nst, dimnames = list(scnms, stknms[c(1,3,4,2)]))
for(sc in scnms) 
  for(st in stknms) 
    catch_stock[sc,st] <- subset(bio, indicator == 'catch' &  year == '2019'& 
                                          stock == st & scenario == sc)$value

diff_catch <- round(sweep(catch_stock, 2, TAC, "-"))

maxC <- max(catch_stock)
minC <- min(diff_catch)

a <- barplot(t(catch_stock), beside = TRUE, ylim = c(-7000,maxC*1.3),  xlim = c(1,50), 
             names.arg=rep("",length(scnms)))
segments(0,0,0, 35)
segments(rep(1,4), TAC, rep(35,4), TAC,lty = 2)
text(36, TAC, 1:4, cex = 0.7)
text(a[2,], maxC*1.15, c('max', 'min',  'hke', 'ldb', 'meg', 'mon', 'sq_E'))
text(a[3,1], maxC*1.2, 'Scenarios:')
for(i in 1:7){ 
  polygon(c(a[1,i]-0.75, a[1,i]-0.75, a[4,i]+0.75, a[4,i]+0.75), 
          c(minC*1.2,maxC*1.1 , maxC*1.1, minC*1.2), lty = 2)
  text(a[,i],minC*1.4,1:4, cex = 0.9)
}

# Plot the overshoot using shading lines.
for(st in 1:4){
  sces <- which(diff_catch[,st] > 0)
  for(i in sces){ 
    polygon(c(a[st,i]-0.5, a[st,i]-0.5, a[st,i]+0.5, a[st,i]+0.5), 
            c(TAC[st], catch_stock[i,st], catch_stock[i,st], TAC[st]),  col = 'white')
    polygon(c(a[st,i]-0.5, a[st,i]-0.5, a[st,i]+0.5, a[st,i]+0.5), 
            c(TAC[st], catch_stock[i,st], catch_stock[i,st], TAC[st]), density = 25)
  }
}

for(st in 1:4){
  sces <- which(diff_catch[,st] < 0)
  for(i in sces){ 
    polygon(c(a[st,i]-0.5, a[st,i]-0.5, a[st,i]+0.5, a[st,i]+0.5), 
            c(0, diff_catch[i,st], diff_catch[i,st], 0),  col = grey((st-1)/3))
  }
}

mtext('Predicted catches for 2019 per stock and scenario', font.main = 1,3,2,at = 17)
mtext(substitute(italic("Overshoot")), 2, 3, at = 7000)
mtext(substitute(italic("|--------------------------------------->")), 2, 2, at = 0, adj = 0)
mtext(substitute(italic("<-------------------|")), 2, 2, at = 0, adj = 1)
mtext(substitute(italic("Undershoot")), 2, 3, at = -6000)

legend(a[4,7]+2, maxC/2, c('1: Hake', '2: Four-spot megrim', '3: Megrim', '4: White anglerfish'), 
       fill =  grey((1:4-1)/4), cex = 0.7, bty = 'n')


