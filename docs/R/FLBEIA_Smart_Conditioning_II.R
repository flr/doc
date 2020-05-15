## ---- ini, echo=FALSE, results='hide', message=FALSE--------------------------
# This chunk set the document environment, so it is hidden
library(knitr)
knitr::opts_chunk$set(fig.align="center",
                      message=FALSE, warning=FALSE, echo=TRUE, cache=FALSE)
options(width=50)
set.seed(1423)


## ----echo=FALSE, out.width='20%'----------------------------------------------
include_graphics('images/FLBEIA_logo.png')


## ----install, eval=FALSE------------------------------------------------------
## install.packages( c("ggplot2", "XLConnect"))
## install.packages( c("FLCore", "FLFleet", "FLBEIA",
##                     "FLash", "FLAssess", "FLXSA"),
##                   repos="http://flr-project.org/R")


## ----libraries, pkgs, results = "hide"----------------------------------------
library(FLBEIA)
library(XLConnect)
library(plyr)


## ----data, echo=TRUE, eval=TRUE-----------------------------------------------
tdir <- tempdir()
# download.file("http://www.flr-project.org/doc/src/flbeia_smart_cond_II.zip", 
#               file.path(dir, "flbeia_smart_cond_II.zip"))
# unzip(file.path(dir, "flbeia_smart_cond_II.zip"), exdir=dir)
unzip("src/flbeia_smart_cond_II.zip", exdir=tdir)
tdir <- file.path(tdir,"flbeia_data_smart_cond_II")


## ----biol, echo=TRUE, eval=TRUE-----------------------------------------------
stknms <- c('HKE', 'LDB', 'MEG', 'MON')

hke <- create.biol.arrays(file.path(tdir,'stocks/HKE2017.xlsx'), name = 'HKE',
                   ages = 0:15, hist.yrs = 1982:2017 , sim.yrs = 2018:2025, 
                   fbar = c(1,3), mean.yrs = 2015:2017, source = 'excel')

mon <- create.biol.arrays(file.path(tdir,'stocks/MON2017.xlsx'), name = 'MON',
                          ages = 0:30, hist.yrs = 1980:2017 , sim.yrs = 2018:2025,
                           fbar = c(1,8), mean.yrs = 2015:2017,  source = 'excel')
ldb <- create.biol.arrays(file.path(tdir,'stocks/LDB2017.xlsx'), name = 'LDB',
                          ages = 0:7, hist.yrs = 1986:2017 , sim.yrs = 2018:2025, 
                          fbar = c(2,4), mean.yrs = 2015:2017, source = 'excel')

meg <- create.biol.arrays(file.path(tdir,'stocks/MEG2017.xlsx'), name = 'MEG',
                          ages = 1:7, hist.yrs = 1986:2017 , sim.yrs = 2018:2025, 
                          fbar = c(2,4),mean.yrs = 2015:2017, source = 'excel')


## ----biols, echo=TRUE, eval=TRUE----------------------------------------------
biols <- list(HKE = hke, MON = mon, LDB = ldb, MEG = meg)

biols <- FLBiols(lapply(biols, function(x) window(x, 2000,2025)))


## ----weights, echo=TRUE, eval=TRUE--------------------------------------------
biols$HKE@wt[, ac(2018:2020)] <- c(0.00, 0.05, 0.30, 0.87, 1.71, 2.72, 3.81, 4.93, 6.04, 
                                   7.11, 8.15, 9.13, 10.02, 10.82, 11.51, 12.39)
biols$LDB@wt[,ac(2018:2020)]  <- c(0.004,	0.024,	0.044,	0.071,	0.1,	0.131,	0.162,	
                                   0.218)
biols$MEG@wt[,ac(2018:2020)]  <- c(0.038, 0.089, 0.134, 0.180, 0.222, 0.2840, 0.396)


## ----echo=TRUE, eval=FALSE----------------------------------------------------
## ?create.fleets.arrays


## ----fleetNam, echo=TRUE, eval=TRUE-------------------------------------------
# fleet names
flnms <-  c('SP_GNS', 'PT_GNS', 'PT_GTR', 'SP_GTR', 'SP_LLS', 'PT_MIS', 'SP_MIS',
            'PT_OTB', 'SP_OTB', 'SP_OTB_24m', 'OTH_OTH', 'SP_PTB')

# List with the metiers for each fleet
flt_mt_nms <- list(SP_GNS = c("DEF_100_0_0", "DEF_60_79_0_0", "DEF_80_99_0_0"),
                   PT_GNS = "DEF_0_0_0", 
                   PT_GTR = "DEF_0_0_0", 
                   SP_GTR = "DEF_60_79_0_0",
                   SP_LLS = "DEF_0_0_0", 
                   PT_MIS = "MIS_0_0_0_HC", 
                   SP_MIS = "MIS_0_0_0_HC",
                   PT_OTB = c("CRU_55_0_0", "DEF_65_0_0"),
                   SP_OTB = c("DEF_65_0_0", "MPD_55_0_0"),
                   SP_OTB_24m = "MCD_55_0_0", 
                   OTH_OTH = "OTH",
                   SP_PTB = "MPD_55_0_0")

# List of list with the stocks caugth by each metier for each fleet
flt_mt_stk_nms <- list(SP_GNS = list(DEF_100_0_0 = c("HKE", "LDB", "MEG", "MON"),
                                     DEF_60_79_0_0 = c("HKE", "LDB", "MEG", "MON"),
                                     DEF_80_99_0_0 = c("HKE", "LDB", "MEG", "MON")),
                       PT_GNS = list(DEF_0_0_0 = c("HKE", "LDB", "MEG", "MON")),
                       PT_GTR = list(DEF_0_0_0 = c("HKE", "LDB", "MEG", "MON")),
                       SP_GTR = list(DEF_60_79_0_0 = c("HKE", "LDB", "MEG", "MON")),
                       SP_LLS = list(DEF_0_0_0 = c("HKE", "LDB",  "MON")),
                       PT_MIS = list(MIS_0_0_0_HC = c("HKE", "LDB", "MEG", "MON")),
                       SP_MIS = list(MIS_0_0_0_HC = c("HKE", "LDB", "MEG", "MON")),
                       PT_OTB = list(CRU_55_0_0 = c("HKE", "LDB", "MEG", "MON"),
                                     DEF_65_0_0 = c("HKE", "LDB", "MEG", "MON")),
                       SP_OTB = list(DEF_65_0_0 = c("HKE", "LDB", "MEG", "MON"),
                                     MPD_55_0_0 = c("HKE", "LDB", "MEG", "MON")),
                       SP_OTB_24m = list(MCD_55_0_0 = c("HKE", "LDB", "MEG", "MON")),
                       OTH_OTH = list(OTH = c("HKE", "LDB", "MEG", "MON")),
                       SP_PTB = list(MPD_55_0_0 = c("HKE", "LDB", "MEG", "MON")))


## ----stks, echo=TRUE, eval=TRUE-----------------------------------------------
# stock data file  names
stk_objs <- c(file.path(tdir,"stocks/HKE2017.xlsx"), file.path(tdir,"stocks/LDB2017.xlsx"), 
              file.path(tdir,"stocks/MEG2017.xlsx"), file.path(tdir,"stocks/MON2017.xlsx") )
names(stk_objs) <- c('HKE', 'LDB', 'MEG', 'MON')
stk_objs


## ----fleets, echo=TRUE, eval=TRUE, results = "hide"---------------------------
# Create the fleets object
fleets <- create.fleets.arrays(stk_objs,                               
                             caa_objs = c("caa_HKE.xlsx", "caa_LDB.xlsx", 
                                          "caa_MEG.xlsx", "caa_MON.xlsx"),
                             caa_objs_path = file.path(tdir,'fleets/'),
                             catch_obj     = file.path(tdir,'fleets/catch.csv'),
                             effort_obj    = file.path(tdir,'fleets/effort.csv'),
                             flt_obj = NULL,
                             stk_nms =  c('HKE', 'LDB', 'MEG', 'MON'),
                             flt_nms = flnms,
                             flt_mt_nms = flt_mt_nms,
                             flt_mt_stk_nms = flt_mt_stk_nms,
                             ages = list(HKE = ac(0:15), MON = ac(0:30), 
                                         MEG = ac(1:7), LDB = ac(0:7)),
                             hist.yrs = 2000:2017,
                             sim.yrs = 2018:2025,
                             mean.yrs = 2015:2017,
                             caa_flt_mt_correspondences = NULL, 
                             paa_flt_mt_correspondences = NULL,
                             caaOpt = c(HKE = 4, LDB = 4, MEG = 4, MON = 4),
                             update_price = FALSE,
                             priceOpt = NULL,
                             excel = TRUE)


## ----qcalc, echo=TRUE, eval=TRUE,  results = "hide"---------------------------

fleets.ctrl <- list()
for (fl in flnms) {
  fleets.ctrl[[fl]] <- list()
  for (st in catchNames(fleets[[fl]]))
    fleets.ctrl[[fl]][[st]][['catch.model']] <- "CobbDouglas"
}

fleets <- calculate.q.sel.flrObjs(biols, fleets, NULL, fleets.ctrl, 
                                  mean.yrs = ac(2015:2017), sim.yrs = ac(2018:2025))


## ----echo=FALSE, eval=TRUE----------------------------------------------------
list2env(list(fleets = fleets), globalenv())


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
validObject(fleets)


## ----price, echo=TRUE, eval=TRUE,  results = "hide"---------------------------
# Update the object with the price data
fleets <- create.fleets.arrays(flt_obj = fleets,
                               price_objs = c('paa_HKE.xlsx', 'paa_MEG.xlsx', 
                                              'paa_LDB.xlsx', 'paa_MON.xlsx'), 
                               price_objs_path = file.path(tdir,'fleets/'), 
                               priceOpt = c(HKE = 3, LDB = 4, MEG = 4, MON = 3),
                               paa_flt_mt_correspondences = file.path(tdir,'fleets/price_correspondences.xlsx'),
                               update_catch_effort = FALSE, 
                               update_price = TRUE, 
                               update_weight = FALSE,
                               hist.yrs = 2000:2017, sim.yrs = 2018:2025, 
                               mean.yrs = 2015:2017,
                               excel = TRUE)



## ----echo=TRUE, eval=TRUE-----------------------------------------------------
?create.ecoData


## ----ecoData, echo=TRUE, eval=TRUE--------------------------------------------

ecoD <- create.ecoData(file.path(tdir,'fleets/economic_data.xlsx'), fleets,
                       hist.yrs = 2005:2017, mean.yrs = 2017, 
                       sim.yrs = 2018:2025)

covars <- ecoD$covars
fleets <- ecoD$fleets


## ----landWt, echo=TRUE, eval=TRUE---------------------------------------------
for(fl in names(fleets)){
  for(mt in names(fleets[[fl]]@metiers)){
    if('LDB' %in% catchNames(fleets[[fl]])){
      fleets[[fl]]@metiers[[mt]]@catches[['LDB']]@landings.wt[,ac(2018:2020)]  <- c(0.002,	
                                    0.034,	0.069,	0.086,	0.106,	0.133,	0.162,	0.218)
      fleets[[fl]]@metiers[[mt]]@catches[['LDB']]@discards.wt[,ac(2018:2020)]  <-  c(0.004,	
                                       0.024,	0.041,	0.054,	0.069,	0.094, 0.116,	0.094)
    }
    
  if('MEG' %in% catchNames(fleets[[fl]])){
      fleets[[fl]]@metiers[[mt]]@catches[['MEG']]@landings.wt[,ac(2018:2020)]  <- c(0.064,	
                                              0.098,	0.135,	0.18,	0.222,	0.284,	0.396)
      fleets[[fl]]@metiers[[mt]]@catches[['MEG']]@discards.wt[,ac(2018:2020)]  <-  c(0.035,	
                                                0.062,	0.091,	0.125,	0.062,	0.038,	0)
  }  
  }
}


