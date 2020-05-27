## ---- ini, echo=FALSE, results='hide', message=FALSE, warning=FALSE-----------
# This chunk set the document environment, so it is hidden
library(knitr)
knitr::opts_chunk$set(fig.align="center",
                      message=FALSE, warning=FALSE, echo=TRUE, cache=FALSE)
options(width=50)
set.seed(1423)


## ----echo=FALSE, out.width='20%'----------------------------------------------
include_graphics('images/FLBEIA_logo.png')


## ---- eval=FALSE--------------------------------------------------------------
## data(package="FLBEIA")


## ---- eval=FALSE--------------------------------------------------------------
## install.packages( c("FLCore", "FLFleet", "FLBEIA"),
##                   repos="http://flr-project.org/R")


## ---- pkgs, results = "hide"--------------------------------------------------
library(FLBEIA)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
  rm(list=ls()) # empty the workspace
  
  data(one)     # load the datasets
  data(oneIt)
  data(multi)


## ----echo=TRUE----------------------------------------------------------------
  checkBiols(oneBio)
  checkBiols(oneItBio)
  checkBiols(multiBio)


## ----echo=TRUE, eval=FALSE----------------------------------------------------
##   obj1 <- obj2 <- oneBio
## 
##   mat(obj1$stk1)[1,1,] <- -0.5 # mat < 0
##   checkBiols(obj1)             # returns an error
## 
##   mat(obj2$stk1)[1,1,] <- 5    # mat > 1
##   checkBiols(obj2)             # returns an error


## ----echo=TRUE, warning=TRUE--------------------------------------------------
  checkFleets(oneFl)
  checkFleets(oneItFl)
  checkFleets(multiFl)


## ----echo=TRUE, eval=FALSE----------------------------------------------------
##   checkFleets(multiFl, ctrl = multiFlC) # returns an error


## ----echo=TRUE----------------------------------------------------------------
  sim.years <- as.numeric(multiMainC$sim.years)
  checkFleets( window(multiFl, start = sim.years[1]-1, end = sim.years[2]), 
               ctrl = multiFlC)


## ----echo=TRUE, eval=FALSE----------------------------------------------------
##   obj1 <- obj2 <- obj3 <- obj4 <- multiFl
## 
##   obj1$fl1@effort[,ac(1990),,1,]
##   obj1$fl1@metiers$met1@effshare[,ac(1990),,1,] <- NA # sum != 1, but effort = 0
##   checkFleets(obj1)                                   # pass the check
## 
##   obj1$fl1@effort[,ac(1999),,1,]
##   obj1$fl1@metiers$met1@effshare[,ac(1999),,1,] <- 5 # sum != 1, and effort > 0
##   checkFleets(obj1)                                  # returns an error
## 
##   obj2$fl1@metiers$met1@catches$stk1@landings.sel[] <-
##     obj2$fl1@metiers$met1@catches$stk1@discards.sel[] <- 0 # landins.sel + discards.sel != 1
##   checkFleets(obj2)                                        # returns an error
## 
##   obj3$fl1@metiers$met1@catches$stk1@landings.wt[,5,] <- NA   # landings.wt == NA
##   checkFleets(obj3)                                           # returns an error
## 
##   obj3$fl1@metiers$met1@catches$stk1@landings.wt[,5,] <- -0.7 # landings.wt < 0
##   checkFleets(obj3)                                           # returns an error
## 
##   obj4$fl1@metiers$met1@catches$stk1@discards.wt[,5,] <- NA   # discards.wt == NA
##   checkFleets(obj4)                                           # returns an error
## 
##   obj4$fl1@metiers$met1@catches$stk1@discards.wt[,5,] <- -0.1 # discards.wt < 0
##   checkFleets(obj4)                                           # returns an error


## ----echo=TRUE, eval=FALSE----------------------------------------------------
##   checkSRs(oneSR)
##   checkSRs(oneItSR)
##   checkSRs(multiSR)
##   checkBDs(multiBD)


## ----echo=TRUE----------------------------------------------------------------
  checkSRs(lapply(oneSR, window, start = sim.years[1]-1, end = sim.years[2]))
  checkSRs(lapply(oneItSR, window, start = sim.years[1]-1, end = sim.years[2]))


## ----echo=TRUE, eval=FALSE----------------------------------------------------
## # BDs
## 
##   obj1 <- obj2 <- obj3 <- multiSR
## 
##   obj1$stk1@proportion[,,,1,] <- -1000   # proportions > 0
##   checkSRs(obj1)                         # returns an error
## 
##   obj1$stk1@proportion[,,,1,] <- 1000    # proportions < 1
##   checkSRs(obj1)                         # returns an error
## 
##   obj2$stk1@proportion[,,,1:4,] <- 0.5   # sum proportions = 1
##   checkSRs(obj2)                         # returns an error
## 
##   obj3$stk1@uncertainty[1,1,,1,] <- -0.5 # uncertainty> 0
##   checkSRs(obj3)                         # returns an error
## 
## # SRs
## 
##   obj1 <- obj2 <- obj3 <- multiBD
## 
##   obj1$stk2@alpha[1,1,] <- 10          # alpha < 1
##   checkBDs(obj1)                       # returns an error
## 
##   obj2$stk2@alpha[1,1,] <- (obj2$stk2@params["p",1,1,] / obj2$stk2@params["r",1,1,]+1) ^
##     (1/obj2$stk2@params["p",1,1,]) - 1 # alpha > (p/r+1)^(1/p)
##   checkBDs(obj2)                       # returns an error


## ----echo=TRUE----------------------------------------------------------------
  checkAdvice(oneAdv)
  checkAdvice(oneItAdv)
  checkAdvice(multiAdv)


## ----echo=TRUE, eval=FALSE----------------------------------------------------
##   obj1 <- multiAdv
##   obj1$quota.share$stk1[,1,] <- 2 # sum quota shares != 1
##   checkAdvice(obj1)               # returns an error


## ----echo=TRUE----------------------------------------------------------------
  checkObsctrl(oneObsC)
  checkObsctrl(oneObsCIndAge)
  checkObsctrl(oneObsCIndBio)
  checkObsctrl(oneItObsC)
  checkObsctrl(oneItObsCIndAge)
  checkObsctrl(oneItObsCIndBio)
  checkObsctrl(multiObsC)


## ----echo=TRUE, eval=FALSE----------------------------------------------------
## # Index: total biomass
## 
##   obj1 <- oneObsCIndBio
## 
##   obj1$stk1$stkObs$land.bio.error[,1,] <- -0.7 # error < 0
##   checkObsctrl(obj1)                           # returns an error
## 
## # Index: numbers at age
## 
##   obj2 <- oneObsCIndAge
## 
##   obj2$stk1$stkObs$ages.error[1,,,] <- 2 # sum ages.error by age != 1
##   checkObsctrl(obj2)                     # returns an error


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
checkFLBEIAData( biols = oneBio, SRs = oneSR, BDs = NULL, fleets = oneFl,
                 covars = oneCv, indices = NULL, advice = oneAdv,
                 main.ctrl = oneMainC, biols.ctrl = oneBioC, fleets.ctrl = oneFlC,
                 covars.ctrl = oneCvC, obs.ctrl = oneObsC, assess.ctrl = oneAssC, 
                 advice.ctrl = oneAdvC)

checkFLBEIAData( biols = oneItBio, SRs = oneItSR, BDs = NULL, fleets = oneItFl,
                 covars = oneItCv, indices = NULL, advice = oneItAdv,
                 main.ctrl = oneItMainC, biols.ctrl = oneItBioC, fleets.ctrl = oneItFlC,
                 covars.ctrl = oneItCvC, obs.ctrl = oneItObsC, assess.ctrl = oneItAssC, 
                 advice.ctrl = oneItAdvC)

checkFLBEIAData( biols = multiBio, SRs = multiSR, BDs = multiBD, fleets = multiFl,
                 covars = multiCv, indices = NULL, advice = multiAdv,
                 main.ctrl = multiMainC, biols.ctrl = multiBioC, fleets.ctrl = multiFlC,
                 covars.ctrl = multiCvC, obs.ctrl = multiObsC, assess.ctrl = multiAssC, 
                 advice.ctrl = multiAdvC)

