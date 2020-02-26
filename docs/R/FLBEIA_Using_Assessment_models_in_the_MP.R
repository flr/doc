## ---- ini, echo=FALSE, results='hide', message=FALSE---------------------
# This chunk set the document environment, so it is hidden
library(knitr)
source("R/ini.R")
knitr::opts_chunk$set(fig.align='center',
  message=FALSE, warning=FALSE, echo=TRUE, cache=FALSE)
options(width=50)
set.seed(1423)


## ----logo, echo=FALSE, out.width='20%'-----------------------------------
include_graphics('images/FLBEIA_logo.png')


## ----package, eval=FALSE-------------------------------------------------
## data(package='FLBEIA')


## ----installation, eval=FALSE--------------------------------------------
## install.packages( c("ggplot2"))
## install.packages( c("FLCore", "FLBEIA", "FLFleets", "FLash",
##                     "FLAssess", "FLXSA", "FLa4a", "ggplotFL"),
##                   repos="http://flr-project.org/R")
## # Package for running SPiCT (only development version --> needed package "devtools"")
## install.packages("devtools")
## library(devtools)
## install_github("mawp/spict/spict")


## ----libraries, pkgs, results = "hide"-----------------------------------
# Load all necessary packages.
library(FLBEIA)
library(FLAssess)
library(FLash)
library(ggplotFL)


## ----data, echo=TRUE, eval=TRUE------------------------------------------
rm(list=ls()) # empty the workspace
data(one)     # load the dataset


## ----assCtrl, echo=TRUE, eval=FALSE--------------------------------------
## oneAssC


## ----s0, echo=TRUE, eval=TRUE, results = "hide"--------------------------
s0 <- FLBEIA( biols       = oneBio,   # FLBiols: FLBiol for stk1.
              SRs         = oneSR,    # List: FLSRSim for stk1.
              BDs         = NULL,     # Not population with biomass dynamics.
              fleets      = oneFl,    # FLFleets: one fleet.
              covars      = oneCv,    # List: covars related to economy.
              indices     = NULL,     # Not indices.
              advice      = oneAdv,   # List: 'TAC' and 'quota.share'
              main.ctrl   = oneMainC, # List: info on start and end of the simulation.
              biols.ctrl  = oneBioC,  # List: model to simulate the stock dynamics.
              fleets.ctrl = oneFlC,   # List: fleet dynamics models select. and other params.
              covars.ctrl = oneCvC,   # List: covariates dynamics ("fixedCovar").
              obs.ctrl    = oneObsC,  # List: type of stock and index observation
                                      #       ("PerfectObs").
              assess.ctrl = oneAssC,  # List: assessment model used ("NoAssessment").
              advice.ctrl = oneAdvC)  # List: rule for TAC advice ("IcesHCR").


## ----s0_out, echo=TRUE, eval=TRUE, results = "hide"----------------------
stk1.mp0 <- s0$stocks[['stk1']]
stk1.om0 <- biolfleets2flstock(s0$biols[['stk1']], s0$fleets)
plot( FLStocks(real=stk1.om0, obs=stk1.mp0)) + theme(legend.position="top")


## ----spict, echo=TRUE, eval=TRUE-----------------------------------------
library(spict)


## ----assCtrl2, echo=TRUE, eval=FALSE-------------------------------------
## oneAssC


## ----assCtrl3, echo=TRUE, eval=TRUE--------------------------------------
oneAssC.spict <- oneAssC
oneAssC.spict[["stk1"]]$assess.model  <- "spict2flbeia" # selected assessment model
oneAssC.spict[["stk1"]]$harvest.units <- "f"


## ----sum, echo=TRUE, eval=TRUE-------------------------------------------
summary(oneIndBio)
summary(oneObsCIndBio)

# Check the observation controls related to the assessment and the observation of the index
oneObsCIndBio$stk1$stkObs$stkObs.model
oneObsCIndBio$stk1$indObs


## ----s1, echo=TRUE, eval=TRUE, results = "hide"--------------------------
s1 <- FLBEIA( biols       = oneBio,    # FLBiols: FLBiol for stk1.
              SRs         = oneSR,     # List: FLSRSim for stk1.
              BDs         = NULL,      # Not population with biomass dynamics.
              fleets      = oneFl,     # FLFleets: one fleet.
              covars      = oneCv,     # List: covars related to economy.
              indices     = oneIndBio, # Biomass index.
              advice      = oneAdv,    # List: 'TAC' and 'quota.share'
              main.ctrl   = oneMainC,  # List: info on start and end of the simulation.
              biols.ctrl  = oneBioC,   # List: model to simulate the stock dynamics.
              fleets.ctrl = oneFlC,    # List: fleet dynamics models select. and other params.
              covars.ctrl = oneCvC,    # List: covariates dynamics ("fixedCovar").
              obs.ctrl    = oneObsCIndBio, # List: type of stock and index observation
                                           #       ("age2bioDat","bioInd").
              assess.ctrl = oneAssC.spict, # List: assessment model used ("spict2flbeia").
              advice.ctrl = oneAdvC)  # List: rule for TAC advice ("IcesHCR").


## ----s1out, echo=TRUE, eval=TRUE, results = "hide"-----------------------
stk1.mp1 <- s1$stocks[['stk1']]
stk1.om1 <- biolfleets2flstock(s1$biols[['stk1']], s1$fleets)
adf <- as.data.frame
s1_pop <- rbind( data.frame( population='obs', indicator='SSB', as.data.frame(ssb(stk1.mp1))), 
                 data.frame( population='obs', indicator='Harvest', as.data.frame(harvest(stk1.mp1))), 
                 data.frame( population='obs', indicator='Catch', as.data.frame(catch(stk1.mp1))), 
                 data.frame( population='real', indicator='SSB', as.data.frame(ssb(stk1.om1))), 
                 data.frame( population='real', indicator='Harvest', as.data.frame(fbar(stk1.om1))), 
                 data.frame( population='real', indicator='Catch', as.data.frame(catch(stk1.om1))))
p <- ggplot( data=s1_pop, aes(x=year, y=data, color=population)) + 
  geom_line() +
  facet_grid(indicator ~ ., scales="free") + 
  geom_vline(xintercept = oneMainC$sim.years[['initial']]-1, linetype = "longdash")+
  theme_bw()+
  theme(text=element_text(size=15),
        title=element_text(size=15,face="bold"),
        strip.text=element_text(size=15), 
        legend.position="top")+
  ylab("")
print(p)


## ----FLa4a, echo=TRUE, eval=TRUE-----------------------------------------
library(FLa4a)


## ----data2, echo=TRUE, eval=TRUE-----------------------------------------
rm(list=ls()) # empty the workspace
data(one)     # load the dataset


## ----assCtrl4, echo=TRUE, eval=FALSE-------------------------------------
## oneAssC


## ----assCtrl5, echo=TRUE, eval=TRUE--------------------------------------
oneAssC.sca <- oneAssC
oneAssC.sca$stk1$assess.model <- "sca2flbeia" # selected assessment model
oneAssC.sca[["stk1"]]$harvest.units <- "f"
oneAssC.sca[["stk1"]]$control$test <- TRUE    # control values


## ----sum2, echo=TRUE, eval=TRUE------------------------------------------
summary(oneIndAge)
summary(oneObsCIndAge)

# Check the observation controls related to the assessment and the observation of the index
oneObsCIndAge$stk1$stkObs$stkObs.model
oneObsCIndAge$stk1$indObs


## ----s2, echo=TRUE, eval=TRUE, results = "hide"--------------------------
s2 <- FLBEIA( biols       = oneBio,    # FLBiols: FLBiol for stk1.
              SRs         = oneSR,     # List: FLSRSim for stk1.
              BDs         = NULL,        # Not population with biomass dynamics.
              fleets      = oneFl,     # FLFleets: one fleet.
              covars      = oneCv,     # List: covars related to economy.
              indices     = oneIndAge, # Age-structured index.
              advice      = oneAdv,    # List: 'TAC' and 'quota.share'
              main.ctrl   = oneMainC,  # List: info on start and end of the simulation.
              biols.ctrl  = oneBioC,   # List: model to simulate the stock dynamics.
              fleets.ctrl = oneFlC,    # List: fleet dynamics models select. and other params.
              covars.ctrl = oneCvC,    # List: covariates dynamics ("fixedCovar").
              obs.ctrl    = oneObsCIndAge, # List: type of stock and index observation
                                           #       ("age2ageDat","ageInd").
              assess.ctrl = oneAssC.sca,   # List: assessment model used ("sca2flbeia").
              advice.ctrl = oneAdvC)  # List: rule for TAC advice ("IcesHCR").


## ----s2out, echo=TRUE, eval=TRUE, results = "hide"-----------------------
stk1.mp2 <- s2$stocks[['stk1']]
stk1.om2 <- biolfleets2flstock(s2$biols[['stk1']], s2$fleets)
adf <- as.data.frame
s2_pop <- rbind( data.frame( population='obs', indicator='SSB', as.data.frame(ssb(stk1.mp2))),
                 data.frame( population='obs', indicator='Harvest', as.data.frame(harvest(stk1.mp2))),
                 data.frame( population='obs', indicator='Catch', as.data.frame(catch(stk1.mp2))),
                 data.frame( population='real', indicator='SSB', as.data.frame(ssb(stk1.om2))),
                 data.frame( population='real', indicator='Harvest', as.data.frame(fbar(stk1.om2))),
                 data.frame( population='real', indicator='Catch', as.data.frame(catch(stk1.om2))))
p <- ggplot( data=s2_pop, aes(x=year, y=data, color=population)) +
  geom_line() +
  facet_grid(indicator ~ ., scales="free") +
  geom_vline(xintercept = oneMainC$sim.years[['initial']]-1, linetype = "longdash")+
  theme_bw()+
  theme(text=element_text(size=15),
        title=element_text(size=15,face="bold"),
        strip.text=element_text(size=15),
        legend.position="top")+
  ylab("")
print(p)


## ----FLXSA, echo=TRUE, eval=TRUE-----------------------------------------
library(FLXSA)


## ----data3, echo=TRUE, eval=TRUE-----------------------------------------
rm(list=ls()) # empty the workspace
data(one)     # load the dataset


## ----assCtrl6, echo=TRUE, eval=FALSE-------------------------------------
## oneAssC


## ----assCtrl7, echo=TRUE, eval=TRUE--------------------------------------
oneAssC1 <- list()
oneAssC1$stk1 <- list()
oneAssC1$stk1$assess.model <- 'FLXSA2flbeia'  # selected assessment model
oneAssC1$stk1$control      <- FLXSA.control() # default control values
oneAssC1$stk1$work_w_Iter  <- TRUE
oneAssC1$stk1$harvest.units <- 'f'


## ----sum3, echo=TRUE, eval=TRUE------------------------------------------
summary(oneIndAge)
summary(oneObsCIndAge)

# Check the observation controls related to the assessment and the observation of the index
oneObsCIndAge$stk1$stkObs$stkObs.model
oneObsCIndAge$stk1$indObs


## ----errs3, echo=FALSE, results='hide', message=FALSE--------------------
try( FLXSA::FLXSA(stock = biolfleets2flstock(oneBio$stk1, oneFl), indices = oneIndAge$stk1, 
             control=oneAssC1$stk1$control, diag.flag = FALSE), silent = TRUE)

## ----s3, echo=TRUE, eval=TRUE, results = "hide"--------------------------
s3 <- FLBEIA( biols       = oneBio,    # FLBiols: FLBiol for stk1.
              SRs         = oneSR,     # List: FLSRSim for stk1.
              BDs         = NULL,        # Not population with biomass dynamics.
              fleets      = oneFl,     # FLFleets: one fleet.
              covars      = oneCv,     # List: covars related to economy.
              indices     = oneIndAge, # Age-structured index.
              advice      = oneAdv,    # List: 'TAC' and 'quota.share'
              main.ctrl   = oneMainC,  # List: info on start and end of the simulation.
              biols.ctrl  = oneBioC,   # List: model to simulate the stock dynamics.
              fleets.ctrl = oneFlC,    # List: fleet dynamics models select. and other params.
              covars.ctrl = oneCvC,    # List: covariates dynamics ("fixedCovar").
              obs.ctrl    = oneObsCIndAge, # List: type of stock and index observation
                                           #       ("age2ageDat","ageInd").
              assess.ctrl = oneAssC1, # List: assessment model used ("FLXSAnew").
              advice.ctrl = oneAdvC)  # List: rule for TAC advice ("IcesHCR").


## ----sum4, echo=TRUE, eval=TRUE, results = "hide"------------------------
stk1.mp3 <- s3$stocks[['stk1']]
stk1.om3 <- biolfleets2flstock(s3$biols[['stk1']], s3$fleets)
plot( FLStocks(real=stk1.om3, obs=stk1.mp3)) + theme(legend.position="top")

