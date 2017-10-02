## ----setup, include=FALSE------------------------------------------------
library(knitr)
source("R/ini.R")

## ---- ini, echo=FALSE, results='hide', message=FALSE---------------------
# This chunk set the document environment, so it is hidden
library(knitr)
knitr::opts_chunk$set(fig.align='center',
                      message=FALSE, warning=FALSE, echo=TRUE, cache=FALSE)
options(width=50)
set.seed(1423)

## ---- eval=FALSE---------------------------------------------------------
## data(package='FLBEIA')

## ---- eval=FALSE---------------------------------------------------------
## install.packages( c("ggplot2"))
## install.packages( c("FLCore", "FLBEIA", "FLFleets", "FLash", "FLAssess", "FLXSA", "ggplotFL"),
##                   repos="http://flr-project.org/R")

## ---- pkgs, results = "hide"---------------------------------------------
# Load all necessary packages.
library(FLBEIA)
library(FLXSA)
library(FLash)
library(ggplotFL)

## ----echo=TRUE, eval=TRUE------------------------------------------------
# Load the dataset
rm(list=ls())  # empty the workspace
data(oneIt) # load the dataset

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## # Objects with info related to management advice
## oneItAdv  # info on TAC/TAE and quota share
## oneItAdvC # control object related to advice

## ----echo=TRUE, eval=TRUE------------------------------------------------
# required an index in biomass:
ls()

summary(oneItIndBio[[1]]) # Biomass index
summary(oneItIndBio[['stk1']]$idBio) # or: oneItIndBio$stk1$idBio

summary(oneItObsCIndBio)  # Control object related to the biomass index

summary(oneItObsCIndBio$stk1)
oneItObsCIndBio$stk1$indObs

## ----echo=TRUE, eval=TRUE------------------------------------------------
# set the control parameters for the new HCR
oneItAdvC2 <- oneItAdvC
stk1.advC <- list()
stk1.advC$HCR.model <- 'annexIVHCR'     # selected HCR
stk1.advC$index <- 'id2'                # biomass index
nit <- dim(oneItFl[[1]]@effort)[6]
stk1.advC$ref.pts <- array( c(0.1,0.2), # reference points (alpha=0.1, beta=0.2)
                            dim=c(2,nit), 
                            dimnames = list(c('alpha','beta'),dimnames(oneItFl[[1]]@effort)$iter)) 
stk1.advC$type <- 2
oneItAdvC2$stk1 <- stk1.advC

# There is not assessment and the stock does not need to be obseved
oneItObsCIndBio$stk1$stkObs$stkObs.model <- 'NoObsStock'

# Set in the advice control the name of the index to be used
oneItAdvC2[['stk1']][["index"]] <- name(oneItIndBio$stk1$idBio)

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
s1 <- FLBEIA( biols       = oneItBio,   # FLBiols: FLBiol for stk1.
              SRs         = oneItSR,    # List: FLSRSim for stk1.
              BDs         = NULL,       # Not population with biomass dynamics.
              fleets      = oneItFl,    # FLFleets: one fleet.
              covars      = oneItCv,    # List: covars related to economy.
              indices     = NULL,       # Indices not used.
              advice      = oneItAdv,   # List: 'TAC' and 'quota.share'
              main.ctrl   = oneItMainC, # List: info on start and end of the simulation.
              biols.ctrl  = oneItBioC,  # List: model to simulate the stock dynamics.
              fleets.ctrl = oneItFlC,   # List: fleet dyn. models and other parameters.
              covars.ctrl = oneItCvC,   # List: covariates dynamics ("fixedCovar").
              obs.ctrl    = oneItObsC,  # List: type of stock and index observation
                                        #       ("PerfectObs","NoObsIndex").
              assess.ctrl = oneItAssC,  # List: assessment model used ("NoAssessment").
              advice.ctrl = oneItAdvC)  # List: rule for TAC advice ("IcesHCR").

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
s1a <- FLBEIA( biols       = oneItBio,    # FLBiols: FLBiol for stk1.
               SRs         = oneItSR,     # List: FLSRSim for stk1.
               BDs         = NULL,        # Not population with biomass dynamics.
               fleets      = oneItFl,     # FLFleets: one fleet.
               covars      = oneItCv,     # List: covars related to economy.
               indices     = oneItIndBio, # Biomass index.
               advice      = oneItAdv,    # List: 'TAC' and 'quota.share'
               main.ctrl   = oneItMainC,  # List: info on start and end of the simulation.
               biols.ctrl  = oneItBioC,   # List: model to simulate the stock dynamics.
               fleets.ctrl = oneItFlC,    # List: fleet dyn. models and other parameters.
               covars.ctrl = oneItCvC,    # List: covariates dynamics ("fixedCovar").
               obs.ctrl    = oneItObsCIndBio, # List: type of stock and index observation
                                              #       ("NoObsStock","bioInd").
               assess.ctrl = oneItAssC,   # List: assessment model used ("NoAssessment").
               advice.ctrl = oneItAdvC2)  # List: rule for TAC advice ("AnnexIV").

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
Blim.stk1 <- 800
Bpa.stk1  <- 1200

## ----echo=TRUE, eval=TRUE, fig.width = 8, fig.height = 5-----------------
# - stock summary
s10_bio <- rbind( bioSum(s1), 
                  bioSum(s1a, scenario='a4')) # biological indicators
plotbioSum( s10_bio, Blim=Blim.stk1, Bpa=Bpa.stk1, proj.yr=oneItMainC$sim.years[['initial']])

## ----echo=TRUE, eval=TRUE, fig.width = 8, fig.height = 5-----------------
# - economic summary
s10_flt <- rbind( fltSum(s1), 
                  fltSum(s1a, scenario='a4')) # indicators at fleet level
plotfltSum( s10_flt, proj.yr=oneItMainC$sim.years[['initial']])

## ----echo=TRUE, eval=TRUE, fig.width = 7, fig.height = 4-----------------
# - risk summary
s1_risk  <- riskSum(  s1, Bpa = c(stk1=Bpa.stk1), Blim = c(stk1=Blim.stk1), Prflim = c(fl1 = 0))
s1a_risk <- riskSum( s1a, Bpa = c(stk1=Bpa.stk1), Blim = c(stk1=Blim.stk1), Prflim = c(fl1 = 0), 
                     scenario='a4')
s10_risk <- rbind( s1_risk, s1a_risk) # risk indicators
s10_risk$year <- as.numeric(s10_risk$year)
p <- ggplot( data=s10_risk, aes(x=year, y=value, color=scenario)) + 
  geom_line() +
  facet_wrap(~indicator, scales="free") + 
  facet_grid(indicator ~ .) + 
  geom_vline(xintercept = oneItMainC$sim.years[['initial']]-1, linetype = "longdash")+
  theme_bw()+
  theme(text=element_text(size=15),
        title=element_text(size=15,face="bold"),
        strip.text=element_text(size=15))+
  ylab("Risk")
print(p)

## ----echo=TRUE, eval=TRUE------------------------------------------------
data(oneIt) # load the dataset

## ----echo=TRUE, eval=TRUE------------------------------------------------
# Fleet dynamics: effort model
oneItFlC$fl1$effort.model
# reset it to fixed effort
oneItFlC$fl1$effort.model <- 'fixedEffort'

## ----echo=TRUE, eval=TRUE------------------------------------------------
# Define the FLFleetsExt object for the alternative scenarios
oneItFl2a <- oneItFl

# Store values of simulation and historic years
yrs <- dimnames(oneItFl[[1]]@effort)$year
sim.yrs  <- as.character(oneItMainC$sim.years['initial']:oneItMainC$sim.years['final'])
hist.yrs <- yrs[!yrs %in% sim.yrs]

# Change the effort for the simulation years for the alternative scenario:
oneItFl2a[[1]]@effort[,sim.yrs,] <- 
  0.80 * yearMeans(oneItFl2a[[1]]@effort[,hist.yrs[length(hist.yrs)+(-2:0)],])

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
s2 <- FLBEIA( biols       = oneItBio,   # FLBiols: FLBiol for stk1.
              SRs         = oneItSR,    # List: FLSRSim for stk1.
              BDs         = NULL,       # Not population with biomass dynamics.
              fleets      = oneItFl,    # FLFleets: one fleet.
              covars      = oneItCv,    # List: covars related to economy.
              indices     = NULL,       # Not indices.
              advice      = oneItAdv,   # List: 'TAC' and 'quota.share'
              main.ctrl   = oneItMainC, # List: info on start and end of the simulation.
              biols.ctrl  = oneItBioC,  # List: model to simulate the stock dynamics.
              fleets.ctrl = oneItFlC,   # List: fleet dyn. models and other parameters.
              covars.ctrl = oneItCvC,   # List: covariates dynamics ("fixedCovar").
              obs.ctrl    = oneItObsC,  # List: type of stock and index observation
                                        #       ("PerfectObs","NoObsIndex").
              assess.ctrl = oneItAssC,  # List: assessment model used ("NoAssessment").
              advice.ctrl = oneItAdvC)  # List: rule for TAC advice ("IcesHCR").

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
s2a <- FLBEIA( biols       = oneItBio,   # FLBiols: FLBiol for stk1.
               SRs         = oneItSR,    # List: FLSRSim for stk1.
               BDs         = NULL,       # Not population with biomass dynamics.
               fleets      = oneItFl2a,  # FLFleets: one fleet.
               covars      = oneItCv,    # List: covars related to economy.
               indices     = NULL,       # Not indices.
               advice      = oneItAdv,   # List: 'TAC' and 'quota.share'
               main.ctrl   = oneItMainC, # List: info on start and end of the simulation.
               biols.ctrl  = oneItBioC,  # List: model to simulate the stock dynamics.
               fleets.ctrl = oneItFlC,   # List: fleet dyn. models and other parameters.
               covars.ctrl = oneItCvC,   # List: covariates dynamics ("fixedCovar").
               obs.ctrl    = oneItObsC,  # List: type of stock and index observation
                                         #       ("PerfectObs","NoObsIndex").
               assess.ctrl = oneItAssC,  # List: assessment model used ("NoAssessment").
               advice.ctrl = oneItAdvC)  # List: rule for TAC advice ("IcesHCR").

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
Blim.stk1 <- 800
Bpa.stk1  <- 1200

## ----echo=TRUE, eval=TRUE, fig.width = 8, fig.height = 5-----------------
# - stock summary
s20_bio <- rbind( bioSum(s2), 
                  bioSum(s2a, scenario='E20')) # biological indicators
plotbioSum( s20_bio, Blim=Blim.stk1, Bpa=Bpa.stk1, proj.yr=oneItMainC$sim.years[['initial']])

## ----echo=TRUE, eval=TRUE, fig.width = 8, fig.height = 5-----------------
# - economic summary
s20_flt <- rbind( fltSum(s2), 
                  fltSum(s2a, scenario='E20')) # indicators at fleet level
plotfltSum( s20_flt, proj.yr=oneItMainC$sim.years[['initial']])

## ----echo=TRUE, eval=TRUE, fig.width = 7, fig.height = 4-----------------
# - risk summary
s2_risk  <- riskSum(  s2, Bpa = c(stk1=Bpa.stk1), Blim = c(stk1=Blim.stk1), Prflim = c(fl1 = 0))
s2a_risk <- riskSum( s2a, Bpa = c(stk1=Bpa.stk1), Blim = c(stk1=Blim.stk1), Prflim = c(fl1 = 0), 
                     scenario='E20')
s20_risk <- rbind( s2_risk, s2a_risk) # risk indicators
s20_risk$year <- as.numeric(s20_risk$year)
p <- ggplot( data=s20_risk, aes(x=year, y=value, color=scenario)) + 
  geom_line() +
  facet_wrap(~indicator, scales="free") + 
  facet_grid(indicator ~ .) + 
  geom_vline(xintercept = oneItMainC$sim.years[['initial']]-1, linetype = "longdash")+
  theme_bw()+
  theme(text=element_text(size=15),
        title=element_text(size=15,face="bold"),
        strip.text=element_text(size=15))+
  ylab("Risk")
print(p)

## ----echo=TRUE, eval=TRUE------------------------------------------------
data(oneIt) # load the dataset

## ----echo=TRUE, eval=TRUE------------------------------------------------
# summary(oneItFl)
names(oneItFl)                                    # only one fleet (fl1)
names(oneItFl[['fl1']]@metiers)                   # with an unique metier (met1) 
names(oneItFl[['fl1']]@metiers[['met1']]@catches) # targeting one stock (stk1)
# oneItFl[['fl1']]@metiers[['met1']]@catches[['stk1']]@catch.q

## ----echo=TRUE, eval=TRUE------------------------------------------------
# Define the FLFleetsExt object for the alternative scenarios

# Check simulation and historic years
yrs <- dimnames(oneItFl[[1]]@effort)$year
sim.yrs  <- as.character(oneItMainC$sim.years['initial']:oneItMainC$sim.years['final'])
hist.yrs <- yrs[!yrs %in% sim.yrs]

# Change the catchability for the simulation years:
oneItFl3a <- oneItFl
oneItFl3a[['fl1']]@metiers[['met1']]@catches[['stk1']]@catch.q[1:6,sim.yrs,] <- 0

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
s3 <- FLBEIA( biols       = oneItBio,   # FLBiols: FLBiol for stk1.
              SRs         = oneItSR,    # List: FLSRSim for stk1.
              BDs         = NULL,       # Not population with biomass dynamics.
              fleets      = oneItFl,    # FLFleets: one fleet.
              covars      = oneItCv,    # List: covars related to economy.
              indices     = NULL,       # Not indices.
              advice      = oneItAdv,   # List: 'TAC' and 'quota.share'
              main.ctrl   = oneItMainC, # List: info on start and end of the simulation.
              biols.ctrl  = oneItBioC,  # List: model to simulate the stock dynamics.
              fleets.ctrl = oneItFlC,   # List: fleet dyn. models and other parameters.
              covars.ctrl = oneItCvC,   # List: covariates dynamics ("fixedCovar").
              obs.ctrl    = oneItObsC,  # List: type of stock and index observation
                                        #       ("PerfectObs","NoObsIndex").
              assess.ctrl = oneItAssC,  # List: assessment model used ("NoAssessment").
              advice.ctrl = oneItAdvC)  # List: rule for TAC advice ("IcesHCR").

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
s3a <- FLBEIA( biols       = oneItBio,   # FLBiols: FLBiol for stk1.
               SRs         = oneItSR,    # List: FLSRSim for stk1.
               BDs         = NULL,       # Not population with biomass dynamics.
               fleets      = oneItFl3a,  # FLFleets: one fleet.
               covars      = oneItCv,    # List: covars related to economy.
               indices     = NULL,       # Not indices.
               advice      = oneItAdv,   # List: 'TAC' and 'quota.share'
               main.ctrl   = oneItMainC, # List: info on start and end of the simulation.
               biols.ctrl  = oneItBioC,  # List: model to simulate the stock dynamics.
               fleets.ctrl = oneItFlC,   # List: fleet dyn. models and other parameters.
               covars.ctrl = oneItCvC,   # List: covariates dynamics ("fixedCovar").
               obs.ctrl    = oneItObsC,  # List: type of stock and index observation
                                         #       ("PerfectObs","NoObsIndex").
               assess.ctrl = oneItAssC,  # List: assessment model used ("NoAssessment").
               advice.ctrl = oneItAdvC)  # List: rule for TAC advice ("IcesHCR").

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
Blim.stk1 <- 800
Bpa.stk1  <- 1200

## ----echo=TRUE, eval=TRUE, fig.width = 8, fig.height = 5-----------------
# - stock summary
s30_bio <- rbind( bioSum(s3), 
                  bioSum(s3a, scenario='netBig')) # biological indicators
plotbioSum( s30_bio, Blim=Blim.stk1, Bpa=Bpa.stk1, proj.yr=oneItMainC$sim.years[['initial']])

## ----echo=TRUE, eval=TRUE, fig.width = 8, fig.height = 5-----------------
# - economic summary
s30_flt <- rbind( fltSum(s3), 
                  fltSum(s3a, scenario='netBig')) # indicators at fleet level
plotfltSum( s30_flt, proj.yr=oneItMainC$sim.years[['initial']])

## ----echo=TRUE, eval=TRUE, fig.width = 7, fig.height = 4-----------------
# - risk summary
s3_risk  <- riskSum(  s3, Bpa = c(stk1=Bpa.stk1), Blim = c(stk1=Blim.stk1), Prflim = c(fl1 = 0))
s3a_risk <- riskSum( s3a, Bpa = c(stk1=Bpa.stk1), Blim = c(stk1=Blim.stk1), Prflim = c(fl1 = 0), 
                     scenario='netBig')
s30_risk <- rbind( s3_risk, s3a_risk) # risk indicators
s30_risk$year <- as.numeric(s30_risk$year)
p <- ggplot( data=s30_risk, aes(x=year, y=value, color=scenario)) + 
  geom_line() +
  facet_wrap(~indicator, scales="free") + 
  facet_grid(indicator ~ .) + 
  geom_vline(xintercept = oneItMainC$sim.years[['initial']]-1, linetype = "longdash")+
  theme_bw()+
  theme(text=element_text(size=15),
        title=element_text(size=15,face="bold"),
        strip.text=element_text(size=15))+
  ylab("Risk")
print(p)

## ----echo=TRUE, eval=TRUE------------------------------------------------
data(multi) # load the dataset

## ----echo=TRUE, eval=TRUE------------------------------------------------
# summary(multiFl)
names(multiFl)                                    # two fleets (fl1, fl2)
names(multiFl[['fl1']]@metiers)                   # fl1: with two metiers (met1, met2) 
names(multiFl[['fl1']]@metiers[['met1']]@catches) # fl1_met1: targeting 2 stocks (stk1, stk2)
names(multiFl[['fl1']]@metiers[['met2']]@catches) # fl1_met2: targeting 2 stocks (stk1, stk2)
names(multiFl$fl2@metiers)                        # fl2: with two metiers (met1, met2) 
names(multiFl$fl2@metiers$met1@catches)           # fl2_met1: targeting 2 stocks (stk1, stk2)
names(multiFl$fl2@metiers$met2@catches)           # fl2_met1: targeting 2 stocks (stk1, stk2)

## ----echo=TRUE, eval=TRUE------------------------------------------------
names(multiFl$fl1@metiers) <- names(multiFl$fl2@metiers) <- c('metN', 'metS')

## ----echo=TRUE, eval=TRUE------------------------------------------------
# Define the FLFleetsExt object for the alternative scenarios

# Check simulation and historic years
yrs <- dimnames(multiFl[[1]]@effort)$year
sim.yrs  <- as.character(multiMainC$sim.years['initial']:multiMainC$sim.years['final'])
hist.yrs <- yrs[!yrs %in% sim.yrs]

# - Northern area closure:
# Change the effort share to 0 for metN and assign this share to the other metiers, 
# in this case only metS
multiFl4a <- multiFl
multiFl4a[['fl1']]@metiers[['metS']]@effshare[,sim.yrs,] <- 
  multiFl4a[['fl1']]@metiers[['metS']]@effshare[,sim.yrs,] + 
  multiFl4a[['fl1']]@metiers[['metN']]@effshare[,sim.yrs,]
multiFl4a[['fl1']]@metiers[['metN']]@effshare[,sim.yrs,] <- 0

# - Ban completely the fishery in the 3rd quarter:
# Set the fleet effort to 0 in this season for both fleets
multiFl4b <- multiFl
multiFl4b[['fl1']]@effort[,sim.yrs,,3,] <- 0
multiFl4b[['fl2']]@effort[,sim.yrs,,3,] <- 0

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
s4 <- FLBEIA( biols       = multiBio,   # FLBiols: FLBiol for stk1 and stk2.
              SRs         = multiSR,    # List: FLSRSim for stk1.
              BDs         = multiBD,    # List: FLBDSim for stk2.
              fleets      = multiFl,    # FLFleets: two fleets (fl1, fl2), with 2 metiers each.
              covars      = multiCv,    # List: covars related to economy.
              indices     = NULL,       # Not indices.
              advice      = multiAdv,   # List: 'TAC' and 'quota.share'
              main.ctrl   = multiMainC, # List: info on start and end of the simulation.
              biols.ctrl  = multiBioC,  # List: model to simulate the stock dynamics.
              fleets.ctrl = multiFlC,   # List: fleet dyn. models and other parameters.
              covars.ctrl = multiCvC,   # List: covariates dynamics ("fixedCovar").
              obs.ctrl    = multiObsC,  # List: type of stock and index observation
                                        #       ("PerfectObs","NoObsIndex").
              assess.ctrl = multiAssC,  # List: assessment model used ("NoAssessment").
              advice.ctrl = multiAdvC)  # List: rule for TAC advice ("IcesHCR").

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
s4a <- FLBEIA( biols       = multiBio,  # FLBiols: FLBiol for stk1 and stk2.
               SRs         = multiSR,    # List: FLSRSim for stk1.
               BDs         = multiBD,    # List: FLBDSim for stk2.
               fleets      = multiFl4a,  # FLFleets: two fleets (fl1, fl2), with 2 metiers each.
               covars      = multiCv,    # List: covars related to economy.
               indices     = NULL,       # Not indices.
               advice      = multiAdv,   # List: 'TAC' and 'quota.share'
               main.ctrl   = multiMainC, # List: info on start and end of the simulation.
               biols.ctrl  = multiBioC,  # List: model to simulate the stock dynamics.
               fleets.ctrl = multiFlC,   # List: fleet dyn. models and other parameters.
               covars.ctrl = multiCvC,   # List: covariates dynamics ("fixedCovar").
               obs.ctrl    = multiObsC,  # List: type of stock and index observation
                                         #       ("PerfectObs","NoObsIndex").
               assess.ctrl = multiAssC,  # List: assessment model used ("NoAssessment").
               advice.ctrl = multiAdvC)  # List: rule for TAC advice ("IcesHCR").

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
s4b <- FLBEIA( biols       = multiBio,  # FLBiols: FLBiol for stk1 and stk2.
               SRs         = multiSR,    # List: FLSRSim for stk1.
               BDs         = multiBD,    # List: FLBDSim for stk2.
               fleets      = multiFl4b,  # FLFleets: two fleets (fl1, fl2), with 2 metiers each.
               covars      = multiCv,    # List: covars related to economy.
               indices     = NULL,       # Not indices.
               advice      = multiAdv,   # List: 'TAC' and 'quota.share'
               main.ctrl   = multiMainC, # List: info on start and end of the simulation.
               biols.ctrl  = multiBioC,  # List: model to simulate the stock dynamics.
               fleets.ctrl = multiFlC,   # List: fleet dyn. models and other parameters.
               covars.ctrl = multiCvC,   # List: covariates dynamics ("fixedCovar").
               obs.ctrl    = multiObsC,  # List: type of stock and index observation
                                         #       ("PerfectObs","NoObsIndex").
               assess.ctrl = multiAssC,  # List: assessment model used ("NoAssessment").
               advice.ctrl = multiAdvC)  # List: rule for TAC advice ("IcesHCR").

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
Blim.stk1 <- 800
Bpa.stk1  <- 1200
Blim.stk2 <- 50000
Bpa.stk2  <- 65000

## ----echo=TRUE, eval=TRUE, fig.width = 8, fig.height = 5-----------------
# - stock summary
s40_bio <- rbind( bioSum(s4, scenario='closeNO'), 
                  bioSum(s4a, scenario='closeNorth'), 
                  bioSum(s4a, scenario='closeSson2')) # biological indicators
plotbioSum( s40_bio, stk.nam='stk1', Blim=Blim.stk1, Bpa=Bpa.stk1, proj.yr=multiMainC$sim.years[['initial']])
plotbioSum( s40_bio, stk.nam='stk2', Blim=Blim.stk2, Bpa=Bpa.stk2, proj.yr=multiMainC$sim.years[['initial']])

## ----echo=TRUE, eval=TRUE, fig.width = 8, fig.height = 5-----------------
# - economic summary at metier level
s40_mt <- rbind( mtSum(s4, scenario='closeNO'), 
                 mtSum(s4a, scenario='closeNorth'), 
                 mtSum(s4b, scenario='closeSson2')) # indicators at fleet level
s40_mt$year <- as.numeric(s40_mt$year)
s40_mt$flmt <- paste( s40_mt$fleet, s40_mt$metier, sep='_')
p <- ggplot( data=s40_mt, aes(x=year, y=value, color=scenario)) + 
  geom_line() +
  facet_grid(indicator ~ flmt, scales="free") +
  geom_vline(xintercept = multiMainC$sim.years[['initial']]-1, linetype = "longdash")+
  theme_bw()+
  theme(text=element_text(size=10),
        title=element_text(size=10,face="bold"),
        strip.text=element_text(size=10))
print(p)

## ----echo=TRUE, eval=TRUE, fig.width = 7, fig.height = 4-----------------
# - risk summary
s4_risk  <- riskSum(  s4, Bpa = c(stk1=Bpa.stk1,stk2=Bpa.stk2), Blim = c(stk1=Blim.stk1,stk2=Blim.stk2), 
                     Prflim = c(fl1=0,fl2=0), scenario = 'closeNO')
s4a_risk <- riskSum( s4a, Bpa = c(stk1=Bpa.stk1,stk2=Bpa.stk2), Blim = c(stk1=Blim.stk1,stk2=Blim.stk2), 
                     Prflim = c(fl1=0,fl2=0), scenario='closeNorth')
s4b_risk <- riskSum( s4b, Bpa = c(stk1=Bpa.stk1,stk2=Bpa.stk2), Blim = c(stk1=Blim.stk1,stk2=Blim.stk2), 
                     Prflim = c(fl1=0,fl2=0), scenario='closeSson2')
s40_risk <- rbind( s4_risk, s4a_risk, s4b_risk) # risk indicators
s40_risk$year <- as.numeric(s40_risk$year)
p <- ggplot( data=subset(s40_risk,indicator=='pPrflim'), aes(x=year, y=value, color=scenario)) + 
  geom_line() +
  facet_grid(indicator ~ unit, scales="free") +
  geom_vline(xintercept = multiMainC$sim.years[['initial']]-1, linetype = "longdash") +
  theme_bw()+
  theme(text=element_text(size=15),
        title=element_text(size=15,face="bold"),
        strip.text=element_text(size=15)) +
  ylab("Risk")
print(p)
p <- ggplot( data=subset(s40_risk,indicator!='pPrflim'), aes(x=year, y=value, color=scenario)) + 
  geom_line() +
  facet_grid(indicator ~ unit, scales="free") +
  geom_vline(xintercept = multiMainC$sim.years[['initial']]-1, linetype = "longdash") +
  theme_bw()+
  theme(text=element_text(size=15),
        title=element_text(size=15,face="bold"),
        strip.text=element_text(size=15)) +
  ylab("Risk")
print(p)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## 
## # EXERCISE 1
## ##############
## 
## # Set the TAC value to 300 tons
## 
## # Set the control parameters related to advice to 'fixedAdvice'
## oneItAdvC3 <- list()
## oneItAdvC3$stk1 <- list()
## oneItAdvC3$stk1$HCR.model <- 'fixedAdvice'     # selected HCR
## 
## # Set the TAC in the projection year to 300 tons
## oneItAdv3 <- oneItAdv
## proj.yrs <- dimnames(oneItAdv3$TAC)$year[as.numeric(dimnames(oneItAdv3$TAC)$year)>=
##                                           oneItMainC$sim.years[['initial']]]
## oneItAdv3$TAC[,proj.yrs,] <- 300
## 
## ## Run FLBEIA
## # Fixed advice (fTAC): TAC fixed at 300 tons
## s1b <- FLBEIA( biols       = oneItBio,     # FLBiols: FLBiol for stk1.
##                SRs         = oneItSR,      # List: FLSRSim for stk1.
##                BDs         = NULL,         # Not population with biomass dynamics.
##                fleets      = oneItFl,      # FLFleets: one fleet.
##                covars      = oneItCv,      # List: covars related to economy.
##                indices     = oneItInd,     # Not indices.
##                advice      = oneItAdv3,    # List: 'TAC' and 'quota.share'
##                main.ctrl   = oneItMainC,   # List: info on start and end of the simulation.
##                biols.ctrl  = oneItBioC,    # List: model to simulate the stock dynamics.
##                fleets.ctrl = oneItFlC,     # List: fleet dyn. models and other parameters.
##                covars.ctrl = oneItCvC,     # List: covariates dynamics ("fixedCovar").
##                obs.ctrl    = oneItObsCInd, # List: type of stock and index observation
##                                            #       ("NoObsStock","bioInd").
##                assess.ctrl = oneItAssC,    # List: assessment model used ("NoAssessment").
##                advice.ctrl = oneItAdvC3)   # List: rule for TAC advice ("AnnexIV").
## 
## ## Results: comparison with previous scenarios
## 
## # - stock summary
## s10_bio <- rbind( s10_bio,
##                   bioSum(s1b, scenario='fTAC')) # biological indicators
## plotbioSum( s10_bio, Blim=Blim.stk1, Bpa=Bpa.stk1, proj.yr=oneItMainC$sim.years[['initial']])
## 
## # - economic summary
## s10_flt <- rbind( s10_flt,
##                   fltSum(s1b, scenario='fTAC')) # indicators at fleet level
## plotfltSum( s10_flt, proj.yr=oneItMainC$sim.years[['initial']])
## 
## # - risk summary
## s1b_risk <- riskSum( s1b, Bpa = c(stk1=Bpa.stk1), Blim = c(stk1=Blim.stk1), Prflim = c(fl1 = 0),
##                      scenario='fTAC')
## s10_risk <- rbind( s10_risk, s1b_risk) # risk indicators
## s10_risk$year <- as.numeric(s10_risk$year)
## p <- ggplot( data=s10_risk, aes(x=year, y=value, color=scenario)) +
##   geom_line() +
##   facet_wrap(~indicator, scales="free") +
##   facet_grid(indicator ~ .) +
##   geom_vline(xintercept = oneItMainC$sim.years[['initial']]-1, linetype = "longdash")+
##   theme_bw()+
##   theme(text=element_text(size=15),
##         title=element_text(size=15,face="bold"),
##         strip.text=element_text(size=15))+
##   ylab("Risk")
## print(p)
## 
## 
## 
## # EXERCISE 2
## ##############
## 
## # Fleet dynamics: effort model
## oneItFlC$fl1$effort.model  # fixed effort
## 
## # Set the effort values to fit the condition required
## # (i.e. gradual reduction to 50% reduction of the effort in the last proj. year)
## 
## # - Generate a new FLFleets object
## oneItFl2b <- oneItFl
## 
## # - Store values of simulation and historic years
## yrs <- dimnames(oneItFl[[1]]@effort)$year
## sim.yrs  <- as.character(oneItMainC$sim.years['initial']:oneItMainC$sim.years['final'])
## hist.yrs <- yrs[!yrs %in% sim.yrs]
## 
## # - Change the effort for the simulation years:
## eff.yred <- 0.5/length(sim.yrs)
## i <- 0
## for (yr in sim.yrs) {
##   i <- i + 1
##   rf <- 1 - eff.yred * i
##   oneItFl2b[[1]]@effort[,yr,] <- rf * oneItFl2b[[1]]@effort[,hist.yrs[length(hist.yrs)],]
## }
## 
## # - check if correct
## oneItFl2b[[1]]@effort[,sim.yrs[length(sim.yrs)],]/oneItFl2b[[1]]@effort[,hist.yrs[length(hist.yrs)],]
## 
## ## Run FLBEIA
## # Gradual reduction of effort (E50g): gradual reduction for 50% reduction in last proj. yr
## #                                      (rel. to last 3yrs mean)
## s2b <- FLBEIA( biols       = oneItBio,   # FLBiols: FLBiol for stk1.
##                SRs         = oneItSR,    # List: FLSRSim for stk1.
##                BDs         = NULL,       # Not population with biomass dynamics.
##                fleets      = oneItFl2b,  # FLFleets: one fleet.
##                covars      = oneItCv,    # List: covars related to economy.
##                indices     = NULL,       # Not indices.
##                advice      = oneItAdv,   # List: 'TAC' and 'quota.share'
##                main.ctrl   = oneItMainC, # List: info on start and end of the simulation.
##                biols.ctrl  = oneItBioC,  # List: model to simulate the stock dynamics.
##                fleets.ctrl = oneItFlC,   # List: fleet dyn. models and other parameters.
##                covars.ctrl = oneItCvC,   # List: covariates dynamics ("fixedCovar").
##                obs.ctrl    = oneItObsC,  # List: type of stock and index observation
##                                          #       ("PerfectObs","NoObsIndex").
##                assess.ctrl = oneItAssC,  # List: assessment model used ("NoAssessment").
##                advice.ctrl = oneItAdvC)  # List: rule for TAC advice ("IcesHCR").
## 
## ## Results: comparison with previous scenarios
## 
## # - stock summary
## s20_bio <- rbind( s20_bio,
##                   bioSum(s2b, scenario='E50g')) # biological indicators
## plotbioSum( s20_bio, Blim=Blim.stk1, Bpa=Bpa.stk1, proj.yr=oneItMainC$sim.years[['initial']])
## 
## # - economic summary
## s20_flt <- rbind( s20_flt,
##                   fltSum(s2b, scenario='E50g')) # indicators at fleet level
## plotfltSum( s20_flt, proj.yr=oneItMainC$sim.years[['initial']])
## 
## # - risk summary
## s2b_risk <- riskSum( s2b, Bpa = c(stk1=Bpa.stk1), Blim = c(stk1=Blim.stk1), Prflim = c(fl1 = 0),
##                      scenario='E50g')
## s20_risk <- rbind( s20_risk, s2b_risk) # risk indicators
## s20_risk$year <- as.numeric(s20_risk$year)
## p <- ggplot( data=s20_risk, aes(x=year, y=value, color=scenario)) +
##   geom_line() +
##   facet_wrap(~indicator, scales="free") +
##   facet_grid(indicator ~ .) +
##   geom_vline(xintercept = oneItMainC$sim.years[['initial']]-1, linetype = "longdash")+
##   theme_bw()+
##   theme(text=element_text(size=15),
##         title=element_text(size=15,face="bold"),
##         strip.text=element_text(size=15))+
##   ylab("Risk")
## print(p)
## 
## 
## 
## # EXERCISE 3
## ##############
## 
## # Define the FLFleetsExt object for the alternative scenarios
## 
## # - Check simulation and historic years
## yrs <- dimnames(oneItFl[[1]]@effort)$year
## sim.yrs  <- as.character(oneItMainC$sim.years['initial']:oneItMainC$sim.years['final'])
## hist.yrs <- yrs[!yrs %in% sim.yrs]
## 
## # - Set the catchability at age:
## #   10% increase of catchability at ages 4-10
## oneItFl3b <- oneItFl
## oneItFl3b[['fl1']]@metiers[['met1']]@catches[['stk1']]@catch.q[4:10,sim.yrs,] <-
##   1.1 * oneItFl[['fl1']]@metiers[['met1']]@catches[['stk1']]@catch.q[4:10,sim.yrs,]
## 
## # - Set the selectivity at age:
## #   50% of the catch is discarded at ages 1-2
## #   25% of the catch is discarded at age 3
## oneItFl3b[['fl1']]@metiers[['met1']]@catches[['stk1']]@landings.sel # 100% catch : landing
## oneItFl3b[['fl1']]@metiers[['met1']]@catches[['stk1']]@discards.sel #   0% catch : discard
## oneItFl3b[['fl1']]@metiers[['met1']]@catches[['stk1']]@landings.sel[1:2,sim.yrs,] <- 0.5
## oneItFl3b[['fl1']]@metiers[['met1']]@catches[['stk1']]@discards.sel[1:2,sim.yrs,] <- 0.5
## oneItFl3b[['fl1']]@metiers[['met1']]@catches[['stk1']]@landings.sel[3,sim.yrs,] <- 0.75
## oneItFl3b[['fl1']]@metiers[['met1']]@catches[['stk1']]@discards.sel[3,sim.yrs,] <- 0.25
## 
## # Comparison plot for selectivities:
## d3  <- as.data.frame(apply(oneItFl[['fl1']]@metiers[['met1']]@catches[['stk1']]@catch.q[,sim.yrs[1],,],
##                           1:5,quantile,0.5))
## d3a <- as.data.frame(apply(oneItFl3a[['fl1']]@metiers[['met1']]@catches[['stk1']]@catch.q[,sim.yrs[1],,],
##                           1:5,quantile,0.5))
## d3b <- as.data.frame(apply(oneItFl3b[['fl1']]@metiers[['met1']]@catches[['stk1']]@catch.q[,sim.yrs[1],,],
##                           1:5,quantile,0.5))
## d30_q <- rbind( data.frame(d3,case='bc'), data.frame(d3a,case='netBig'), data.frame(d3b,case='netSmall'))
## p <- ggplot( data=d30_q, aes(x=age, y=data, color=case)) +
##   geom_line() +
##   theme_bw() +
##   theme(text=element_text(size=15),
##         title=element_text(size=15,face="bold"),
##         strip.text=element_text(size=15)) +
##   scale_x_continuous(breaks=unique(d30_q$age)) +
##   ylab("catchability")
## print(p)
## 
## ## Run FLBEIA
## # Net size decrease (netSmall): 10% increase of selectivity for ages 4-10 relative to the mean 2006-2008
## #                               + 50% ages 1-2 catch discarded and 25% of age 3
## s3b <- FLBEIA( biols       = oneItBio,   # FLBiols: FLBiol for stk1.
##                SRs         = oneItSR,    # List: FLSRSim for stk1.
##                BDs         = NULL,       # Not population with biomass dynamics.
##                fleets      = oneItFl3b,  # FLFleets: one fleet.
##                covars      = oneItCv,    # List: covars related to economy.
##                indices     = NULL,       # Not indices.
##                advice      = oneItAdv,   # List: 'TAC' and 'quota.share'
##                main.ctrl   = oneItMainC, # List: info on start and end of the simulation.
##                biols.ctrl  = oneItBioC,  # List: model to simulate the stock dynamics.
##                fleets.ctrl = oneItFlC,   # List: fleet dyn. models and other parameters.
##                covars.ctrl = oneItCvC,   # List: covariates dynamics ("fixedCovar").
##                obs.ctrl    = oneItObsC,  # List: type of stock and index observation
##                                          #       ("PerfectObs","NoObsIndex").
##                assess.ctrl = oneItAssC,  # List: assessment model used ("NoAssessment").
##                advice.ctrl = oneItAdvC)  # List: rule for TAC advice ("IcesHCR").
## 
## 
## ## Results: comparison with previous scenarios
## 
## # - stock summary
## s30_bio <- rbind( s30_bio,
##                   bioSum(s3b, scenario='netSmall')) # biological indicators
## plotbioSum( s30_bio, Blim=Blim.stk1, Bpa=Bpa.stk1, proj.yr=oneItMainC$sim.years[['initial']])
## 
## # - economic summary
## s30_flt <- rbind( s30_flt,
##                   fltSum(s3b, scenario='netSmall')) # indicators at fleet level
## plotfltSum( s30_flt, proj.yr=oneItMainC$sim.years[['initial']])
## 
## # - risk summary
## s3b_risk <- riskSum( s3b, Bpa = c(stk1=Bpa.stk1), Blim = c(stk1=Blim.stk1), Prflim = c(fl1 = 0),
##                      scenario='netSmall')
## s30_risk <- rbind( s30_risk, s3b_risk) # risk indicators
## s30_risk$year <- as.numeric(s30_risk$year)
## p <- ggplot( data=s30_risk, aes(x=year, y=value, color=scenario)) +
##   geom_line() +
##   facet_wrap(~indicator, scales="free") +
##   facet_grid(indicator ~ .) +
##   geom_vline(xintercept = oneItMainC$sim.years[['initial']]-1, linetype = "longdash")+
##   theme_bw()+
##   theme(text=element_text(size=15),
##         title=element_text(size=15,face="bold"),
##         strip.text=element_text(size=15))+
##   ylab("Risk")
## print(p)
## 
## # - risk
## s30_risk <- rbind( s30_risk, s3b_risk)
## s30_risk$year <- as.numeric(s30_risk$year)
## p <- ggplot( data=s30_risk, aes(x=year, y=value, color=scenario)) +
##   geom_line() +
##   # geom_ribbon(aes(x=year, ymin=q05, ymax=q95, fill=scenario), alpha=0.5) +
##   facet_wrap(~indicator, scales="free") +
##   facet_grid(indicator ~ .) +
##   geom_vline(xintercept = oneItMainC$sim.years[['initial']]-1, linetype = "longdash")+
##   theme_bw()+
##   theme(text=element_text(size=15),
##         title=element_text(size=15,face="bold"),
##         strip.text=element_text(size=15))+
##   ylab("Risk")
## print(p)

