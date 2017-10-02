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
## install.packages(c("ggplot2"))
## install.packages(c("FLCore", "FLBEIA", "FLFleets", "FLash", "FLAssess", "FLXSA", "ggplotFL"), repos="http://flr-project.org/R")

## ---- pkgs, results = "hide"---------------------------------------------
  library(FLCore) 
  library(FLAssess)
  library(FLash)
  library(FLFleet)
  library(FLXSA)
  library(FLBEIA) 
  library(ggplot2)

## ----echo=TRUE, eval=TRUE------------------------------------------------
data(one)
ls()

## ----echo=TRUE, eval=TRUE------------------------------------------------
# Set Simulation parameters related with time

  first.yr          <- 1990
  proj.yr           <- 2010 
  last.yr           <- 2025  
  yrs <- c(first.yr=first.yr,proj.yr=proj.yr,last.yr=last.yr)
  
#  Set names, age, dimensions 

  fls   <- c('fl1')

  stks <- c('stk1')

  fl1.mets      <- c('met1')
  fl1.met1.stks  <- c('stk1')

  # all stocks the same
  
  ni           <- 1
  it           <- 1:ni
  ns             <- 1
  
  # stock stk1
  stk1.age.min    <- 1
  stk1.age.max    <- 12
  stk1.unit       <- 1         
 
#  Data: stk1_n.flq, m, spwn, fec, wt

  #stock stk1
  stk1_n.flq     <- iter(as.FLQuant(read.csv(file = 'data/stk1_n.csv')),it)
  stk1_m.flq     <- iter(as.FLQuant(read.csv(file = 'data/stk1_m.csv')),it)
  stk1_spwn.flq  <- iter(as.FLQuant(read.csv(file = 'data/stk1_spwn.csv')),it)
  stk1_fec.flq   <- iter(as.FLQuant(read.csv(file = 'data/stk1_fec.csv')),it)
  stk1_wt.flq    <- iter(as.FLQuant(read.csv(file = 'data/stk1_wt.csv')),it)
  stk1_mat.flq   <- stk1_fec.flq
  stk1_mat.flq[] <- 1
  
  stk1_range.min       <- 1
  stk1_range.max       <- 12
  stk1_range.plusgroup <- 12
  stk1_range.minyear   <- 1990
  stk1_range.minfbar   <- 4
  stk1_range.maxfbar   <- 9

# Projection biols: weight,fecundity,mortality and spawning 

  stk1_biol.proj.avg.yrs  <- c(2007:2009)
  
# Create the object
  
   stks.data <- list(stk1=ls(pattern="^stk1")) 

    biols    <- create.biols.data(yrs,ns,ni,stks.data)
  plotFLBiols(biols,pdfnm='s0')


## ----echo=TRUE, eval=TRUE------------------------------------------------

  stk1_sr.model        <- 'bevholtAR1'
  stk1_params.n        <- 3
  stk1_params.array    <- xtabs2(data~param+year+season+iter, 
                                data=read.csv(file = 'data/stk1_params.csv'),
                                exclude=NULL,na.action=na.pass)[,,,it,drop=F]         
  stk1_params.name     <- c('a','b','c') 
  stk1_rec.flq         <- iter(as.FLQuant(read.csv(file = 'data/stk1_rec.csv')),it)
  stk1_ssb.flq         <- iter(as.FLQuant(read.csv(file = 'data/stk1_ssb.csv')),it)
  stk1_uncertainty.flq <- iter(as.FLQuant(read.csv(file = 'data/stk1_uncertainty.csv')),it)
  stk1_proportion.flq  <- iter(as.FLQuant(read.csv(file = 'data/stk1_proportion.csv')),it)
  stk1_prop.avg.yrs    <- ac(2006:2008)
  stk1_timelag.matrix  <- matrix(c(1,1),nrow=2,ncol=1, dimnames = list(c('year', 'season'),'all'))

    #              FLBEIA input object: SRs

    stks.data <- list(stk1=ls(pattern="^stk1")) 
  
  SRs      <- create.SRs.data(yrs,ns,ni,stks.data)

## ----echo=TRUE, eval=TRUE------------------------------------------------
# Data per fleet
#    effort, crewshare, fcost, capacity
# Data per fleet and metier
#    effshare, vcost
# Data per fleet, metier and stock
#    landings.n, discards.n,landings.wt, discards.wt, landings, discards, landings.sel, discards.sel, price

  fl1_effort.flq        <- iter(as.FLQuant(read.csv(file = 'data/fl1_effort.csv')),it)
  fl1_capacity.flq      <- iter(as.FLQuant(read.csv(file = 'data/fl1_capacity.csv')),it)
  fl1_fcost.flq         <- iter(as.FLQuant(read.csv(file = 'data/fl1_fcost.csv')),it)
  fl1_crewshare.flq     <- iter(as.FLQuant(read.csv(file = 'data/fl1_crewshare.csv')),it)

  fl1.met1_effshare.flq  <- iter(as.FLQuant(read.csv(file = 'data/fl1.met1_effshare.csv')),it)
          
  fl1.met1.stk1_landings.n.flq <- iter(as.FLQuant(read.csv(file = 'data/fl1.met1.stk1_landings.n.csv')),it)
  fl1.met1.stk1_discards.n.flq <- iter(as.FLQuant(read.csv(file = 'data/fl1.met1.stk1_discards.n.csv')),it)
#  fl1.met1.stk1_alpha.flq      <- iter(as.FLQuant(read.csv(file = 'data/fl1.met1.stk1_alpha.csv'))  ,it)
#  fl1.met1.stk1_beta.flq       <- iter(as.FLQuant(read.csv(file = 'data/fl1.met1.stk1_beta.csv'))  ,it)
#  fl1.met1.stk1_catch.q.flq    <- iter(as.FLQuant(read.csv(file = 'data/fl1.met1.stk1_catch.q.csv'))  ,it)
    
    # Projection
    #         fleets: fl1
  fl1_proj.avg.yrs           <- c(2008:2009)
  fl1.met1_proj.avg.yrs       <- c(2008:2009)   
  fl1.met1.stk1_proj.avg.yrs   <- c(2006:2008)   

    #              create fleets object
  
  fls.data <- list(fl1=ls(pattern="^fl1")) 
  
  fleets   <- create.fleets.data(yrs,ns,ni,fls.data,stks.data)
  
  plotFLFleets(fleets,pdfnm='s0')   
  

## ----echo=TRUE, eval=TRUE------------------------------------------------
#  advice:TAC/TAE/quota.share

  stk1_advice.TAC.flq         <- iter(as.FLQuant(read.csv(file = 'data/stk1_advice.tac.csv')),it)
  stk1_advice.quota.share.flq <- iter(as.FLQuant(read.csv(file = 'data/stk1_advice.quota.share.csv')),it)
  stk1_advice.avg.yrs         <- c(2006:2008)

#   create advice object
  stks.data <- list(stk1=ls(pattern="^stk1")) 
  advice   <- create.advice.data(yrs,ns,ni,stks.data,fleets)

## ----echo=TRUE, eval=TRUE------------------------------------------------
indices <- NULL

## ----echo=TRUE, eval=TRUE------------------------------------------------
  main.ctrl           <- list()
  main.ctrl$sim.years <- c(initial = proj.yr, final = last.yr)

## ----echo=TRUE, eval=TRUE------------------------------------------------
  growth.model     <- c('ASPG')
  biols.ctrl       <- create.biols.ctrl (stksnames=stks,growth.model=growth.model)

## ----echo=TRUE, eval=TRUE------------------------------------------------
  n.fls.stks      <- 1
  fls.stksnames   <- 'stk1'
  effort.models    <- 'SMFB'
  effort.restr.fl1 <- 'stk1'
  restriction.fl1  <- 'catch'
  catch.models     <- 'CobbDouglasAge'
  capital.models   <- 'fixedCapital'       
  flq.stk1<- FLQuant(dimnames = list(age = 'all', year = first.yr:last.yr, unit = stk1.unit, 
                                     season = 1:ns, iter = 1:ni)) 
  fleets.ctrl      <- create.fleets.ctrl(fls=fls,n.fls.stks=n.fls.stks,fls.stksnames=fls.stksnames,
                                         effort.models= effort.models, catch.models=catch.models,
                                         capital.models=capital.models, flq=flq.stk1,
                                         effort.restr.fl1 = effort.restr.fl1, restriction.fl1 = restriction.fl1)

fleets.ctrl$fl1$stk1$discard.TAC.OS  <- FALSE
fleets.ctrl$fl1$restriction <- "landings"

## ----echo=TRUE, eval=TRUE------------------------------------------------
  HCR.models       <- c('IcesHCR') 
  ref.pts.stk1      <- matrix(rep(c(548.6296271, 768.0814779, 0.1057783),3), 3,ni, dimnames = list(c('Blim', 'Btrigger','Fmsy'), 1:ni))
  advice.ctrl      <- create.advice.ctrl(stksnames = stks, HCR.models =  HCR.models, 
                      ref.pts.stk1 = ref.pts.stk1,first.yr=first.yr,last.yr=last.yr)
    
  advice.ctrl[['stk1']][['sr']]            <- list()
  advice.ctrl[['stk1']][['sr']][['model']] <- 'geomean'
  advice.ctrl[['stk1']][['sr']][['years']] <- c(y.rm = 2, num.years = 10)
  advice.ctrl$stk1$AdvCatch <- rep(TRUE,length(first.yr:last.yr))   #TRUE advice in catches, FALSE advice in landings
  names(advice.ctrl$stk1$AdvCatch) <- as.character((first.yr:last.yr))

## ----echo=TRUE, eval=TRUE------------------------------------------------
assess.models    <- 'NoAssessment'
assess.ctrl <- create.assess.ctrl(stksnames = stks, assess.models = assess.models)
assess.ctrl[['stk1']]$work_w_Iter   <- TRUE

## ----echo=TRUE, eval=TRUE------------------------------------------------
stkObs.models<- "perfectObs"
flq.stk1<- FLQuant(dimnames = list(age = 'all', year = first.yr:last.yr, unit = stk1.unit, 
                                   season = 1:ns, iter = 1:ni)) 

obs.ctrl        <- create.obs.ctrl(stksnames = stks,  stkObs.models = stkObs.models,flq.stk1 = flq.stk1)

## ----echo=TRUE, eval=TRUE------------------------------------------------
  covars.ctrl      <- NULL
  BDs       <- NULL
  covars    <- NULL

## ----echo=TRUE, eval=TRUE------------------------------------------------
  save(biols, SRs,BDs, fleets, covars, 
     indices, advice, main.ctrl, 
     biols.ctrl, fleets.ctrl, 
     covars.ctrl, obs.ctrl, 
     assess.ctrl, advice.ctrl, file="input_S0_1it.RData")

## ----echo=TRUE, eval=TRUE------------------------------------------------
s0 <- FLBEIA(biols = biols, SRs = SRs, BDs = BDs, fleets=fleets, covars = covars, 
              indices = indices, advice = advice, main.ctrl = main.ctrl, 
              biols.ctrl = biols.ctrl, fleets.ctrl = fleets.ctrl, 
              covars.ctrl = covars.ctrl, obs.ctrl = obs.ctrl, 
              assess.ctrl = assess.ctrl, advice.ctrl = advice.ctrl) 


## ----echo=TRUE, eval=TRUE------------------------------------------------
    names(s0)
  plotFLBiols(s0$biols,pdfnm='s0_res')
  plotFLFleets(s0$fleets,pdfnm='s0_res')   

