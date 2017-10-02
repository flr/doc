## ---- ini, echo=FALSE, results='hide', message=FALSE---------------------
# This chunk set the document environment, so it is hidden
library(knitr)
source("R/ini.R")
knitr::opts_chunk$set(fig.align='center',
  message=FALSE, warning=FALSE, echo=TRUE, cache=FALSE)
options(width=50)
set.seed(1423)

## ----echo=FALSE, out.width='20%'-----------------------------------------
include_graphics('images/FLBEIA_logo.png')

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("ggplot2"))
## install.packages(c("FLCore", "FLBEIA", "FLFleets", "FLash", "FLAssess", "FLXSA", "ggplotFL"), repos="http://flr-project.org/R")

## ---- pkgs, results = "hide"---------------------------------------------
# This chunk loads all necessary packages.
library(FLBEIA)
library(FLXSA)
library(FLash)
library(ggplotFL)
#library(FLBEIAShiny) # This is a beta version

## ----echo=TRUE, eval=TRUE------------------------------------------------
data(one) 

## ----echo=TRUE, eval=TRUE------------------------------------------------
ls()
# Show the class of each of the objects.
sapply(ls(), function(x) class(get(x)))

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
s0 <- FLBEIA(biols = oneBio,       # FLBiols object with one FLBiol element for stk1.
               SRs = oneSR,        # A list with one FLSRSim object for stk1.
               BDs = NULL,         # No Biomass dynamics populations in this case.
            fleets = oneFl,        # FLFleets object with one fleet.
            covars = oneCv,        # Covar is and object with aditional data on fleet (number of vessels, etc).
           indices = NULL,         # Indices not used 
            advice = oneAdv,       # A list with two elements 'TAC' and 'quota.share'
         main.ctrl = oneMainC,     # A list with one element to define the start and end of the simulation.
        biols.ctrl = oneBioC,      # A list with one element to select the model to simulate the stock dynamics.
       fleets.ctrl = oneFlC,       # A list with several elements to select fleet dynamic models and store additional parameters.
       covars.ctrl = oneCvC,       # A list with several data related to the fleet.
          obs.ctrl = oneObsC,      # A list with one element to define how the stock observed ("PerfectObs").
       assess.ctrl = oneAssC,      # A list with one element to define how the stock assessment model used ("NoAssessment").
       advice.ctrl = oneAdvC)      # A list with one element to define how the TAC advice is obtained ("IcesHCR").

## ----echo=TRUE, eval=TRUE------------------------------------------------
names(s0)
sapply(s0, function(x) class(x))

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
s0_bio    <- bioSum(s0)           # Data frame (DF) of biological indicators.
s0_adv    <- advSum(s0)           # DF of management advice (TAC). 
s0_flt    <- fltSum(s0)           # DF of economic indicators at fleet level.
s0_fltStk <- fltStkSum(s0)        # DF of indicators at fleet and stock level.
s0_mt     <- mtSum(s0)            # DF of indicators at fleet.
s0_mtStk  <- mtStkSum(s0)         # DF of indicators at fleet and metier level.
s0_vessel <- vesselSum(s0)        # DF of indicators at vessel level.
s0_vesselStk <- vesselStkSum(s0)  # DF of indicators at vessel and stock level.
s0_npv  <- npv(s0, y0 = '2014')   # DF of net present value per fleet over the selected range of years.
s0_risk <- riskSum(s0, Bpa = c(stk1= 135000), Blim = c(stk1= 96000), Prflim = c(fl1 = 0))

# Exploring data frames
head(s0_bio); unique(s0_bio$indicator)
head(s0_adv); unique(s0_adv$indicator)
head(s0_flt); unique(s0_flt$indicator)
head(s0_fltStk); unique(s0_fltStk$indicator)
head(s0_mt); unique(s0_mt$indicator)
head(s0_mtStk); unique(s0_mtStk$indicator)
head(s0_vessel); unique(s0_vessel$indicator)
head(s0_vesselStk); unique(s0_vesselStk$indicator)
head(s0_risk); unique(s0_risk$indicator)

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
s0_bio_l    <- bioSum(s0, long = FALSE, years = ac(2016:2020))            
s0_adv_l    <- advSum(s0, long = FALSE, years = ac(2016:2020))             
s0_flt_l    <- fltSum(s0, long = FALSE, years = ac(2016:2020))
s0_fltStk_l <- fltStkSum(s0, long = FALSE, years = ac(2016:2020))          
s0_mt_l     <- mtSum(s0, long = FALSE, years = ac(2016:2020))             
s0_mtStk_l  <- mtStkSum(s0, long = FALSE, years = ac(2016:2020))           
s0_vessel_l <- vesselSum(s0, long = FALSE, years = ac(2016:2020))
s0_vesselStk_l <- vesselStkSum(s0, long = FALSE, years = ac(2016:2020))       

# Exploring data frames
head(s0_bio_l, 2)
head(s0_adv_l, 2)
head(s0_flt_l, 2)
head(s0_fltStk_l, 2)
head(s0_mt_l, 2)
head(s0_mtStk_l, 2)
head(s0_vessel_l, 2)
head(s0_vesselStk_l, 2)

## ----echo=TRUE, fig.width = 4, fig.height = 4, eval=TRUE-----------------
plot(s0$biols[[1]])

## ----echo=TRUE, fig.width = 4, fig.height = 4, eval=TRUE-----------------
plot(s0$stocks[[1]])

## ----echo=TRUE, eval=TRUE, results= 'hide'-------------------------------
# set your own working directory.
# myWD <- "My working directory"
# setwd(myWD)
plotFLBiols(s0$biols, pdfnm="s0")
plotFLFleets(s0$fleets, pdfnm="s0")
plotEco(s0, pdfnm="s0")
plotfltStkSum(s0, pdfnm="s0")

## ----echo=TRUE,  fig.width = 4, fig.height = 2, eval=TRUE----------------
aux <- subset(s0_bio, indicator=="catch" )
p <- ggplot(data=aux, aes(x=year, y=value, color=stock))+
  geom_line()+
  geom_vline(xintercept = 2016, linetype = "longdash")+
  theme_bw()+
  theme(text=element_text(size=10),
        title=element_text(size=10,face="bold"),
        strip.text=element_text(size=10))+
  ylab("Catch (t)")
print(p)

## ----echo=TRUE, eval=TRUE------------------------------------------------
  first.yr          <- 1990
  proj.yr           <- 2010 
  last.yr           <- 2025  
  yrs <- c(first.yr=first.yr,proj.yr=proj.yr,last.yr=last.yr)

  stks <- c('stk1')

## ----echo=TRUE, eval=TRUE------------------------------------------------
oneSR$stk1@model

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
ssb <-  ssb(oneBio[[1]])[,as.character(first.yr:(proj.yr-1)),1,1]
rec <-  oneBio[[1]]@n[1,as.character(first.yr:(proj.yr-1)),]
sr.segreg <- fmle(FLSR(model="segreg",ssb = ssb, rec = rec))

# Introduce the new model and its parameters in SR object.
SRs.segreg <- oneSR
SRs.segreg[[1]]@params[,,,] <- sr.segreg@params[,] 
SRs.segreg[[1]]@model <- 'segreg'

#Run FLBEIA with the new SR function.
s1 <- FLBEIA(biols = oneBio, SRs = SRs.segreg , BDs = NULL, fleets = oneFl, covars = oneCv,         
            indices = NULL, advice = oneAdv, main.ctrl = oneMainC, biols.ctrl = oneBioC,      
            fleets.ctrl = oneFlC, covars.ctrl = oneCvC, obs.ctrl = oneObsC, 
            assess.ctrl = oneAssC, advice.ctrl = oneAdvC)    

plotFLBiols(s1$biols, pdfnm='s1')
plotFLFleets(s1$fleets, pdfnm='s1')  

## ----echo=TRUE,  fig.width = 3.5, fig.height = 3.5, eval=TRUE------------
temp <- cbind(matrix(B_flbeia(s0)), matrix(B_flbeia(s1)))
matplot(temp, x = dimnames( B_flbeia(s1))$year, type = 'l', xlab = 'Year', ylab = 'Biomass')
legend('top', c('s0', 's1'), col = c('black','red'), lty = c(1,2))

## ----echo=TRUE, eval=TRUE,  results = "hide"-----------------------------
oneBioM <- oneBio
oneBioM$stk1@m[7:12,,,,] <- oneBioM$stk1@m[7:12,,,,]*1.2

s2 <- FLBEIA(biols = oneBioM, SRs = oneSR , BDs = NULL, fleets = oneFl, covars = oneCv,         
            indices = NULL, advice = oneAdv, main.ctrl = oneMainC, biols.ctrl = oneBioC,      
            fleets.ctrl = oneFlC, covars.ctrl = oneCvC, obs.ctrl = oneObsC, 
            assess.ctrl = oneAssC, advice.ctrl = oneAdvC) 

plotFLBiols(s1$biols, pdfnm='s2')
plotFLFleets(s1$fleets, pdfnm='s2')  

## ----echo=TRUE,  fig.width = 3.5, fig.height = 3.5, eval=TRUE------------
temp <- cbind(matrix(B_flbeia(s0)), matrix(B_flbeia(s2)))
matplot(temp, x = dimnames( B_flbeia(s0))$year, type = 'l', xlab = 'Year', ylab = 'Biomass')
legend('top', c('s0', 's2'), col = c('black','red'), lty = c(1,2))

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
oneAdvC$stk1$ref.pts[3]
oneAdvC2 <- oneAdvC
oneAdvC2$stk1$ref.pts[3] <- oneAdvC$stk1$ref.pts[3]*0.8
s3 <- FLBEIA(biols = oneBio, SRs = oneSR , BDs = NULL, fleets = oneFl, covars = oneCv,         
            indices = NULL, advice = oneAdv, main.ctrl = oneMainC, biols.ctrl = oneBioC,      
            fleets.ctrl = oneFlC, covars.ctrl = oneCvC, obs.ctrl = oneObsC,
            assess.ctrl = oneAssC, advice.ctrl = oneAdvC2)

## ----echo=TRUE, eval=TRUE,  fig.width = 3.5, fig.height = 3.5, results = "hide"----
temp <- cbind(matrix(B_flbeia(s0)), matrix(B_flbeia(s3)))
matplot(temp, x = dimnames( B_flbeia(s0))$year, type = 'l', xlab = 'Year', ylab = 'Biomass')
legend('top', c('s0', 's3'), lwd = 3, col = c('black','red') )

temp <- cbind(matrix(F_flbeia(s0)), matrix(F_flbeia(s3)))
matplot(temp, x = dimnames( B_flbeia(s0))$year, type = 'l', xlab = 'Year', ylab = 'Fishing Mortality')
legend('top', c('s0', 's3'), col = c('black','red'), lty = c(1,2) )

## ----echo=TRUE, eval=TRUE,  results = "hide"-----------------------------
HCR.models       <- 'fixedAdvice'
oneAdvC1     <- create.advice.ctrl(stksnames = stks, HCR.models =  HCR.models) 

s4 <- FLBEIA(biols = oneBio, SRs = oneSR , BDs = NULL, fleets = oneFl, covars = oneCv,         
            indices = NULL, advice = oneAdv, main.ctrl = oneMainC, biols.ctrl = oneBioC,      
            fleets.ctrl = oneFlC, covars.ctrl = oneCvC, obs.ctrl = oneObsC,
            assess.ctrl = oneAssC,  advice.ctrl = oneAdvC1) 

plotFLBiols(s4$biols,pdfnm= 's4')
plotFLFleets(s4$fleets,pdfnm= 's4')  

## ----echo=TRUE, eval=TRUE,  fig.width = 3.5, fig.height = 3.5, results = "hide"----
temp <- cbind(matrix(B_flbeia(s0)), matrix(B_flbeia(s4)))
matplot(temp, x = dimnames( B_flbeia(s0))$year, type = 'l', xlab = 'Year', ylab = 'Biomass')
legend('top', c('s0', 's4'), lwd = 3, col = c('black','red'), lty = c(1,2) )

## ----echo=TRUE, eval=TRUE,  fig.width = 3.5, fig.height = 3.5, results = "hide"----
temp <- cbind(matrix(F_flbeia(s0)), matrix(F_flbeia(s4)))
matplot(temp, x = dimnames( B_flbeia(s4))$year, type = 'l', xlab = 'Year', 
        ylab = 'Fishing Mortality')
legend('top', c('s0', 's4'), col = c('black','red'), lty = c(1,2)) 

## ----echo=TRUE, eval=TRUE,  fig.width = 3.5, fig.height = 3.5, results = "hide"----
temp <- cbind(matrix(s0$advice$TAC), matrix(s4$advice$TAC))
matplot(temp, x = dimnames(s0$advice$TAC)$year, type = 'l', xlab = 'Year', 
        ylab = 'TAC')
legend('top', c('s0', 's4'), col = c('black','red'), lty = c(1,2)) 

## ----echo=TRUE, eval=TRUE,  fig.width = 3.5, fig.height = 3.5, results = "hide"----
oneFlC1 <- oneFlC 
oneFlC1$fl1$effort.model <- 'fixedEffort'
s5 <- FLBEIA(biols = oneBio, SRs = oneSR , BDs = NULL, fleets = oneFl, covars = oneCv,         
            indices = NULL, advice = oneAdv, main.ctrl = oneMainC, biols.ctrl = oneBioC,      
            fleets.ctrl = oneFlC1, covars.ctrl = oneCvC, obs.ctrl = oneObsC, 
            assess.ctrl = oneAssC, advice.ctrl = oneAdvC)

s0_flt    <- fltSum(s0); s0_eff <- subset(s0_flt, indicator == 'effort')
s5_flt    <- fltSum(s5); s5_eff <- subset(s5_flt, indicator == 'effort')

temp <- cbind(s0_eff$value, s5_eff$value)
matplot(temp, x = dimnames( B_flbeia(s0))$year, type = 'l', xlab = 'Year', ylab = 'Effort')
legend('top', c('s0', 's5'), col = c('black','red'), lty = c(1,2))

## ----echo=TRUE, eval=TRUE,  fig.width = 3.5, fig.height = 3.5, results = "hide"----
oneFl1 <- oneFl
oneFl1$fl1@metiers$met1@catches$stk1@price <- oneFl$fl1@metiers$met1@catches$stk1@price*1.4 

s6 <- FLBEIA(biols = oneBio, SRs = oneSR , BDs = NULL, fleets = oneFl1, covars = oneCv,         
            indices = NULL, advice = oneAdv, main.ctrl = oneMainC, biols.ctrl = oneBioC,      
            fleets.ctrl = oneFlC, covars.ctrl = oneCvC, obs.ctrl = oneObsC, 
            assess.ctrl = oneAssC, advice.ctrl = oneAdvC)

s0_prof<- subset(s0_flt, indicator == 'income')
s6_flt <- fltSum(s6); s6_prof <- subset(s6_flt, indicator == 'income')

temp <- cbind(s0_prof$value, s6_prof$value)
matplot(temp[21:36,], x = dimnames(B_flbeia(s6))$year[21:36], type = 'l', 
        xlab = 'Year', ylab = 'Income')
legend('bottom', c('s0', 's6'),col = c('black','red'), lty = c(1,2) )

## ----echo=TRUE, eval=TRUE------------------------------------------------
rev_s0 <- revenue_flbeia(s0$fleets$fl1)   
rev_s6 <- revenue_flbeia(s6$fleets$fl1) 

## ----echo=TRUE,  eval=FALSE----------------------------------------------
## one_simul <- list(s0, s1, s2, s3, s4, s5, s6)
## scenarios <- c('s0', 's1', 's2', 's3', 's4', 's5', 's6')
## names(one_simul) <- scenarios
## 
## RefPts <- data.frame(stock = rep(names(one_simul[[1]][[1]]), each = 6*length(one_simul)),
##                      scenario = rep(names(one_simul), each = 6),
##                      indicator = rep(c('Bmsy','Fmsy', 'Bpa', 'Blim', 'Fpa', 'Flim'), length(one_simul)),
##                      value = rep(c(max(ssb(one_simul[[1]][[1]][['stk1']])[,1:12],na.rm = TRUE)*0.5,
##                                0.27,
##                                max(ssb(one_simul[[1]][[1]][['stk1']])[,1:12],na.rm = TRUE)*0.25,
##                                max(ssb(one_simul[[1]][[1]][['stk1']])[,1:12],na.rm = TRUE)*0.15,
##                                0.35, 0.5), length(one_simul)))
## 
## 
## flbeiaApp(one_simul , RefPts = RefPts,  years = ac(1990:2025), npv.y0 = '2009', npv.yrs = ac(2010:2025))

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
rm(list =ls())  # Clean the environment
data(oneIt)
ls()

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
opts_chunk$set(message=FALSE)

s0_it <- FLBEIA(biols = oneItBio, SRs = oneItSR , BDs = NULL, fleets = oneItFl, covars = oneItCv,         
            indices = NULL, advice = oneItAdv, main.ctrl = oneItMainC, biols.ctrl = oneItBioC,      
            fleets.ctrl = oneItFlC, covars.ctrl = oneItCvC, obs.ctrl = oneItObsC, assess.ctrl = oneItAssC, 
            advice.ctrl = oneItAdvC) 

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
s0_it_bio    <- bioSum(s0_it)             
s0_it_adv    <- advSum(s0_it)             
s0_it_flt    <- fltSum(s0_it)             
s0_it_fltStk <- fltStkSum(s0_it)          
s0_it_mt     <- mtSum(s0_it)              
s0_it_mtStk  <- mtStkSum(s0_it)          
s0_it_vessel <- vesselSum(s0_it)          
s0_it_vesselStk <- vesselStkSum(s0_it)    
s0_it_npv  <- npv(s0_it, y0 = '2014')     
s0_it_risk <- riskSum(s0_it, Bpa = c(stk1= 135000), Blim = c(stk1= 96000), Prflim = c(fl1 = 0))

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
proj.yr     <- 2009  
s0_it_bioQ   <- bioSumQ(s0_it_bio)
s0_it_fltQ   <- fltSumQ(s0_it_flt)

## ----echo=TRUE,  eval=TRUE-----------------------------------------------
plotFLBiols(s0_it$biols, pdfnm='s0_it')
plotFLFleets(s0_it$fleets,pdfnm='s0_it')
plotEco(s0_it, pdfnm='s0_it')
plotfltStkSum(s0_it, pdfnm='s0_it')

## ----echo=TRUE,  fig.width = 3.5, fig.height = 3, eval=TRUE--------------

aux <-  subset(s0_it_bioQ, indicator=="biomass")
p <- ggplot(data=aux , aes(x=year, y=q50, color=stock))+
      geom_line()+
      geom_ribbon(aes(x=year, ymin=q05, ymax=q95, fill=stock), alpha=0.5)+
      facet_wrap(~scenario, scales="free")+
      geom_vline(xintercept = proj.yr, linetype = "longdash")+
      theme_bw()+
      theme(text=element_text(size=10),
            title=element_text(size=10,face="bold"),
            strip.text=element_text(size=10))+
      ylab("Biomass (t)")
print(p)

## ----echo=TRUE,  fig.width = 3.5, fig.height = 3, eval=TRUE--------------
aux <-  subset(s0_it_fltQ, indicator=="profits")
aux$year <- as.numeric(as.character(aux$year))
p1 <- ggplot(data=aux , aes(x=year, y=q50, color=fleet))+
      geom_line()+
      geom_ribbon(aes(x=year, ymin=q05, ymax=q95, fill=fleet), alpha=0.5)+
      facet_wrap(~scenario, scales="free")+
      geom_vline(xintercept = proj.yr, linetype = "longdash")+
      theme_bw()+
      theme(text=element_text(size=10),
            title=element_text(size=10,face="bold"),
            strip.text=element_text(size=10))+
      ylab("Profits")
print(p1)

## ----echo=TRUE,  eval=FALSE----------------------------------------------
## 
## RefPts <- data.frame(stock = rep(names(one_simul[[1]][[1]]), each = 6*length(one_simul)),
##                      scenario = rep(names(one_simul), each = 6),
##                      indicator = rep(c('Bmsy','Fmsy', 'Bpa', 'Blim', 'Fpa', 'Flim'),
##                      length(one_simul)),value = rep(c(max(ssb(one_simul[[1]][[1]][['stk1']])
##                      [,1:12],na.rm = TRUE)*0.5, 0.27,
##                      max(ssb(one_simul[[1]][[1]][['stk1']])[,1:12],na.rm = TRUE)*0.25,
##                      max(ssb(one_simul[[1]][[1]][['stk1']])[,1:12],na.rm = TRUE)*0.15,
##                      0.35, 0.5), length(one_simul)))
## 
## RefPts <- data.frame(stock = rep(c('stk1'), each = 6), scenario = 'bc',
##                      indicator = rep(c('Bmsy','Fmsy', 'Bpa', 'Blim', 'Fpa', 'Flim')
##                       , 2), value = c(max((ssb(s0_it[[1]][[1]])), na.rm = TRUE)*0.75,
##                       0.27, max((ssb(s0_it[[1]][[1]])),na.rm = TRUE)*0.5,
##                       max((ssb(s0_it[[1]][[1]])),na.rm = TRUE)*0.25,
##                       0.35, 0.5))
## 
## 
## flbeiaApp(list(bc = s0_it), RefPts = RefPts, npv.y0 = '2012', npv.yrs = ac(2013:2020))

