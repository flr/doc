## ---- ini, echo=FALSE, results='hide', message=FALSE--------------------------
# This chunk set the document environment, so it is hidden
library(knitr)
source("R/ini.R")
knitr::opts_chunk$set(fig.align='center',
  message=FALSE, warning=FALSE, echo=TRUE, cache=FALSE)
options(width=50)
set.seed(1423)


## ----echo=FALSE, out.width='20%'----------------------------------------------
include_graphics('images/FLBEIA_logo.png')


## ---- eval=FALSE--------------------------------------------------------------
## data(package='FLBEIA')


## ---- eval=FALSE--------------------------------------------------------------
## install.packages( c("ggplot2"))
## install.packages( c("FLCore", "FLFleet", "FLBEIA", "ggplotFL",
##                     "FLash", "FLAssess", "FLXSA"),
##                   repos="http://flr-project.org/R")


## ----echo=FALSE, eval=FALSE---------------------------------------------------
## library(devtools)
## install_github('FLR/FLBEIA')


## ---- pkgs, results = "hide"--------------------------------------------------
# This chunk loads all necessary packages.
library(FLBEIA)
library(tidyr)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
data(one) 


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
ls()


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
sapply(ls(), function(x) class(get(x)))


## ----echo=TRUE, eval=TRUE, results = "hide"-----------------------------------
s0 <- FLBEIA(biols = oneBio,       # FLBiols object with one FLBiol element for stk1.
               SRs = oneSR,        # A list with one FLSRSim object for stk1.
               BDs = NULL,         # No Biomass dynamics populations in this case.
            fleets = oneFl,        # FLFleets object with one fleet.
            covars = oneCv,        # Covar is and object with aditional data on fleet 
                                   # (number of vessels, etc).
           indices = NULL,         # Indices not used 
            advice = oneAdv,       # A list with two elements 'TAC' and 'quota.share'
         main.ctrl = oneMainC,     # A list with one element to define the start and 
                                   # end of the simulation.
        biols.ctrl = oneBioC,      # A list with one element to select the model to 
                                   # simulate the stock dynamics.
       fleets.ctrl = oneFlC,       # A list with several elements to select fleet dynamics
                                   # models and store additional parameters.
       covars.ctrl = oneCvC,       # A list with several data related to the fleet.
          obs.ctrl = oneObsC,      # A list with one element to define how the stock 
                                   # observed ("PerfectObs").
       assess.ctrl = oneAssC,      # A list with one element to define how the stock 
                                   # assessment model used ("NoAssessment").
       advice.ctrl = oneAdvC)      # A list with one element to define how the TAC advice 
                                   # is obtained ("IcesHCR").


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
names(s0)
sapply(s0, function(x) class(x))


## ----echo=TRUE, eval=TRUE, results = "hide"-----------------------------------
s0_bio    <- bioSum(s0, long = TRUE)           # Data frame (DF) of biological indicators.
s0_adv    <- advSum(s0, long = TRUE)           # DF of management advice (TAC). 
s0$fleets <- setUnitsNA(s0$fleets)
s0_flt    <- fltSum(s0, long = TRUE)           # DF of economics indicators at fleet level.
s0_fltStk <- fltStkSum(s0, long = TRUE)        # DF of indicators at fleet and stock level.
s0_mt     <- mtSum(s0, long = TRUE)            # DF of indicators at fleet and metier level.
s0_mtStk  <- mtStkSum(s0, long = TRUE)         # DF of indicators at fleet, metier and stock level.
s0_vessel <- vesselSum(s0, long = TRUE)        # DF of indicators at vessel level.
s0_vesselStk <- vesselStkSum(s0, long = TRUE)  # DF of indicators at vessel and stock level.
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


## ----echo=TRUE, eval=TRUE, results = "hide"-----------------------------------
s0_bio_w    <- bioSum(s0, years = ac(2016:2020))            
s0_adv_w    <- advSum(s0, years = ac(2016:2020))             
s0_flt_w    <- fltSum(s0, years = ac(2016:2025))
s0_fltStk_w <- fltStkSum(s0, years = ac(2016:2020))          
s0_mt_w     <- mtSum(s0, years = ac(2016:2020))             
s0_mtStk_w  <- mtStkSum(s0, years = ac(2016:2020))           
s0_vessel_w <- vesselSum(s0, years = ac(2016:2020))
s0_vesselStk_w <- vesselStkSum(s0, years = ac(2016:2020))       

# Exploring data frames
head(s0_bio_w, 2)
head(s0_adv_w, 2)
head(s0_flt_w, 2)
head(s0_fltStk_w, 2)
head(s0_mt_w, 2)
head(s0_mtStk_w, 2)
head(s0_vessel_w, 2)
head(s0_vesselStk_w, 2)


## ----echo=TRUE, fig.width = 4, fig.height = 4, eval=TRUE----------------------
plot(s0$biols[[1]])


## ----echo=TRUE, fig.width = 4, fig.height = 4, eval=TRUE----------------------
plot(s0$stocks[[1]])


## ----echo=TRUE, eval=FALSE----------------------------------------------------
## # set your own working directory.
## # myWD <- "My working directory"
## # setwd(myWD)
## plotFLBiols(s0$biols, pdfnm="s0")
## plotFLFleets(s0$fleets, pdfnm="s0")
## plotEco(s0, pdfnm="s0")
## plotfltStkSum(s0, pdfnm="s0")


## ----echo=TRUE,  fig.width = 4, fig.height = 2, eval=TRUE---------------------
aux <- subset(s0_bio, indicator=="catch" )
p <- ggplot(data=aux, aes(x=year, y=value, color=stock))+
  geom_line()+
  geom_vline(xintercept = 2016, linetype = "longdash")+
  theme_bw()+
  theme(text=element_text(size=8),
        title=element_text(size=8,face="bold"),
        strip.text=element_text(size=8))+
  ylab("Catch (t)")
print(p)


## ----echo=FALSE, eval=FALSE---------------------------------------------------
## dev.off()


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
oneSR$stk1@model


## ----echo=TRUE, eval=TRUE, results = "hide"-----------------------------------

first.yr          <- 1990
proj.yr           <- 2010 
last.yr           <- 2025  
yrs <- c(first.yr=first.yr,proj.yr=proj.yr,last.yr=last.yr)
stks <- c('stk1')

# As age 1 recruitment --> R_{y+1} = f(SSB_{y})
ssb <-  ssb(oneBio[[1]])[,as.character(first.yr:(proj.yr-2)),1,1]
rec <-  oneBio[[1]]@n[1,as.character((first.yr+1):(proj.yr-1)),]
sr.segreg <- fmle(FLSR(model="segreg", ssb = ssb, rec = rec))

# Introduce the new model and its parameters in SR object.
SRs.segreg <- oneSR
SRs.segreg[[1]]@params[,,,] <- sr.segreg@params[,] 
SRs.segreg[[1]]@model <- 'segreg'

#Run FLBEIA with the new SR function.
s1 <- FLBEIA(biols = oneBio, SRs = SRs.segreg , BDs = NULL, fleets = oneFl, #covars = oneCv,         
            indices = NULL, advice = oneAdv, main.ctrl = oneMainC, biols.ctrl = oneBioC,      
            fleets.ctrl = oneFlC, covars.ctrl = oneCvC, obs.ctrl = oneObsC, 
            assess.ctrl = oneAssC, advice.ctrl = oneAdvC)    


## ----echo=TRUE, eval=FALSE----------------------------------------------------
## plotFLBiols(s1$biols, pdfnm='s1')
## plotFLFleets(s1$fleets, pdfnm='s1')


## ----echo=TRUE,  fig.width = 3.5, fig.height = 3.5, eval=TRUE-----------------
temp <- cbind(matrix(B_flbeia(s0)), matrix(B_flbeia(s1)))
matplot(temp, x = dimnames( B_flbeia(s1))$year, type = 'l', 
        xlab = 'Year', ylab = 'Biomass')
legend('top', c('s0', 's1'), col = c('black','red'), lty = c(1,2))


## ----echo=TRUE,   eval=TRUE---------------------------------------------------
s1_bio    <- bioSum(s1, long = TRUE)
s0_bio$scenario <- c('s0')
s1_bio$scenario <- c('s1')

s0_s1_bio <- rbind(s0_bio , s1_bio )
unique(s0_s1_bio$scenario)
head(s0_s1_bio)
aux <- s0_s1_bio
aux$year <- as.numeric(as.character(aux$year))
p1 <- ggplot(data=aux , aes(x=year, y=value, color=scenario))+
      geom_line()+
      facet_wrap(~indicator, scales="free")+
      geom_vline(xintercept = proj.yr, linetype = "longdash")+
      theme_bw()+
      theme(text=element_text(size=8),
            title=element_text(size=8,face="bold"),
            strip.text=element_text(size=8))+
      ylab("Biological Indicators")
print(p1)


## ----echo=TRUE, eval=TRUE,  results = "hide"----------------------------------
oneBioM <- oneBio
oneBioM$stk1@m[7:12,,,,] <- oneBioM$stk1@m[7:12,,,,]*1.2

s2 <- FLBEIA(biols = oneBioM, SRs = oneSR , BDs = NULL, fleets = oneFl, # covars = oneCv,         
            indices = NULL, advice = oneAdv, main.ctrl = oneMainC, biols.ctrl = oneBioC,      
            fleets.ctrl = oneFlC, covars.ctrl = oneCvC, obs.ctrl = oneObsC, 
            assess.ctrl = oneAssC, advice.ctrl = oneAdvC) 


## ----echo=TRUE, eval=FALSE----------------------------------------------------
## plotFLBiols(s1$biols, pdfnm='s2')
## plotFLFleets(s1$fleets, pdfnm='s2')


## ----echo=TRUE,  fig.width = 3.5, fig.height = 3.5, eval=TRUE-----------------
temp <- cbind(matrix(B_flbeia(s0)), matrix(B_flbeia(s2)))
matplot(temp, x = dimnames( B_flbeia(s0))$year, type = 'l', xlab = 'Year', ylab = 'Biomass')
legend('top', c('s0', 's2'), col = c('black','red'), lty = c(1,2))



## ----echo=TRUE,  eval=TRUE----------------------------------------------------
s2_bio    <- bioSum(s2, long = TRUE)
s2_bio$scenario <- c('s2')

s0_s2_bio <- rbind(s0_bio , s2_bio )
unique(s0_s2_bio$scenario)
head(s0_s2_bio)
aux <- s0_s2_bio
aux$year <- as.numeric(as.character(aux$year))
p1 <- ggplot(data=aux , aes(x=year, y=value, color=scenario))+
      geom_line()+
      facet_wrap(~indicator, scales="free")+
      geom_vline(xintercept = proj.yr, linetype = "longdash")+
      theme_bw()+
      theme(text=element_text(size=8),
            title=element_text(size=8,face="bold"),
            strip.text=element_text(size=8))+
      ylab("Biological Indicators")
print(p1)



## ----echo=TRUE, eval=TRUE, results = "hide"-----------------------------------
oneAdvC$stk1$ref.pts[3]
oneAdvC2 <- oneAdvC
oneAdvC2$stk1$ref.pts[3] <- oneAdvC$stk1$ref.pts[3]*0.8
s3 <- FLBEIA(biols = oneBio, SRs = oneSR , BDs = NULL, fleets = oneFl, #covars = oneCv,         
            indices = NULL, advice = oneAdv, main.ctrl = oneMainC, biols.ctrl = oneBioC,      
            fleets.ctrl = oneFlC, covars.ctrl = oneCvC, obs.ctrl = oneObsC,
            assess.ctrl = oneAssC, advice.ctrl = oneAdvC2)


## ----echo=TRUE, eval=TRUE,  results = "hide"----------------------------------
s3_bio    <- bioSum(s3, long = TRUE)
s3_bio$scenario <- c('s3')

s0_s3_bio <- rbind(s0_bio , s3_bio )
unique(s0_s3_bio$scenario)
head(s0_s3_bio)
aux <- s0_s3_bio
aux$year <- as.numeric(as.character(aux$year))
p1 <- ggplot(data=aux , aes(x=year, y=value, color=scenario))+
      geom_line()+
      facet_wrap(~indicator, scales="free")+
      geom_vline(xintercept = proj.yr, linetype = "longdash")+
      theme_bw()+
      theme(text=element_text(size=8),
            title=element_text(size=8,face="bold"),
            strip.text=element_text(size=8))+
      ylab("Biological Indicators")
print(p1)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
oneAdv$TAC
oneAdv2 <- oneAdv
TAC_mean <- mean(oneAdv$TAC[,ac(2007:2009),])
oneAdv2$TAC[,ac(2010:2025),] <- TAC_mean 


## ----eval=FALSE---------------------------------------------------------------
## ?create.advice.ctrl # see function documentation


## ----echo=TRUE, eval=TRUE,  results = "hide"----------------------------------
HCR.models       <- 'fixedAdvice'
oneAdvC1     <- create.advice.ctrl(stksnames = stks, HCR.models =  HCR.models) 

s4 <- FLBEIA(biols = oneBio, SRs = oneSR , BDs = NULL, fleets = oneFl, #covars = oneCv,         
            indices = NULL, advice = oneAdv, main.ctrl = oneMainC, biols.ctrl = oneBioC,      
            fleets.ctrl = oneFlC, covars.ctrl = oneCvC, obs.ctrl = oneObsC,
            assess.ctrl = oneAssC,  advice.ctrl = oneAdvC1) 


## ----echo=TRUE, eval=FALSE----------------------------------------------------
## plotFLBiols(s4$biols,pdfnm= 's4')
## plotFLFleets(s4$fleets,pdfnm= 's4')


## ----echo=TRUE, eval=TRUE, results = "hide"-----------------------------------
s4_bio    <- bioSum(s4, long = TRUE)
s4_bio$scenario <- c('s4')

s0_s4_bio <- rbind(s0_bio , s4_bio )
unique(s0_s4_bio$scenario)
head(s0_s4_bio)
aux <- s0_s4_bio
aux$year <- as.numeric(as.character(aux$year))
p1 <- ggplot(data=aux , aes(x=year, y=value, color=scenario))+
      geom_line()+
      facet_wrap(~indicator, scales="free")+
      geom_vline(xintercept = proj.yr, linetype = "longdash")+
      theme_bw()+
      theme(text=element_text(size=8),
            title=element_text(size=8,face="bold"),
            strip.text=element_text(size=8))+
      ylab("Biological Indicators")
print(p1)


## ----echo=TRUE, eval=TRUE,  fig.width = 3.5, fig.height = 3.5, results = "hide"----
oneFlC1 <- oneFlC 
oneFlC1$fl1$effort.model <- 'fixedEffort' 

oneFl$fl1@effort # Note that the effort of projection period is equal to the average effort of years 2007:2009.

s5 <- FLBEIA(biols = oneBio, SRs = oneSR , BDs = NULL, fleets = oneFl, #covars = oneCv,         
            indices = NULL, advice = oneAdv, main.ctrl = oneMainC, biols.ctrl = oneBioC,      
            fleets.ctrl = oneFlC1, covars.ctrl = oneCvC, obs.ctrl = oneObsC, 
            assess.ctrl = oneAssC, advice.ctrl = oneAdvC)
s5$fleets <- setUnitsNA(s5$fleets)
s0_flt    <- fltSum(s0, long = TRUE); s0_eff <- subset(s0_flt, indicator == 'effort')
s5_flt    <- fltSum(s5, long = TRUE); s5_eff <- subset(s5_flt, indicator == 'effort')

temp <- cbind(s0_eff$value, s5_eff$value)
matplot(temp, x = dimnames( B_flbeia(s0))$year, type = 'l', xlab = 'Year', ylab = 'Effort')
legend('top', c('s0', 's5'), col = c('black','red'), lty = c(1,2))


## ----echo=TRUE, eval=TRUE, results = "hide"-----------------------------------

s5_flt    <- fltSum(s5, long = TRUE)
s5_flt$scenario <- c('s5')
s0_flt$scenario <- c('s0')

s0_s5_flt <- rbind(s0_flt , s5_flt)
unique(s0_s5_flt$scenario)
head(s0_s5_flt)
aux <- s0_s5_flt
aux$year <- as.numeric(as.character(aux$year))
p1 <- ggplot(data=aux , aes(x=year, y=value, color=scenario))+
      geom_line()+
      facet_wrap(~indicator, scales="free")+
      geom_vline(xintercept = proj.yr, linetype = "longdash")+
      theme_bw()+
      theme(text=element_text(size=8),
            title=element_text(size=8,face="bold"),
            strip.text=element_text(size=8))+
      ylab("Economics Indicators")
print(p1)


## ----echo=TRUE, eval=TRUE,  fig.width = 3.5, fig.height = 3.5, results = "hide"----
oneFl1 <- oneFl
oneFl1$fl1@metiers$met1@catches$stk1@price <- oneFl$fl1@metiers$met1@catches$stk1@price*1.4 

s6 <- FLBEIA(biols = oneBio, SRs = oneSR , BDs = NULL, fleets = oneFl1, #covars = oneCv,         
            indices = NULL, advice = oneAdv, main.ctrl = oneMainC, biols.ctrl = oneBioC,      
            fleets.ctrl = oneFlC, covars.ctrl = oneCvC, obs.ctrl = oneObsC, 
            assess.ctrl = oneAssC, advice.ctrl = oneAdvC)
s6$fleets <- setUnitsNA(s6$fleets)
s0_prof<- subset(s0_flt, indicator == 'grossValue')
s6_flt <- fltSum(s6, long = TRUE); s6_prof <- subset(s6_flt, indicator == 'grossValue')

temp <- cbind(s0_prof$value, s6_prof$value)
matplot(temp[21:36,], x = dimnames(B_flbeia(s6))$year[21:36], type = 'l', 
        xlab = 'Year', ylab = 'Income')
legend('bottom', c('s0', 's6'),col = c('black','red'), lty = c(1,2) )


## ----echo=TRUE, eval=TRUE, results = "hide"-----------------------------------

s6_flt    <- fltSum(s6, long = TRUE)
s6_flt$scenario <- c('s6')
s0_flt$scenario <- c('s0')

s0_s6_flt <- rbind(s0_flt , s6_flt)
unique(s0_s6_flt$scenario)
head(s0_s6_flt)
aux <- s0_s6_flt
aux$year <- as.numeric(as.character(aux$year))
p1 <- ggplot(data=aux , aes(x=year, y=value, color=scenario))+
      geom_line()+
      facet_wrap(~indicator, scales="free")+
      geom_vline(xintercept = proj.yr, linetype = "longdash")+
      theme_bw()+
      theme(text=element_text(size=8),
            title=element_text(size=8,face="bold"),
            strip.text=element_text(size=8))+
      ylab("Economics Indicators")
print(p1)


## ----echo=TRUE,  eval=FALSE---------------------------------------------------
## 
## scnms <-c('s0', 's1', 's2', 's3', 's4', 's5', 's6')
## stknms <- unique(s0_bio$stock)
## RefPts <- expand.grid(indicator=c("Bmsy", "Fmsy", "Bpa", "Blim", "Fpa", "Flim"),
##                       scenario=scnms, stock=stknms, value=NA)[,c(3,2,1,4)]
## RefPts$value <- c( c(800, 0.11, 800, 550, 0.25, 0.50), c(800, 0.2, 800, 550, 0.25, 0.50),
##                    c(800, 0.2, 800, 550, 0.25, 0.50), c(800, 0.2, 800, 550, 0.25, 0.50),
##                    c(800, 0.2, 800, 550, 0.25, 0.50), c(800, 0.2, 800, 550, 0.25, 0.50),
##                    c(800, 0.2, 800, 550, 0.25, 0.50))
## 
## flbeiaObjs <- list( s0 = s0, s1 = s1, s2 = s2, s3 = s3,
##                     s4 = s4, s5 = s5, s6 = s6)
## 
## 
## flbeiaApp( flbeiaObjs = flbeiaObjs, RefPts = RefPts, years = ac(2000:2025),
##              calculate_npv = TRUE, npv.y0 = '2012', npv.yrs = ac(2013:2025))


## ----echo=TRUE, eval=TRUE, results = "hide"-----------------------------------
rm(list =ls())  # Clean the environment
data(oneIt)
ls()


## ----echo=TRUE, eval=TRUE, results = "hide"-----------------------------------
opts_chunk$set(message=FALSE)

s0_it <- FLBEIA(biols = oneItBio, SRs = oneItSR , BDs = NULL, fleets = oneItFl, #covars = oneItCv,         
            indices = NULL, advice = oneItAdv, main.ctrl = oneItMainC, 
            biols.ctrl = oneItBioC, fleets.ctrl = oneItFlC, covars.ctrl = oneItCvC, 
            obs.ctrl = oneItObsC, assess.ctrl = oneItAssC, 
            advice.ctrl = oneItAdvC) 


## ----echo=TRUE, eval=TRUE, results = "hide"-----------------------------------
s0_it$fleets <- setUnitsNA(s0_it$fleets)
s0_it_bio    <- bioSum(s0_it, long = TRUE)             
s0_it_adv    <- advSum(s0_it, long = TRUE)             
s0_it_flt    <- fltSum(s0_it, long = TRUE)             
s0_it_fltStk <- fltStkSum(s0_it, long = TRUE)          
s0_it_mt     <- mtSum(s0_it, long = TRUE)              
s0_it_mtStk  <- mtStkSum(s0_it, long = TRUE)          
s0_it_vessel <- vesselSum(s0_it, long = TRUE)          
s0_it_vesselStk <- vesselStkSum(s0_it, long = TRUE)    
s0_it_npv  <- npv(s0_it, y0 = '2014')     
s0_it_risk <- riskSum(s0_it, Bpa = c(stk1= 135000), Blim = c(stk1= 96000), 
                      Prflim = c(fl1 = 0))


## ----echo=TRUE, eval=TRUE, results = "hide"-----------------------------------
proj.yr     <- 2009  
s0_it_bioQ   <- bioSumQ(s0_it_bio)
s0_it_fltQ   <- fltSumQ(s0_it_flt)


## ----echo=TRUE, eval=FALSE----------------------------------------------------
## plotFLBiols(s0_it$biols, pdfnm='s0_it')
## plotFLFleets(s0_it$fleets,pdfnm='s0_it')
## plotEco(s0_it, pdfnm='s0_it')
## plotfltStkSum(s0_it, pdfnm='s0_it')


## ----echo=TRUE,  fig.width = 3.5, fig.height = 3, eval=TRUE-------------------

aux <-  subset(s0_it_bioQ, indicator=="biomass")
p <- ggplot(data=aux , aes(x=year, y=q50, color=stock))+
      geom_line()+
      geom_ribbon(aes(x=year, ymin=q05, ymax=q95, fill=stock), alpha=0.5)+
      facet_wrap(~scenario, scales="free")+
      geom_vline(xintercept = proj.yr, linetype = "longdash")+
      theme_bw()+
      theme(text=element_text(size=8),
            title=element_text(size=8,face="bold"),
            strip.text=element_text(size=8))+
      ylab("Biomass (t)")
print(p)


## ----echo=TRUE,  fig.width = 3.5, fig.height = 3, eval=TRUE-------------------
aux <-  subset(s0_it_fltQ, indicator=="netProfit")
aux$year <- as.numeric(as.character(aux$year))
p1 <- ggplot(data=aux , aes(x=year, y=q50, color=fleet))+
      geom_line()+
      geom_ribbon(aes(x=year, ymin=q05, ymax=q95, fill=fleet), alpha=0.5)+
      facet_wrap(~scenario, scales="free")+
      geom_vline(xintercept = proj.yr, linetype = "longdash")+
      theme_bw()+
      theme(text=element_text(size=8),
            title=element_text(size=8,face="bold"),
            strip.text=element_text(size=8))+
      ylab("netProfit")
print(p1)


## ----echo=TRUE, eval=TRUE, results = "hide"-----------------------------------
dev.off()


## ----echo=TRUE,  eval=FALSE---------------------------------------------------
## scnms <-c('s0_it')
## stknms <- unique(s0_it_bio$stock)
## RefPts <- expand.grid(indicator=c("Bmsy", "Fmsy", "Bpa", "Blim", "Fpa", "Flim"),
##                       scenario=scnms, stock=stknms, value=NA)[,c(3,2,1,4)]
## RefPts$value <- c(c(800, 0.11, 800, 550, 0.25, 0.50))
## 
## flbeiaObjs <- list( s0_it = s0_it)
## 
## 
## flbeiaApp( flbeiaObjs = flbeiaObjs, RefPts = RefPts, years = ac(2000:2025),
##              calculate_npv = TRUE, npv.y0 = '2012', npv.yrs = ac(2013:2025))

