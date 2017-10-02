## ---- ini, echo=FALSE, results='hide', message=FALSE---------------------
# This chunk set the document environment, so it is hidden
library(knitr)
knitr::opts_chunk$set(fig.align='center',
  message=FALSE, warning=FALSE, echo=TRUE, cache=FALSE)
options(width=50)
set.seed(1423)

## ---- eval=FALSE---------------------------------------------------------
## library(devtools)
## install_github("flr/FLBEIA")

## ---- pkgs, results = "hide"---------------------------------------------
# This chunk loads all necessary packages.
library(FLBEIA)
library(FLXSA)
library(FLash)
library(ggplotFL)
library(FLBEIAShiny) # This is a beta version.

## ----echo=TRUE, eval=TRUE------------------------------------------------
rm(list=ls())
data(multi)

## ----echo=TRUE, eval=TRUE------------------------------------------------
ls()
# Show the class of each of the objects.
sapply(ls(), function(x) class(get(x)))

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
SM <- FLBEIA(biols = multiBio,       # FLBiols object with 2 FLBiol element for stk1.
               SRs = multiSR,        # A list with 1 FLSRSim object for stk1.
               BDs = multiBD,        # A list with 1 FLBDSim object for stk2.
            fleets = multiFl,        # FLFleets object with on fleet.
            covars = multiCv,        # A list with socio - economic data.         
           indices = NULL,           # Indices not available.
            advice = multiAdv,       # A list with two elements 'TAC' and 'quota.share'.
         main.ctrl = multiMainC,     # A list with one element to define the start/end of the simulation.
        biols.ctrl = multiBioC,      # A list with one element to select the model to simulate the stock dynamics.
       fleets.ctrl = multiFlC,       # A list with several elements to select fleet dynamic models and store additional parameters.
       covars.ctrl = multiCvC,       # Covars control (additional data for capital dynamics) 
          obs.ctrl = multiObsC,      # A list with one element to define how the stock observed ("PerfectObs").
       assess.ctrl = multiAssC,      # A list with one element to define how the stock assesSMent model used ("NoAssesSMent").
       advice.ctrl = multiAdvC)      # A list with one element to define how the TAC advice is obtained ("IcesHCR").

## ----echo=TRUE, eval=TRUE------------------------------------------------
names(SM)

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
SM_bio    <- bioSum(SM)             # Data frame (DF) with the biological indicators.
SM_adv    <- advSum(SM)             # DF with the indicators related with the management advice (TAC). 
SM_flt    <- fltSum(SM)             # DF with the indicators at fleet level.
SM_fltStk <- fltStkSum(SM)          # DF with the indicators at fleet and stock level.
SM_mt     <- mtSum(SM)              # DF with the indicators at fleet.
SM_mtStk  <- mtStkSum(SM)           # DF with the indicators at fleet and metier level.
SM_vessel <- vesselSum(SM)          # DF with the indicators at vessel level.
SM_vesselStk <- vesselStkSum(SM)    # DF with the indicators at vessel and stock level.
SM_npv  <- npv(SM, y0 = '2014')     # DF with the net present value per fleet over the selected range of years.
SM_risk <- riskSum(SM,  Bpa= c(stk1= 135000, stk2 = 124000), Blim =c(stk1 = 96000, stk2 = 89000),
                   Prflim = c(fl1 = 0, fl2 = 0), flnms = names(SM$fleets), 
                   years = dimnames(SM$biols[[1]]@n)[[2]], scenario = 'SM') #  DF with the risk indicators. The indicators are: pBlim, pBpa and pPrlim.


# Exploring data frames
head(SM_bio); unique(SM_bio$indicator)
head(SM_adv); unique(SM_adv$indicator)
head(SM_flt); unique(SM_flt$indicator)
head(SM_fltStk); unique(SM_fltStk$indicator)
head(SM_mt); unique(SM_mt$indicator)
head(SM_mtStk); unique(SM_mtStk$indicator)
head(SM_vessel); unique(SM_vessel$indicator)
head(SM_vesselStk); unique(SM_vesselStk$indicator)
head(SM_risk); unique(SM_risk$indicator)

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
SM_bio_l    <- bioSum(SM, long = FALSE)            
SM_adv_l    <- advSum(SM, long = FALSE)             
SM_flt_l    <- fltSum(SM, long = FALSE)            
SM_fltStk_l <- fltStkSum(SM, long = FALSE)          
SM_mt_l     <- mtSum(SM, long = FALSE)             
SM_mtStk_l  <- mtStkSum(SM, long = FALSE)           
SM_vessel_l <- vesselSum(SM, long = FALSE)          
SM_vesselStk_l <- vesselStkSum(SM, long = FALSE) 

# Exploring data frames
head(SM_bio_l, 2)
head(SM_adv_l, 2)
head(SM_flt_l, 2)
head(SM_fltStk_l, 2)
head(SM_mt_l, 2)
head(SM_mtStk_l, 2)
head(SM_vessel_l, 2)
head(SM_vesselStk_l, 2)

## ----echo=TRUE, eval=TRUE, fig.width = 3.5, fig.height = 3.5-------------
#plot(SM$biols[[1]]) # There are too much data to display them correctly.
plot(SM$stocks[[1]])

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
# set your own working directory.
# myWD <- "My working directory")
# setwd(myWD)
plotFLBiols(SM$biols, pdfnm = "SM")
plotFLFleets(SM$fleets, pdfnm ="SM")
plotfltStkSum(SM, pdfnm ="SM") 
plotEco(SM, pdfnm ='SM')

## ----echo=TRUE, fig.cex = 0.5 , eval=TRUE--------------------------------
inds <- c('capacity','costs','income','profits')
d <- rbind(subset(SM_flt,indicator  %in% inds ))
d$indicator <- factor( d$indicator, levels=inds)
d$scenario <- factor(d$scenario)
d$year <- as.numeric(d$year)
p <- ggplot( data=d, aes(x=year, y=value, color=fleet)) + 
  geom_line() + 
  facet_wrap(~ indicator, scales="free") + 
  geom_vline(xintercept = multiMainC$sim.years[['initial']], linetype = "longdash") + 
  theme_bw() + 
  theme(text=element_text(size=10),
        title=element_text(size=10,face="bold"),
        strip.text=element_text(size=10)) + 
  ylab("")
print(p)

## ----echo=TRUE, results = "hide"-----------------------------------------
for (i in names(multiFl)) {
  for(j in names(multiFl[[i]]@metiers)) {
    for(k in names(multiFl[[i]]@metiers[[j]]@catches)) {
      multiFl[[i]]@metiers[[j]]@catches[[k]]@price <- multiFl[[i]]@metiers[[j]]@catches[[k]]@price*1000
    }
  }
}  

## ----echo=TRUE, results = "hide"-----------------------------------------
# Adjusting some values.
multiFl$fl2@capacity # There is a fixed capacity from 2009 onwards, then, the number of vessels and the maxdays will be fixed in the simulation period.
multiCv$MaxDays[2,ac(2010:2025),,,] <-multiCv$MaxDays[2,ac(2009),,,]
multiCv$NumbVessels[2,,,] <- multiFl$fl2@capacity/multiCv$MaxDays[2,,,,]

## ----echo=TRUE, results = "hide"-----------------------------------------
SM <- FLBEIA(biols = multiBio,   SRs = multiSR,  BDs = multiBD, fleets = multiFl, covars = multiCv,
             indices = NULL,advice = multiAdv, main.ctrl = multiMainC, biols.ctrl = multiBioC,
             fleets.ctrl = multiFlC, covars.ctrl = multiCvC, obs.ctrl = multiObsC, assess.ctrl = multiAssC,
             advice.ctrl = multiAdvC)     

## ----echo=TRUE, fig.cex = 0.5 , eval=TRUE--------------------------------
SM_bio    <- bioSum(SM)             # Data frame (DF) with the biological indicators.
SM_adv    <- advSum(SM)             # DF with the indicators related with the management advice (TAC). 
SM_flt    <- fltSum(SM)             # DF with the indicators at fleet level.
SM_fltStk <- fltStkSum(SM)          # DF with the indicators at fleet and stock level.
SM_mt     <- mtSum(SM)              # DF with the indicators at fleet.
SM_mtStk  <- mtStkSum(SM)           # DF with the indicators at fleet and metier level.
SM_vessel <- vesselSum(SM)          # DF with the indicators at vessel level.
SM_vesselStk <- vesselStkSum(SM)    # DF with the indicators at vessel and stock level.
SM_npv  <- npv(SM, y0 = '2014')     # DF with the net present value per fleet over the selected range of years.
SM_risk <- riskSum(SM, stknms = names(SM$biols), Bpa= c(stk1= 135000, stk2 = 124000), Blim =c(stk1 = 96000, stk2 = 89000),
                   Prflim = c(fl1 = 0, fl2 = 0), flnms = names(SM$fleets), 
                   years = dimnames(SM$biols[[1]]@n)[[2]], scenario = 'SM') #  DF with the risk indicators. The indicators are: pBlim, pBpa and pPr

## ----echo=TRUE, fig.cex = 0.5 , eval=TRUE--------------------------------
plotFLBiols(SM$biols, pdfnm = "SM_pricex1000")
plotFLFleets(SM$fleets, pdfnm ="SM_pricex1000")
plotfltStkSum(SM, pdfnm ="SM_pricex1000") 
plotEco(SM, pdfnm ='SM_pricex1000')

inds <- c('capacity','costs','income','profits')
d <- rbind(subset(SM_flt,indicator  %in% inds ))
d$indicator <- factor( d$indicator, levels=inds)
d$scenario <- factor(d$scenario)
d$year <- as.numeric(d$year)
p <- ggplot( data=d, aes(x=year, y=value, color=fleet)) + 
  geom_line() + 
  facet_wrap(~ indicator, scales="free") + 
  geom_vline(xintercept = multiMainC$sim.years[['initial']], linetype = "longdash") + 
  theme_bw() + 
  theme(text=element_text(size=10),
        title=element_text(size=10,face="bold"),
        strip.text=element_text(size=10)) + 
  ylab("")
print(p)

## ----echo=TRUE, results = "hide"-----------------------------------------
# Describe the price function
multiFlC_1 <- multiFlC
multiFlC_1$fl1$stk2$price.model <- "elasticPrice" # Set the price model.

# Include the new paramenter (elasticity)
multiFl_1 <- multiFl 
elasticity <- 0.5 # We assume that the elasticity is 0.2.
multiFlC_1$fl1$stk2$pd.els <-  array(elasticity, dim = c(1, 4, 1),
                                     dimnames= list(age = 'all', season = c(1:4), iter = 1)) 

# Reference landings: year 2008
La0_met1 <- multiFl$fl1@metiers$met1@catches$stk2@landings.n[,as.character(2008),,,]*multiFl$fl1@metiers$met1@catches$stk2@landings.wt[,as.character(2008),,,]
La0_met2 <- multiFl$fl1@metiers$met2@catches$stk2@landings.n[,as.character(2008),,,]*multiFl$fl1@metiers$met2@catches$stk2@landings.wt[,as.character(2008),,,]
pd.La0 <- unitSums(La0_met1 +La0_met2) 
                   
multiFlC_1$fl1$stk2$pd.La0 <- array(pd.La0, dim = c(1,4, 1),
                                   dimnames= list(age = 'all', season = c(1:4), iter = 1))

# Reference price
Pa0_met1 <- multiFl$fl1@metiers$met1@catches$stk2@price[,as.character(2008),,,] 
Pa0_met2 <- multiFl$fl1@metiers$met2@catches$stk2@price[,as.character(2008),,,]  
pd.Pa0 <- unitMeans((La0_met1*Pa0_met1 +La0_met2*Pa0_met2)/(La0_met1+La0_met2))

multiFlC_1$fl1$stk2$pd.Pa0 <- array(pd.Pa0,  dim = c(1,4, 1),
                                   dimnames= list(age = 'all', season = c(1:4), iter = 1))

multiFlC_1$fl1$stk2$pd.total <- TRUE # If TRUE the price is calculated using total landings and if FALSE the landings of the fleet in question are used to estimate the price.


SM_1 <- FLBEIA(biols = multiBio, SRs = multiSR, BDs = multiBD, fleets = multiFl_1,
               covars = multiCv, indices = NULL, advice = multiAdv, main.ctrl = multiMainC,
               biols.ctrl = multiBioC, fleets.ctrl = multiFlC_1, covars.ctrl = multiCvC,
               obs.ctrl = multiObsC, assess.ctrl = multiAssC, advice.ctrl = multiAdvC)


## ----echo=TRUE, eval=TRUE------------------------------------------------
SM_1_fltStk <- fltStkSum(SM_1, scenario ='elasticPrice') 
SM_x <- rbind(SM_fltStk, SM_1_fltStk)


inds <- c('price', 'catch')
d <- rbind(subset(SM_x,indicator  %in% inds & fleet == 'fl1' & stock == 'stk2'))
d$indicator <- factor( d$indicator, levels=inds)
d$scenario <- factor(d$scenario)
d$year <- as.numeric(d$year)
p <- ggplot( data=d, aes(x=year, y=value, color=scenario)) + 
  geom_line() + 
  facet_wrap(~ indicator, scales="free") + 
  geom_vline(xintercept = multiMainC$sim.years[['initial']], linetype = "longdash") + 
  theme_bw() + 
  theme(text=element_text(size=10),
        title=element_text(size=10,face="bold"),
        strip.text=element_text(size=10)) + 
  ylab("")
print(p)

## ----echo=TRUE, eval=TRUE------------------------------------------------
SM_1_flt <- fltSum(SM_1, scenario ='elasticPrice') 
SM_x <- rbind(SM_flt, SM_1_flt)
SM_x <- subset(SM_x, fleet == 'fl1')

inds <- c('capacity','costs','income','profits')
d <- rbind(subset(SM_x,indicator  %in% inds ))
d$indicator <- factor( d$indicator, levels=inds)
d$scenario <- factor(d$scenario)
d$year <- as.numeric(d$year)
p <- ggplot( data=d, aes(x=year, y=value, color=scenario)) + 
  geom_line() + 
  facet_wrap(~ indicator, scales="free") + 
  geom_vline(xintercept = multiMainC$sim.years[['initial']], linetype = "longdash") + 
  theme_bw() + 
  theme(text=element_text(size=10),
        title=element_text(size=10,face="bold"),
        strip.text=element_text(size=10)) + 
  ylab("")
print(p)

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
multiFl_2 <- multiFl
multiFl_2$fl1@fcost <- multiFl_2$fl1@fcost*(1-0.8)
SM_2 <- FLBEIA(biols = multiBio, SRs = multiSR, BDs = multiBD, fleets = multiFl_2,
               covars = multiCv, indices = NULL, advice = multiAdv, main.ctrl = multiMainC,
               biols.ctrl = multiBioC, fleets.ctrl = multiFlC, covars.ctrl = multiCvC,
               obs.ctrl = multiObsC, assess.ctrl = multiAssC, advice.ctrl = multiAdvC) 

## ----echo=TRUE, eval=TRUE, fig.width =7, fig.height = 7------------------
SM_2_flt    <- fltSum(SM_2, scenario = 'SM_2')
SM_x <- rbind(SM_flt, SM_2_flt)

inds <- c('costs','fcosts','income','profits')
d <- rbind(subset(SM_x,indicator  %in% inds & fleet == 'fl1'))
d$indicator <- factor( d$indicator, levels=inds)
d$scenario <- factor(d$scenario)
d$year <- as.numeric(d$year)
p <- ggplot( data=d, aes(x=year, y=value, color=scenario)) + 
  geom_line() + 
  facet_wrap(~ indicator, scales="free") + 
  geom_vline(xintercept = multiMainC$sim.years[['initial']], linetype = "longdash") + 
  theme_bw() + 
  theme(text=element_text(size=10),
        title=element_text(size=10,face="bold"),
        strip.text=element_text(size=10)) + 
  ylab("")
print(p)

## ----echo=TRUE, , results = "hide"---------------------------------------
multiFl_3 <- multiFl
multiFl_3$fl2@metiers$met1@vcost <- multiFl_3$fl2@metiers$met1@vcost*(1-0.5)
multiFl_3$fl2@metiers$met2@vcost <- multiFl_3$fl2@metiers$met2@vcost*(1-0.5)

SM_3 <- FLBEIA(biols = multiBio, SRs = multiSR, BDs = multiBD, fleets = multiFl_3,
               covars = multiCv, indices = NULL, advice = multiAdv, main.ctrl = multiMainC,
               biols.ctrl = multiBioC, fleets.ctrl = multiFlC, covars.ctrl = multiCvC,
               obs.ctrl = multiObsC, assess.ctrl = multiAssC, advice.ctrl = multiAdvC) 

## ----echo=TRUE, eval=TRUE, fig.width = 7, fig.height = 7, eval=TRUE------
SM_3_flt    <- fltSum(SM_3, scenario = 'SM_3')

inds <- c('costs','vcosts','income','profits')
d <- rbind(subset(SM_flt,indicator  %in% inds & fleet == 'fl2'),
            subset(SM_3_flt, indicator %in% inds  & fleet == 'fl2'))
d$indicator <- factor( d$indicator, levels=inds)
d$year <- as.numeric(d$year)
p <- ggplot( data=d, aes(x=year, y=value, color=scenario)) + 
  geom_line() + 
  facet_wrap(~ indicator, scales="free") + 
  geom_vline(xintercept = multiMainC$sim.years[['initial']], linetype = "longdash") + 
  theme_bw() + 
  theme(text=element_text(size=10),
        title=element_text(size=10,face="bold"),
        strip.text=element_text(size=10)) + 
  ylab("")
print(p)

## ----echo=TRUE, results = "hide"-----------------------------------------
multiFl_4 <- multiFl
multiFl_4$fl1@crewshare[] <- 0.5

SM_4 <- FLBEIA(biols = multiBio, SRs = multiSR, BDs = multiBD, fleets = multiFl_4,
               covars = multiCv, indices = NULL, advice = multiAdv, main.ctrl = multiMainC,
               biols.ctrl = multiBioC, fleets.ctrl = multiFlC, covars.ctrl = multiCvC,
               obs.ctrl = multiObsC, assess.ctrl = multiAssC, advice.ctrl = multiAdvC) 

## ----echo=TRUE, eval=TRUE, fig.width = 7, fig.height = 7, eval=TRUE------
SM_4_flt    <- fltSum(SM_4, scenario = 'SM_4')

inds <- c('costs','vcosts','income','profits')
d <- rbind(subset(SM_flt,indicator  %in% inds & fleet == 'fl1'),
            subset(SM_4_flt, indicator %in% inds  & fleet == 'fl1'))
d$indicator <- factor( d$indicator, levels=inds)
d$year <- as.numeric(d$year)
p <- ggplot( data=d, aes(x=year, y=value, color=scenario)) + 
  geom_line() + 
  facet_wrap(~ indicator, scales="free") + 
  geom_vline(xintercept = multiMainC$sim.years[['initial']], linetype = "longdash") + 
  theme_bw() + 
  theme(text=element_text(size=10),
        title=element_text(size=10,face="bold"),
        strip.text=element_text(size=10)) + 
  ylab("")
print(p)

## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
# fl1 is fixed effort
multiFlC_5 <- multiFlC
multiFlC_5$fl2$capital.model <- multiFlC_5$fl2$capital.model <- 'SCD'

multiFl_5 <- multiFl
multiCv_5 <- multiCv
multiCv_5$w1[] <- 0.01  
multiCv_5$w2[] <- 0.01
multiCv_5$InvestShare[] <- 0.02
  
SM_5 <- FLBEIA(biols = multiBio, SRs = multiSR, BDs = multiBD, fleets = multiFl_5,
                covars = multiCv_5, indices = NULL, advice = multiAdv, main.ctrl = multiMainC,
                biols.ctrl = multiBioC, fleets.ctrl = multiFlC_5, covars.ctrl = multiCvC,
                obs.ctrl = multiObsC, assess.ctrl = multiAssC, advice.ctrl = multiAdvC)



## ----echo=TRUE, eval=TRUE------------------------------------------------
SM_5_flt    <- fltSum(SM_5, scenario = 'SM_5')
SM_x <- rbind( SM_flt, SM_5_flt )

inds <- c("capacity",'nVessels','effort','profits')
d <- rbind(subset(SM_x,indicator  %in% inds & fleet == 'fl2'))
d$indicator <- factor( d$indicator, levels=inds)
d$scenario <- factor(d$scenario)
d$year <- as.numeric(d$year)

p <- ggplot( data=d, aes(x=year, y=value, color=scenario)) + 
  geom_line() + 
  facet_wrap(~ indicator, scales="free") + 
  geom_vline(xintercept = multiMainC$sim.years[['initial']], linetype = "longdash") + 
  theme_bw() + 
  theme(text=element_text(size=10),
        title=element_text(size=10,face="bold"),
        strip.text=element_text(size=10)) + 
  ylab("")
print(p)

## ----echo=TRUE,  eval=FALSE----------------------------------------------
## multi_simul <- list(SM, SM_1, SM_2, SM_3, SM_4, SM_5)
## scenarios <- c('SM', 'SM_1', 'SM_2', 'SM_3', 'SM_4', 'SM_5')
## names(multi_simul) <- scenarios
## 
## RefPts <- data.frame(stock = rep(names(multi_simul[[1]][[1]]), each = 6*length(multi_simul)),
##                      scenario = rep(names(multi_simul), each = 6),
##                      indicator = rep(c('Bmsy','Fmsy', 'Bpa', 'Blim', 'Fpa', 'Flim'),  2*length(multi_simul)),
##                      value = rep(c(max(seasonSums(unitSums(ssb(multiBio[[1]]))),na.rm = TRUE)*0.75,
##                                0.27,
##                                max(seasonSums(unitSums(ssb(multiBio[[1]]))),na.rm = TRUE)*0.5,
##                                max(seasonSums(unitSums(ssb(multiBio[[1]]))),na.rm = TRUE)*0.25,
##                                0.35, 0.5,
##                                max(seasonSums(unitSums(ssb(multiBio[[2]]))),na.rm = TRUE)*0.75,
##                                0.2,
##                                max(seasonSums(unitSums(ssb(multiBio[[2]]))),na.rm = TRUE)*0.5,
##                                max(seasonSums(unitSums(ssb(multiBio[[2]]))),na.rm = TRUE)*0.25,
##                                0.3,0.4), length(multi_simul)))
## 
## flbeiaApp(multi_simul , RefPts = RefPts,  years = ac(1990:2025), npv.y0 = '2009', npv.yrs = ac(2010:2025))

