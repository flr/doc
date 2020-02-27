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
## data(package='FLBEIA')


## ---- eval=FALSE---------------------------------------------------------
## install.packages( c("ggplot2"))
## install.packages( c("FLCore", "FLFleet", "FLBEIA", "ggplotFL",
##                     "FLash", "FLAssess", "FLXSA"),
##                   repos="http://flr-project.org/R")


## ----echo=FALSE, eval=FALSE----------------------------------------------
## library(devtools)
## install_github('FLR/FLBEIA')


## ---- pkgs, results = "hide"---------------------------------------------
# This chunk loads all necessary packages.
library(FLBEIA)
library(FLCore)
library(FLFleet)
library(FLXSA)
library(FLash)
library(ggplotFL)


## ----echo=TRUE, eval=TRUE------------------------------------------------
rm(list=ls())
data(multi)


## ----echo=TRUE, eval=TRUE------------------------------------------------
ls()


## ----echo=TRUE, eval=TRUE------------------------------------------------
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
         main.ctrl = multiMainC,     # A list with one element to define the start/end 
                                     # of the simulation.
        biols.ctrl = multiBioC,      # A list with one element to select the model 
                                     # to simulate the stock dynamics.
       fleets.ctrl = multiFlC,       # A list with several elements to select fleet
                                     # dynamic models and store additional parameters.
       covars.ctrl = multiCvC,       # Covars control (additional data for capital dynamics).
          obs.ctrl = multiObsC,      # A list with one element to define how the 
                                     # stock observed ("PerfectObs").
       assess.ctrl = multiAssC,      # A list with one element to define how the 
                                     # stock assesSMent model used ("NoAssesSMent").
       advice.ctrl = multiAdvC)      # A list with one element to define how the TAC advice 
                                     # is obtained ("IcesHCR").


## ----echo=TRUE, eval=TRUE------------------------------------------------
names(SM)


## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
SM$fleets <- setUnitsNA(SM$fleets)
SM_bio    <- bioSum(SM, long = TRUE)          # Data frame (DF) with biological indicators.
SM_adv    <- advSum(SM, long = TRUE)          # DF with indicators related with man. advice (TAC). 
SM_flt    <- fltSum(SM, long = TRUE)          # DF with indicators at fleet level.
SM_fltStk <- fltStkSum(SM, long = TRUE)       # DF with indicators at fleet and stock level.
SM_mt     <- mtSum(SM, long = TRUE)           # DF with indicators at fleet.
SM_mtStk  <- mtStkSum(SM, long = TRUE)        # DF with indicators at fleet and metier level.
SM_vessel <- vesselSum(SM, long = TRUE)       # DF with indicators at vessel level.
SM_vesselStk <- vesselStkSum(SM, long = TRUE) # DF with indicators at vessel and stock level.
SM_npv  <- npv(SM, y0 = '2014')               # DF with net present value per fleet over 
                                              # the selected range of years.
SM_risk <- riskSum(SM,  Bpa= c(stk1= 135000, stk2 = 124000), Blim =c(stk1 = 96000, stk2 = 89000),
                   Prflim = c(fl1 = 0, fl2 = 0), flnms = names(SM$fleets), 
                   years = dimnames(SM$biols[[1]]@n)[[2]], 
                   scenario = 'SM') # DF with risk indicators (pBlim, pBpa and pPrlim).


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
plot(SM$stocks[[1]]) # Stock 2 is in biomass, 'plot(SM$stocks[[2]])' function does not work.


## ----echo=TRUE, eval=TRUE, results = "hide"------------------------------
# set your own working directory.
#myWD <- "C:/use/escritorio/ExampleMulti/"
#setwd(myWD)
plotFLBiols(SM$biols, pdfnm = "SM")
plotFLFleets(SM$fleets, pdfnm ="SM")
plotfltStkSum(SM, pdfnm ="SM") 
plotEco(SM, pdfnm ='SM')


## ----echo=TRUE, fig.cex = 0.5 , eval=TRUE--------------------------------
inds <- c('catch','nVessels', 'effort', 'grossValue')
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

inds <- c('capacity', 'grossValue', 'costs', 'netProfit')
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
      multiFl[[i]]@metiers[[j]]@catches[[k]]@price <- 
        multiFl[[i]]@metiers[[j]]@catches[[k]]@price*1000
    }
  }
}  


## ----echo=TRUE, results = "hide"-----------------------------------------
SM <- FLBEIA(biols = multiBio,   SRs = multiSR,  BDs = multiBD, fleets = multiFl, covars = multiCv,
             indices = NULL,advice = multiAdv, main.ctrl = multiMainC, biols.ctrl = multiBioC,
             fleets.ctrl = multiFlC, covars.ctrl = multiCvC, obs.ctrl = multiObsC, assess.ctrl = multiAssC,
             advice.ctrl = multiAdvC)     


## ----echo=TRUE, fig.cex = 0.5 , eval=TRUE--------------------------------
SM$fleets <- setUnitsNA(SM$fleets)
SM_bio    <- bioSum(SM, long = TRUE)          # Data frame (DF) with biological indicators.
SM_adv    <- advSum(SM, long = TRUE)          # DF with indicators related with man. advice (TAC). 
SM_flt    <- fltSum(SM, long = TRUE)          # DF with indicators at fleet level.
SM_fltStk <- fltStkSum(SM, long = TRUE)       # DF with indicators at fleet and stock level.
SM_mt     <- mtSum(SM, long = TRUE)           # DF with indicators at fleet.
SM_mtStk  <- mtStkSum(SM, long = TRUE)        # DF with indicators at fleet and metier level.
SM_vessel <- vesselSum(SM, long = TRUE)       # DF with indicators at vessel level.
SM_vesselStk <- vesselStkSum(SM, long = TRUE) # DF with indicators at vessel and stock level.
SM_npv  <- npv(SM, y0 = '2014') # DF with net present value per fleet over a selected range of years.
SM_risk <- riskSum(SM, stknms = names(SM$biols), Bpa= c(stk1= 135000, stk2 = 124000), 
                   Blim =c(stk1 = 96000, stk2 = 89000),
                   Prflim = c(fl1 = 0, fl2 = 0), flnms = names(SM$fleets), 
                   years = dimnames(SM$biols[[1]]@n)[[2]], 
                   scenario = 'SM') #  DF with risk indicators (pBlim, pBpa and pPr).


## ----echo=TRUE, fig.cex = 0.5 , eval=TRUE--------------------------------
plotFLBiols(SM$biols, pdfnm = "SM_pricex1000")
plotFLFleets(SM$fleets, pdfnm ="SM_pricex1000")
plotfltStkSum(SM, pdfnm ="SM_pricex1000") 
plotEco(SM, pdfnm ='SM_pricex1000')


## ----echo=TRUE, fig.cex = 0.5 , eval=TRUE--------------------------------
inds <- c('capacity','costs', 'grossValue', 'gva')
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


## ----echo=TRUE, fig.cex = 0.5 , eval=TRUE--------------------------------
inds <- c('ssb','rec', 'biomass', 'catch')
d <- rbind(subset(SM_bio,indicator  %in% inds ))
d$indicator <- factor( d$indicator, levels=inds)
d$scenario <- factor(d$scenario)
d$year <- as.numeric(d$year)
p <- ggplot( data=d, aes(x=year, y=value, color=stock)) + 
  geom_line() + 
  facet_wrap(~ indicator, scales="free") + 
  geom_vline(xintercept = multiMainC$sim.years[['initial']], linetype = "longdash") + 
  theme_bw() + 
  theme(text=element_text(size=10),
        title=element_text(size=10,face="bold"),
        strip.text=element_text(size=10)) + 
  ylab("")
print(p)


## ----echo=TRUE, fig.cex = 0.5, eval=TRUE---------------------------------
plotFLBiols(SM$biols, pdfnm = "SM_pricex1000")
plotFLFleets(SM$fleets, pdfnm ="SM_pricex1000")
plotfltStkSum(SM, pdfnm ="SM_pricex1000") 
plotEco(SM, pdfnm ='SM_pricex1000')

inds <- c('tac')
d <- rbind(subset(SM_adv,indicator  %in% inds ))
d$indicator <- factor( d$indicator, levels=inds)
d$scenario <- factor(d$scenario)
d$year <- as.numeric(d$year)
p <- ggplot( data=d, aes(x=year, y=value, color=stock)) + 
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
La0_met1 <- multiFl$fl1@metiers$met1@catches$stk2@landings.n[,as.character(2008),,,] * 
                multiFl$fl1@metiers$met1@catches$stk2@landings.wt[,as.character(2008),,,]
La0_met2 <- multiFl$fl1@metiers$met2@catches$stk2@landings.n[,as.character(2008),,,] * 
                multiFl$fl1@metiers$met2@catches$stk2@landings.wt[,as.character(2008),,,]
pd.La0 <- unitSums(La0_met1 +La0_met2) 
                   
multiFlC_1$fl1$stk2$pd.La0 <- array(pd.La0, dim = c(1,4, 1),
                                   dimnames= list(age = 'all', season = c(1:4), iter = 1))

# Reference price
Pa0_met1 <- multiFl$fl1@metiers$met1@catches$stk2@price[,as.character(2008),,,] 
Pa0_met2 <- multiFl$fl1@metiers$met2@catches$stk2@price[,as.character(2008),,,]  
pd.Pa0 <- unitMeans((La0_met1*Pa0_met1 +La0_met2*Pa0_met2)/(La0_met1+La0_met2))

multiFlC_1$fl1$stk2$pd.Pa0 <- array(pd.Pa0,  dim = c(1,4, 1),
                                   dimnames= list(age = 'all', season = c(1:4), iter = 1))

multiFlC_1$fl1$stk2$pd.total <- TRUE # If TRUE: price is calculated using total landings, and 
                                     # if FALSE: fleet's landings are used to estimate the price.


SM_1 <- FLBEIA(biols = multiBio, SRs = multiSR, BDs = multiBD, fleets = multiFl_1,
               covars = multiCv, indices = NULL, advice = multiAdv, main.ctrl = multiMainC,
               biols.ctrl = multiBioC, fleets.ctrl = multiFlC_1, covars.ctrl = multiCvC,
               obs.ctrl = multiObsC, assess.ctrl = multiAssC, advice.ctrl = multiAdvC)


## ----echo=TRUE, eval=TRUE------------------------------------------------
SM_1$fleets <- setUnitsNA(SM_1$fleets)
SM_1_fltStk <- fltStkSum(SM_1, scenario ='elasticPrice', long = TRUE) 
SM_x <- rbind(SM_fltStk, SM_1_fltStk)

inds <- c('price', 'catch', 'effort', 'capacity')
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
SM_1_flt <- fltSum(SM_1, scenario ='elasticPrice', long = TRUE) 
SM_x <- rbind(SM_flt, SM_1_flt)
SM_x <- subset(SM_x, fleet == 'fl1')

inds <- c('grossValue','costs','salaries','netProfit')
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
SM_2$fleets <- setUnitsNA(SM_2$fleets)
SM_2_flt    <- fltSum(SM_2, scenario = 'SM_2', long = TRUE)
SM_x <- rbind(SM_flt, SM_2_flt)

inds <- c('costs','fcosts','grossValue','netProfit')
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
SM_3$fleets <- setUnitsNA(SM_3$fleets)
SM_3_flt    <- fltSum(SM_3, scenario = 'SM_3', long = TRUE)

inds <- c('costs','vcosts','grossValue','netProfit')
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
SM_4$fleets <- setUnitsNA(SM_4$fleets)
SM_4_flt    <- fltSum(SM_4, scenario = 'SM_4', long = TRUE)

inds <- c('costs','vcosts','grossValue','netProfit')
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

# SCD2
SCD2 <- 
  function (fleets, covars, fleets.ctrl, flnm, year = 1, season = 1, 
  ...) 
{
  fleet <- fleets[[flnm]]
  ny <- dim(fleet@effort)[2]
  ns <- dim(fleet@effort)[4]
  it <- dim(fleet@effort)[6]
  VaC <- seasonSums(totvcost_flbeia(fleet)[, year])
  FxC <- seasonSums(covars[["NumbVessels"]][flnm, ] * fleet@fcost)[, 
    year]
  FuC <- seasonSums(covars[["FuelCost"]][flnm, ] * fleet@effort)[, 
    year]
  CaC <- seasonMeans((covars[["CapitalValue"]][flnm, ] * covars[["NumbVessels"]][flnm, 
    ]))[, year]
  Rev <- revenue_flbeia(fleet)[, year]
  Rev <- ifelse(Rev == 0, 1e-16, Rev)
  CrC <- seasonSums((Rev * fleet@crewshare[, year] + covars[["Salaries"]][flnm, 
    year]))
  Rev <- seasonSums(Rev)
  x1 <- FuC/Rev
  units(VaC) <- units(Rev)

  x2 <- VaC/Rev
  a <- CrC + FxC + CaC
  b <- 1 - x1 - x2
units(a) <- units(b)
  BER <- a/b
  Inv <- c((Rev - BER)/Rev) * c(covars[["InvestShare"]][flnm, 
    year, , ns])
  Inv <- ifelse((Rev - BER) < 0 & Rev < 0, -Inv, Inv)
  Ks <- fleet@capacity[, year][drop = T]
  K <- c(seasonSums(fleet@capacity[, year]))
  if (ns == 1) 
    pKs <- rep(1, it)
  else if (it > 1) 
    pKs <- sweep(Ks, 2, K, "/")
  else pKs <- Ks/K
  w1 <- c(covars[["w1"]][flnm, year, , ns])
  w2 <- c(covars[["w2"]][flnm, year, , ns])
  omega <- ifelse(Inv < 0, ifelse(-Inv < w1, Inv * K, -w1 * 
    K), ifelse(Inv < w2, Inv * K, w2 * K))
  Ef <- c(seasonSums(fleet@effort[, year]))
  iterSel <- which(omega > 0 & Ef < 0.99 * K)
  omega[iterSel] <- 0
  if (year < ny) {
    fleets[[flnm]]@capacity[, year + 1] <- Ks + omega * 
      pKs
    covars[["NumbVessels"]][flnm, year + 1, ] <- fleets[[flnm]]@capacity[, 
      year + 1]/(covars[["MaxDays"]][flnm, year + 1, ])
  }
  return(list(fleets = fleets, covars = covars))
}


# fl1 has fixed effort
multiFlC_5 <- multiFlC
multiFlC_5$fl2$capital.model <- multiFlC_5$fl2$capital.model <- 'SCD2'

multiCv_5 <- multiCv
multiCv_5$w1[] <- 0.01  
multiCv_5$w2[] <- 0.01
multiCv_5$InvestShare[] <- 0.02
  
SM_5 <- FLBEIA(biols = multiBio, SRs = multiSR, BDs = multiBD, fleets = multiFl,
                covars = multiCv_5, indices = NULL, advice = multiAdv, main.ctrl = multiMainC,
                biols.ctrl = multiBioC, fleets.ctrl = multiFlC_5, covars.ctrl = multiCvC,
                obs.ctrl = multiObsC, assess.ctrl = multiAssC, advice.ctrl = multiAdvC)


## ----echo=TRUE, eval=TRUE------------------------------------------------
SM_5$fleets <- setUnitsNA(SM_5$fleets)
SM_5_flt    <- fltSum(SM_5, scenario = 'SM_5', long = TRUE)
SM_x <- rbind( SM_flt, SM_5_flt )

inds <- c("capacity",'nVessels','effort','netProfit')
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
## scnms <-c('SM', 'SM_1', 'SM_2', 'SM_3', 'SM_4', 'SM_5')
## stknms <- unique(SM_bio$stock)
## RefPts <- expand.grid(indicator=c("Bmsy", "Fmsy", "Bpa", "Blim", "Fpa", "Flim"),
##                       scenario=scnms, stock=stknms, value=NA)[,c(3,2,1,4)]
## RefPts$value <- c(c(800, 0.11, 800, 550, 0.25, 0.50), c(800, 0.2, 800, 550, 0.25, 0.50),
##                   c(800, 0.2, 800, 550, 0.25, 0.50), c(800, 0.2, 800, 550, 0.25, 0.50),
##                   c(800, 0.2, 800, 550, 0.25, 0.50), c(800, 0.2, 800, 550, 0.25, 0.50))
## 
## flbeiaObjs <- list( SM = SM, SM_1 = SM_1, SM_2 = SM_2, SM_3 = SM_3,
##                     SM_4 = SM_4, SM_5 = SM_5)
## 
## 
## flbeiaApp(flbeiaObjs = flbeiaObjs, RefPts = RefPts, years = ac(2000:2025),
##           calculate_npv = TRUE, npv.y0 = '2012', npv.yrs = ac(2013:2025))

