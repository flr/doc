## ---- ini, echo=FALSE, results='hide', message=FALSE--------------------------
# This chunk set the document environment, so it is hidden
library(knitr)
source("R/ini.R")
set.seed(1423)


## ----echo=FALSE, out.width='20%'----------------------------------------------
include_graphics('images/FLBEIA_logo.png')


## ---- include=TRUE, echo=FALSE------------------------------------------------
library(spict)
library(FLBEIA)
library(MASS)
library(corrplot)
library(ggplot2)
library(gtools)
library(fishmethods)


## ---- avoidError, echo=FALSE, results='hide', message=FALSE-------------------
# Avoid error if kobe library loaded
density <- stats::density


## ----data, echo = FALSE, include = FALSE--------------------------------------
data(mur)

catch_long <- reshape(catch, direction = 'long', varying = names(catch)[2:8], sep = "_")
names(catch_long)[2:3] <- c('area', 'catch') 


## ----fig:catch, echo=FALSE, fig.height= 7/2.54, fig.width =  11/2.54, fig.cap = 'Catch time series by area.'----
ggplot(data = catch_long, aes(x = year, y = catch, colour = area)) + geom_line(lwd=1)


## ----fig:evhoe, echo=FALSE, fig.height= 7/2.54, fig.width =  11/2.54, fig.cap = 'EVHOE abundance index. Shaded area corresponds with two times the annual standard deviation of the index.'----
ggplot(evhoe, aes(x = year, y = biomass)) + geom_line(col=2, lwd = 1) +
    geom_ribbon(aes(ymin = evhoe$biomass-1.96*evhoe$std, ymax = evhoe$biomass+1.96*evhoe$std), alpha=0.2, fill = 2) 


## ----spict data---------------------------------------------------------------
murDat <- list(obsC  = catch[,'area_total'],
               timeC = catch[,'year'],
               obsI  = evhoe[,'biomass'],
               timeI  = evhoe[,'year'])

murInp <- check.inp(murDat)

murInp


## ----initial fit--------------------------------------------------------------
 mur_spict <- fit.spict(murInp)
 capture.output(summary(mur_spict))


## ----fig:est_bio, fig.height= 10/2.54, fig.width =  16/2.54, fig.cap = 'Relative and Absolute  Biomasses estimated by SPiCT. Horizontal line correspond with the biomass level at MSY.'----
 par(mfrow = c(1,2))
 plotspict.bbmsy(mur_spict, qlegend = FALSE)
 plotspict.biomass(mur_spict, qlegend = FALSE)


## ----fig:est_f, fig.height= 10/2.54, fig.width =  16/2.54, fig.cap = 'Relative and Absolute  fishing mortalities estimated by SPiCT. Horizontal line correspond with the MSY fishing mortality level.'----
 par(mfrow = c(1,2))
 plotspict.f(mur_spict, qlegend = FALSE)
 plotspict.ffmsy(mur_spict, qlegend = FALSE)


## ----fig:est_catch, fig.height= 10/2.54, fig.width =  8/2.54, fig.cap = 'Catch estimated by SPiCT. The horizontal line corresponds with the MSY.'----
 plotspict.catch(mur_spict, qlegend = FALSE)


## ----spict ff-----------------------------------------------------------------
murDat$obsC  <- murDat$obsC[23:40]
murDat$timeC <- murDat$timeC[23:40]
mur_spict <- fit.spict(murDat)


## ----fig:ff_biomass, fig.height= 10/2.54, fig.width =  16/2.54, fig.cap = 'Relative Biomass estimated by SPiCT with shortened time series.'----
par(mfrow = c(1,2))
 plotspict.bbmsy(mur_spict)
 plotspict.biomass(mur_spict, qlegend = FALSE, stamp = F)


## ----fig:ff_f, fig.height= 10/2.54, fig.width =  16/2.54, fig.cap = 'Absolute fishing mortality estimated by SPiCT.'----
par(mfrow = c(1,2))
 plotspict.f(mur_spict, qlegend = FALSE)
 plotspict.ffmsy(mur_spict, qlegend = FALSE)



## ----fig:ff_catch, fig.height= 10/2.54, fig.width =  14/2.54, fig.cap = 'Catch estimated by SPiCT using the shortened time series. The horizontal line corresponds with the MSY.'----
 plotspict.catch(mur_spict, qlegend = FALSE)


## ----fig:kobe, fig.height= 10/2.54, fig.width =  16/2.54, fig.cap = 'Production curve estimated by SPiCT using the shortened time series.'----
par(mfrow = c(1,2))
plotspict.production(mur_spict)
plotspict.fb(mur_spict)
par(mfrow = c(1,1))


## ----varcov-------------------------------------------------------------------
varcov <- (mur_spict$cov.fixed)
params <- mur_spict$par.fixed
cor    <- cov2cor(mur_spict$cov.fixed)


## ----fig:vcov, fig.height= 8/2.54, fig.width =  8/2.54, fig.cap = 'Graphical representation of the correlation matrix obtained in the final SPiCT fit.'----
corrplot(cor, method = 'ellipse')


## -----------------------------------------------------------------------------
set.seed(27)
RandPar_SPict_log <- mvrnorm(1000, params, varcov)
RandPar_SPict     <- exp(RandPar_SPict_log)
colnames(RandPar_SPict) <- substr(colnames(RandPar_SPict),4, nchar(RandPar_SPict)) 


## -----------------------------------------------------------------------------
RandPar_flbeia     <- matrix(NA, 1000,3, dimnames = list(iter = 1:1000, c('r', 'K', 'p')))

# Growth parameter r
RandPar_flbeia[,1] <- (RandPar_SPict[,'m']*RandPar_SPict[,'n']^(RandPar_SPict[,'n']/(RandPar_SPict[,'n']-1))) /
                      RandPar_SPict[,'K'] 
# K
RandPar_flbeia[,2] <- RandPar_SPict[,'K'] 
# p
RandPar_flbeia[,3] <- RandPar_SPict[,'n'] - 1
# Remove not viable iterations
remiter <- unique(c(which((RandPar_flbeia[,'p']/RandPar_flbeia[,'r']) <  -1), 
                    which(RandPar_flbeia[,'r']>  2)))
RandPar_flbeia <- RandPar_flbeia[-remiter,]
# Identify the valid iterations and select the first 100.
Niter <- Nit <- 20
valid_iters <-  as.numeric(dimnames(RandPar_flbeia)[[1]])[1:Niter]


## ----fig:params, fig.height= 14/2.54, fig.width =  16/2.54, fig.cap = 'Density curves of the production model parameters used to condition the initial population.'----
par(mfrow = c(2,2))
plot(density(RandPar_flbeia[,1]), main = 'Intrinsic Growth Rate (r)', xlab = "", lwd = 2)
abline(v = median(RandPar_flbeia[,1]), col = 2)
plot(density(RandPar_flbeia[,2]), main = 'Carrying Capacity (K)', xlab = "", lwd = 2)
abline(v = median(RandPar_flbeia[,2]), col = 2)
plot(density(RandPar_flbeia[,3]+1), main = 'Shape of Production Curve (n)', xlab = "", lwd = 2)
abline(v = median(RandPar_flbeia[,3]+1), col = 2)
par(mfrow = c(1,1))


## ---- message=FALSE-----------------------------------------------------------
Best <- Cest <- matrix(NA, Niter, 18, dimnames = list(iter = 1:Niter, year = 1997:2014))

for(i in 1:Niter){
  # The data is the same use in the base fit.
  murDat_rand           <- murDat
  # Use as initial parameters those obtained in the sampling.
  murDat_rand$ini       <- as.list(RandPar_SPict_log[i,1:7])
  # Tell SPiCT to keep fixed those parameters. We let SPiCT adjusting one of the variances
  # because otherwise the fit crashes.
  ph   <-  RandPar_SPict_log[i,1:7]
  ph[] <- -1
  murDat_rand$phases    <- as.list(ph) 
  # Fit SPiCT
  mur_rand_fit <- fit.spict(murDat_rand)
  # Extract the parameters.
  Best[i,]  <- get.par("logB", mur_rand_fit, exp = TRUE)[(0:17)*16+1,2]
  Cest[i,]  <- get.par("logCpred", mur_rand_fit, exp = TRUE)[,2]
}


## ----fig:randBC, fig.height= 14/2.54, fig.width =  16/2.54, fig.cap = 'Time series of the biomass and catch obtained in each of the iterations.'----
RandPar_flbeia <- RandPar_flbeia[1:Niter,]

par(mfrow = c(2,1))
matplot(1997:2014,t(Best), type = 'l', main = 'Biomass', ylab = 'MT', xlab = "", lty = 1)
matplot(1997:2014,t(Cest), type = 'l', main = 'Catch', ylab = 'MT', xlab = "", lty = 1)



## ----SRR----------------------------------------------------------------------
steepness <- 0.95
virginBio <- 15*max(murDat$obsC,na.rm = TRUE)
spr0      <- 0.25 # contrast with an existing fit.


## ----FLSRsim------------------------------------------------------------------
sr_params <- unlist(abPars(s = steepness, v = virginBio, spr0 = spr0, model = 'bevholt'))

sr <- FLSR(name = 'mur', params = FLPar(unlist(sr_params)), model = 'bevholt')
sr@params <-  FLPar(unlist(sr_params))
sr@params[2] <- sr@params[2]


## ----VBert--------------------------------------------------------------------
Linf <- 37.7
K    <- 0.29
t0   <- 0
VBert <- function(age, Linf, K, t0) return(Linf*(1-exp(-K*(age-t0))))


## ----lw_rel-------------------------------------------------------------------
lw_a <- 0.016
lw_b <- 2.91


## ----mwa----------------------------------------------------------------------
mwa <- lw_a*VBert((0:10)+0.5, Linf, K, t0)^lw_b


## ----maturity-----------------------------------------------------------------
na <- 11
a2 <- 1/7.88
a1 <- (19.44-15.5*2)*a2
mat0 <- 0
mat1 <- a1+13.29*a2   # 13.29 the length at one year and half a year age.  
mat <- c(mat0, mat1, rep(1,na-2)) 


## ----natmort, results = TRUE--------------------------------------------------
Ms <- M.empirical(Linf = Linf, 
            Kl = K, TC = 16, tmax = 10,
            tm = 0.5, method = c(1,  3, 4, 5, 10, 11))

Ms

M <- mean(mean(Ms[-5,]))


## ----C1999--------------------------------------------------------------------
catch[25,8]<- mean(catch[26,8], catch[24,8]) 


## ----pristineBio--------------------------------------------------------------
pristineBio <- c(prod(sr_params[1])*exp(-(0:9)*M),sum(c(sr_params[1])*exp(-(10:100)*M)))


## ----catch_profile_w----------------------------------------------------------
caw_prop <- c(age0 = 0.000, age1 = 0.194, age2 = 0.172, age3 = 0.335, age4 = 0.193, 
              age5 = 0.031, age6 = 0.019, age7 = 0.032, age8 = 0.008, age9 = 0.006, 
              age10 = 0.011)


## ----ca75---------------------------------------------------------------------
caw75 <- catch[1,8]*caw_prop
ca75  <- caw75/(mwa/1000)


## ----fa1975-------------------------------------------------------------------
fa75 <- numeric(11)

fobj <- function(Fa,Ma,Na,Ca) (((Fa/(Fa+Ma))*(1-exp(-(Fa+Ma)))*Na) - Ca)

for(a in 0:10){
  fa75[a+1] <- uniroot(fobj,c(0,1e10), Ma = M, Na =   pristineBio[a+1],Ca = ca75[a+1])$root
}


## ----stk----------------------------------------------------------------------
mq <- FLQuant(M, dim = c(11,40,1,1,1,Niter), 
              dimnames = list(age = 0:10, year = 1975:2014, unit = 'unique', 
                              season = 'all', area = 'unique', iter = 1:Niter))
matq <- FLQuant(mat, dim = c(11,40,1,1,1,Niter), 
                dimnames = list(age = 0:10, year = 1975:2014, unit = 'unique', 
                                season = 'all', area = 'unique', iter = 1:Niter))
mwaq <- FLQuant(mwa/1000, dim = c(11,40,1,1,1,Niter), 
                dimnames = list(age = 0:10, year = 1975:2014, unit = 'unique', 
                                season = 'all', area = 'unique', iter =1:Niter))
hspwn <- mspwn <- dnq <-  FLQuant(0, dim = c(11,40,1,1,1,Niter), 
                                  dimnames = list(age = 0:10, year = 1975:2014, 
                                                  unit = 'unique', season = 'all', 
                                                  area = 'unique', iter = 1:Niter))
catch.flq <- FLQuant(catch[,8], dim = c(1,40,1,1,1,Niter), 
                     dimnames = list(quant = 'all', year = 1975:2014, unit = 'unique', 
                                     season = 'all', area = 'unique', iter = 1:Niter))

sel         <- fa75/max(fa75)

# beta parameters
alpha1 <- (1-sel[3]*(1+0.001^2))/0.001^2
alphas <- (alpha1/sel[3])*sel[-(1:2)]
sel <- cbind(0,1,(rdirichlet(Niter*40,alphas)))
sel <- array(t(sel),c(11,40,Niter))

harvest <- FLQuant(sel, dim = c(11,40,1,1,1,Niter), 
                   dimnames = list(age = 0:10, year = 1975:2014, unit = 'unique', 
                                   season = 'all', area = 'unique', iter = 1:Niter))

stk <- FLStock(name = 'mur', mwaq, catch = catch.flq, catch.wt = mwaq, landings.wt = mwaq, 
               discards.wt = mwaq, stock.wt = mwaq, m = mq, mat = matq, harvest.spwn = hspwn, 
               m.spwn = mspwn, discards.n = dnq, harvest = harvest)

units(harvest(stk)) <- 'f'


## ----brp----------------------------------------------------------------------
brp_lh <- ypr(age = 0:10, wgt = mwa/1000,partial = fa75/max(fa75), M = M, plus = TRUE, 
              oldest = 10, maxF = 10, incrF = 0.01, graph = FALSE)


## ----numbers_F----------------------------------------------------------------
fobj <- function(fmult,sel,n0,w0,m0,c0){
    f0 <- fmult*sel
    z0 <- f0 + m0
    return((sum((f0/z0)*(1-exp(-z0))*n0*w0) - c0))
}

stk.sc0             <- stk
stk.sc0@stock.n[,1] <- pristineBio

for(i in 1:Niter){
  for(yr in 2:40){
    n0 <- stk.sc0@stock.n[,yr-1,,,,i, drop=T]
    w0 <- stk.sc0@stock.wt[,yr-1,,,,i,drop=T]
    m0 <- stk.sc0@m[,yr-1,,,,i,  drop=T]
    c0 <- stk.sc0@catch[,yr-1,,,,i,  drop=T]
    sel <- stk.sc0@harvest[,yr-1,,,,i]
    stk.sc0@harvest[,yr-1,,,,i] <- sel*uniroot(fobj, c(0,1e100), sel, n0, w0, m0, c0)$root
  
  z0 <- m0 + stk.sc0@harvest[,yr-1,,,,i]

  stk.sc0@stock.n[-c(1,na),yr,,,,i] <- stk.sc0@stock.n[-c(na-1,na),yr-1,,,,i]*
                                        exp(-z0[-c(na-1,na),,,,])
  stk.sc0@stock.n[na,yr,,,,i]       <- stk.sc0@stock.n[na-1,yr-1,,,,i]*exp(-z0[na-1,,,,,]) + 
                                        stk.sc0@stock.n[na,yr-1,,,,i]*exp(-z0[na,,,,,])
  stk.sc0@stock.n[1,yr,,,,i]        <- ssb(stk.sc0)[,yr-1,,,,i, drop=T]*sr_params[1]/
                                        (ssb(stk.sc0)[,yr-1,,,,i, drop=T] + sr_params[2])
  stk.sc0@catch.n[,yr-1,,,,i] <- (stk.sc0@harvest[,yr-1,,,,i]/z0)*(1-exp(-z0))*
                                  stk.sc0@stock.n[,yr-1,,,,i] 
        
  }}


## -----------------------------------------------------------------------------
stk.sc0 <- (window(stk.sc0,1978,2028))


## -----------------------------------------------------------------------------
flq  <- FLQuant(1, dim = c(1,51,1,1,1,Niter), 
                dimnames = list(quant = 'all', year = 1978:2028, unit = 'unique', 
                                season = 'all', area = 'unique', iter = 1:Niter))
flq0 <- FLQuant(0, dim = c(1,51,1,1,1,Niter), 
                dimnames = list(quant = 'all', year = 1978:2028, unit = 'unique', 
                                season = 'all', area = 'unique', iter = 1:Niter))

flqa  <- FLQuant(1, dim = c(11,51,1,1,1,Niter), 
                 dimnames = list(age = 0:10, year = 1978:2028, unit = 'unique', 
                                 season = 'all', area = 'unique', iter = 1:Niter))
flqa0 <- FLQuant(0, dim = c(11,51,1,1,1,Niter), 
                 dimnames = list(age = 0:10, year = 1978:2028, unit = 'unique', 
                                 season = 'all', area = 'unique', iter = 1:Niter))


## -----------------------------------------------------------------------------
murBD <- FLBDsim(name = 'mur', desc = 'Striped Red Mullet in Bay of Biscay', 
                 biomass= flq, catch = flq, uncertainty = flq, gB = flq)

murBD@biomass[,ac(1997:2014)]      <- t(Best)
murBD@catch[,ac(1997:2014)]        <- t(Cest)
murBD@uncertainty[,ac(2014:2028)]  <- rlnorm(Niter*15,0,RandPar_SPict[valid_iters,'sdb']) 
murBD@params[] <- expand(FLQuant(t(RandPar_flbeia[1:Niter,c(1,3,2)]),dim = c(3,1,1,1,1,Niter), 
                                 dimnames = list(par = c('r', 'K', 'p'), iter = 1:Niter)), 
                         year = 1978:2028)
murBD@alpha    <- array((murBD@params['p',,,]/murBD@params['r',,,]+1)^
                          (1/murBD@params['p',,,]), dim = c(51,1,Niter))


## -----------------------------------------------------------------------------
# Correct the catches in 2014 so that C14 < "B14*catch.thres + g(B14)*unc"
r <- murBD@params['r',1,,]
p <- murBD@params['p',1,,]
K <- murBD@params['K',1,,]
B14  <- murBD@biomass[,'2014',drop=T]
unc  <- murBD@uncertainty[,'2014']
gB14 <-  (B14*(r/p) * (1 - (B14/K)^p)*unc)[drop=T]
C14  <- murBD@catch[,'2014',drop=T]
if(any((B14 + gB14)/C14 < 1)) flag <- "TRUE"
C14  <- ifelse((B14 + gB14)/C14 < 1, (B14 + gB14)*0.9,C14)

murBD@gB[,ac(2014)]    <- gB14
murBD@catch[,ac(2014)] <- C14


## -----------------------------------------------------------------------------
murSR <- FLSRsim(name = 'mur', desc = 'Striped Red Mullet in Bay of Biscay', ssb= flq, 
                 model = 'bevholt')

murSR@ssb[]      <- ssb(stk.sc0)
murSR@rec[]      <- stk.sc0@stock.n[1,]
murSR@uncertainty[]  <- rlnorm(Niter*51,0,.30) 
murSR@params[]       <- sr@params 


## -----------------------------------------------------------------------------
biols.bd <- FLBiols(mur = FLBiol(name = 'mur', 
                              desc = 'Striped Red Mullet in Bay of Biscay',
                             range = c(min = 1, max = 1, plusgroup = 1, minyear = 1978, 
                                       maxyear = 2028, minfbar = 1, maxfbar = 1),
                              n    = murBD@biomass, 
                              wt   = flq, 
                              fec  = predictModel(mat = flq, model = ~mat), 
                              mat  = predictModel(mat = flq, model = ~mat), 
                              m    = flq
                             ))


## -----------------------------------------------------------------------------
biols.age <- FLBiols(mur = FLBiol(name = 'mur', 
                              desc = 'Striped Red Mullet in Bay of Biscay',
                             range = c(min = 0, max = 10, plusgroup = 10, minyear = 1978, 
                                       maxyear = 2028, minfbar = 1, maxfbar = 2),
                              n    = stk.sc0@stock.n, 
                              wt   = stk.sc0@stock.wt, 
                              fec  = predictModel(mat = stk.sc0@mat, model = ~mat), 
                              mat  = predictModel(mat = stk.sc0@mat, model = ~ mat), 
                              m    = stk.sc0@m,
                             spwn  = flqa0
                             ))

m(biols.age[[1]])[,ac(2015:2028)]   <- m(biols.age[[1]])[,ac(2014)]
fec(biols.age[[1]])[,ac(2015:2028)] <- fec(biols.age[[1]])[,ac(2014)]
mat(biols.age[[1]])[,ac(2015:2028)] <- mat(biols.age[[1]])[,ac(2014)]
wt(biols.age[[1]])[,ac(2015:2028)]  <- wt(biols.age[[1]])[,ac(2014)]


## -----------------------------------------------------------------------------
cc <- FLCatchExt(name = 'mur', 
                 alpha = flq, beta = flq, 
                 landings = murBD@catch, landings.n = murBD@catch, landings.wt = flq, 
                 discards = flq0, discards.n = flq0, discards.wt = flq, 
                 landings.sel = flq, discards.sel = flq0)


## -----------------------------------------------------------------------------
fleets.bd <- FLFleetsExt(fl = FLFleetExt(name = 'fl', effort= flq, capacity = flq*1e12,
                           metiers = FLMetiersExt(mt = FLMetierExt(name = 'mt', effshare = flq, 
                                     catches = FLCatchesExt(mur = cc)))))
fleets.bd[[1]]@metiers[[1]]@catches[[1]]@catch.q <- murBD@catch/murBD@biomass

fleets.bd[[1]]@metiers[[1]]@catches[[1]]@catch.q[,ac(2015:2028)] <- 
  expand(yearMeans(fleets.bd[[1]]@metiers[[1]]@catches[[1]]@catch.q[,ac(2005:2014)]), 
         year = 2015:2028) 


## -----------------------------------------------------------------------------
cc <- FLCatchExt(name = 'mur', 
                 alpha = flqa, beta = flqa, 
                 landings = stk.sc0@catch, landings.n = stk.sc0@catch.n, 
                 landings.wt = biols.age[['mur']]@wt, 
                 discards = flq0, discards.n = flqa0, 
                 discards.wt = biols.age[['mur']]@wt, 
                 landings.sel = flqa, discards.sel = flqa0)

fleets.age <- FLFleetsExt(fl = FLFleetExt(name = 'fl', effort= flq, capacity = flq*1e12,
                           metiers =  FLMetiersExt(mt = FLMetierExt(name = 'mt', 
                                     effshare = flq, catches = FLCatchesExt(mur = cc)))))

fleets.age[[1]]@metiers[[1]]@catches[[1]]@catch.q[,ac(1978:2014)] <- stk.sc0@harvest[,ac(1978:2014)]

fleets.age[[1]]@metiers[[1]]@catches[[1]]@catch.q[,ac(2015:2028)] <- 
  expand(yearMeans(fleets.age[[1]]@metiers[[1]]@catches[[1]]@catch.q[,ac(2005:2014)]), 
         year = 2015:2028) 


## -----------------------------------------------------------------------------
indices <- FLIndices(evhoe = FLIndex(name = 'mur', catch.wt = flq, effort = flq, index = flq))
indices[[1]]@index.q[]     <- rep(RandPar_SPict[valid_iters[1:Niter],'q'], each = 51)
indices[[1]]@index[]       <- indices[[1]]@index.q[]*murBD@biomass
# 30% CV 
sigma <- sqrt(log(0.3^2+1))
indices[[1]]@index.var[]   <- rlnorm(51*Niter, 0, sigma)


## -----------------------------------------------------------------------------
indices.age <- indices 
indices.age[[1]]@index[] <- indices.age[[1]]@index.q[]*quantSums(wt(biols.age[[1]])*n(biols.age[[1]]))


## -----------------------------------------------------------------------------
# Advice Object 
advice <- list(TAC = murBD@catch,  quota.share = list(mur = flq))
dimnames(advice$TAC)[[1]] <- 'mur'
quant(advice$TAC) <- 'stock'
advice$TAC[,'2015'] <- mean(murDat$obsC[16:18]) # There is no TAC => last three year mean.
dimnames(advice$quota.share[[1]])[[1]] <- 'fl'


## -----------------------------------------------------------------------------
main.ctrl  <- list(sim.years = c(initial = '2015', final = '2025'))


## -----------------------------------------------------------------------------
biols.ctrl.bd  <- create.biols.ctrl(stksnames = 'mur', growth.model = 'BDPG')
biols.ctrl.age <- create.biols.ctrl(stksnames = 'mur', growth.model = 'ASPG')


## -----------------------------------------------------------------------------
fleets.ctrl.bd <- create.fleets.ctrl(fls = 'fl', fls.stksnames = list(fl = 'mur'), flq = flq, 
                                     effort.models = c(fl = 'SMFB'), n.fls.stks = c(fl = 1), 
                                     capital.models = c(fl = 'fixedCapital'), 
                                     price.models = c(fl = 'fixedPrice'), 
                                     catch.models = c('CobbDouglasBio'))

fleets.ctrl.age <- create.fleets.ctrl(fls = 'fl', fls.stksnames = list(fl = 'mur'), flq = flq, 
                                      effort.models = c(fl = 'SMFB'), n.fls.stks = c(fl = 1), 
                                      capital.models = c(fl = 'fixedCapital'), 
                                      price.models = c(fl = 'fixedPrice'), 
                                      catch.models = c('CobbDouglasAge'))


## -----------------------------------------------------------------------------
obs.ctrl.ind <- create.obs.ctrl(stksnames = 'mur', n.stks.inds = c(mur = 1), 
                                stks.indsnames = 'evhoe',indObs.models = c(mur = 'bioInd'))
obs.ctrl.ind[['mur']][['stkObs']][['stkObs.model']] <- 'NoObsStock'

obs.ctrl.stk <- create.obs.ctrl(stksnames = 'mur', n.stks.inds = c(mur = 1), 
                                stks.indsnames = 'evhoe', stkObs.models = c(mur = 'perfectObs'))
obs.ctrl.stk[['mur']][['indObs']][['evhoe']] <- 'NoObsIndex'


## -----------------------------------------------------------------------------
assess.ctrl <- create.assess.ctrl(stksnames = 'mur', assess.models = 'NoAssessment')


## -----------------------------------------------------------------------------
advice.ctrl.dls3 <- create.advice.ctrl(stksnames = 'mur', HCR.models = 'annexIVHCR', 
                                       index = 'evhoe', iter = Niter)
advice.ctrl.dls3$mur$index <- 'evhoe'


## -----------------------------------------------------------------------------
advice.ctrl.little <- advice.ctrl.dls3
advice.ctrl.little$mur$HCR.model <- 'little2011HCR'
advice.ctrl.little[['mur']][['ref.pts']] <- 
  matrix(NA, 4,Niter, dimnames = list(c('Ctarg', 'Ilim', 'Itarg', 'Cmax'), 1:Niter))
advice.ctrl.little[['mur']][['ref.pts']]['Ctarg',] <- mur_spict$report$MSY
advice.ctrl.little[['mur']][['ref.pts']]['Ilim',]  <- min(murDat$obsI, na.rm = TRUE)*1.25
advice.ctrl.little[['mur']][['ref.pts']]['Itarg',] <- 
  mur_spict$report$Bmsy*median(indices[[1]]@index.q[,1])
advice.ctrl.little[['mur']][['ref.pts']]['Cmax',]  <- mur_spict$report$MSY


## -----------------------------------------------------------------------------
advice.ctrl.msy <- create.advice.ctrl(stksnames = 'mur', HCR.models = 'IcesHCR', first.yr = 2014, last.yr = 2025, iter = Niter)
advice.ctrl.msy$mur$AdvCatch         <- c(rep(FALSE, 38), rep(TRUE, 13))
names(advice.ctrl.msy$mur$AdvCatch)  <- 1978:2028
advice.ctrl.msy$mur$ref.pts['Blim',]     <- 
  min(get.par("logB", mur_spict, exp = TRUE)[(0:17)*16+1,2])
advice.ctrl.msy$mur$ref.pts['Btrigger',] <- 
  min(get.par("logB", mur_spict, exp = TRUE)[(0:17)*16+1,2])*1.4
advice.ctrl.msy$mur$ref.pts['Fmsy',]     <- mur_spict$report$Fmsy*0.75


## -----------------------------------------------------------------------------
advice.ctrl.msy.pa <- advice.ctrl.msy
advice.ctrl.msy.pa$mur$ref.pts['Fmsy',]  <- mur_spict$report$Fmsy/1.4

advice.ctrl.little.pa <- advice.ctrl.little
advice.ctrl.little.pa[['mur']][['ref.pts']]['Cmax',]  <- mur_spict$report$MSY/1.4


## ---- message =FALSE----------------------------------------------------------
dls3.bd <- FLBEIA(biols = biols.bd, SRs = NULL, BDs = list(mur = murBD), fleets = fleets.bd, 
                  covars = NULL, indices = list(mur = indices), advice = advice, 
                  main.ctrl, biols.ctrl.bd, fleets.ctrl.bd, covars.ctrl = NULL, 
                  obs.ctrl.ind, assess.ctrl, advice.ctrl.dls3) 

dls3.age <- FLBEIA(biols = biols.age, SRs = list(mur = murSR), BDs = NULL, fleets = fleets.age, 
                   covars = NULL, indices = list(mur = indices), advice = advice, 
                   main.ctrl, biols.ctrl.age, fleets.ctrl.age, covars.ctrl = NULL, 
                   obs.ctrl.ind, assess.ctrl, advice.ctrl.dls3) 

#main.ctrl[[1]][2] <- 2022

little.bd <- FLBEIA(biols = biols.bd, SRs = NULL, BDs = list(mur = murBD), fleets = fleets.bd, 
                    covars = NULL, indices = list(mur = indices), advice = advice, 
                    main.ctrl, biols.ctrl.bd, fleets.ctrl.bd, covars.ctrl = NULL, 
                    obs.ctrl.ind, assess.ctrl, advice.ctrl.little) 

little.age <- FLBEIA(biols = biols.age, SRs = list(mur = murSR), BDs = NULL, fleets = fleets.age, 
                     covars = NULL, indices = list(mur = indices), advice = advice, 
                     main.ctrl, biols.ctrl.age, fleets.ctrl.age, covars.ctrl = NULL, 
                     obs.ctrl.ind, assess.ctrl, advice.ctrl.little) 

biols.bd[[1]]@range[1:3] <- NA
msy.bd <- FLBEIA(biols = biols.bd, SRs = NULL, BDs = list(mur = murBD), fleets = fleets.bd, 
                 covars = NULL, indices = NULL, advice = advice, main.ctrl, 
                 biols.ctrl.bd, fleets.ctrl.bd, covars.ctrl = NULL, 
                 obs.ctrl.stk, assess.ctrl, advice.ctrl.msy)

msy.age <- FLBEIA(biols = biols.age, BDs = NULL, SRs = list(mur = murSR), fleets = fleets.age, 
                  covars = NULL, indices = NULL, advice = advice, 
                  main.ctrl, biols.ctrl.age, fleets.ctrl.age, covars.ctrl = NULL, 
                  obs.ctrl.stk, assess.ctrl, advice.ctrl.msy)

little.pa.bd <- FLBEIA(biols = biols.bd, SRs = NULL, BDs = list(mur = murBD), fleets = fleets.bd, 
                       covars = NULL, indices = list(mur = indices), advice = advice, 
                       main.ctrl, biols.ctrl.bd, fleets.ctrl.bd, covars.ctrl = NULL, 
                       obs.ctrl.ind, assess.ctrl, advice.ctrl.little.pa) 

little.pa.age <- FLBEIA(biols = biols.age, SRs = list(mur = murSR), BDs = NULL, fleets = fleets.age, 
                        covars = NULL, indices = list(mur = indices), advice = advice, 
                        main.ctrl, biols.ctrl.age, fleets.ctrl.age, covars.ctrl = NULL, 
                        obs.ctrl.ind, assess.ctrl, advice.ctrl.little.pa) 

msy.pa.bd <- FLBEIA(biols = biols.bd, SRs = NULL, BDs = list(mur = murBD), fleets = fleets.bd, 
                    covars = NULL, indices = NULL, advice = advice, 
                    main.ctrl, biols.ctrl.bd, fleets.ctrl.bd, covars.ctrl = NULL, 
                    obs.ctrl.stk, assess.ctrl, advice.ctrl.msy.pa) 

msy.pa.age <- FLBEIA(biols = biols.age, SRs = list(mur = murSR), BDs = NULL, fleets = fleets.age, 
                     covars = NULL, indices = NULL, advice = advice, 
                     main.ctrl, biols.ctrl.age, fleets.ctrl.age, covars.ctrl = NULL, 
                     obs.ctrl.stk, assess.ctrl, advice.ctrl.msy.pa)


## -----------------------------------------------------------------------------
scenarios <- c('dls3.bd',  'little.bd',  'msy.bd',  'little.pa.bd',  'msy.pa.bd',
               'dls3.age', 'little.age', 'msy.age', 'little.pa.age', 'msy.pa.age')

Blim <- advice.ctrl.msy$mur$ref.pts['Blim',1]
Bpa  <- advice.ctrl.msy$mur$ref.pts['Btrigger',1]

bio  <- adv  <- risk  <- NULL

for(sc in scenarios){
  res_sc <- get(sc)
  bio    <- rbind(bio, bioSum(res_sc, scenario = sc, years = ac(1997:2025), long = TRUE))
  adv    <- rbind(adv, advSum(res_sc, scenario = sc, years = ac(1997:2025), long = TRUE))
  risk   <- rbind(risk, riskSum(res_sc,scenario = sc, Bpa = c(mur = Bpa), 
                                Blim = c(mur = Blim), Prflim = c(fl = 0), 
                                years = ac(1997:2025)))
}


## -----------------------------------------------------------------------------
  bioQ   <- bioSumQ(bio)
  advQ   <- advSumQ(adv)


## ----  fig.height= 10/2.54, fig.width =  16/2.54, fig.cap = 'Biomass time series obtained in each of the scenarios. The shaded area correspond with the 90% confidence interval.'----
id <- 'biomass'

p <- ggplot(subset(bioQ, indicator == id), aes(x=year, y=q50, ymin=q05, ymax=q95, group = scenario)) +
  geom_ribbon(aes(fill = scenario, alpha=0.3)) +
  geom_line(aes(color=scenario), lwd = 1) +
  ggtitle(id) +
  scale_y_continuous(name="tonnes") + 
  geom_hline(yintercept= advice.ctrl.msy$mur$ref.pts['Btrigger',1]) 
print(p)


## ----  fig.height= 10/2.54, fig.width =  16/2.54, fig.cap = 'Catch time series obtained in each of the scenarios. The shaded area correspond with the 90% confidence interval.'----
id <- 'catch'

p <- ggplot(subset(bioQ, indicator == id), aes(x=year, y=q50, ymin=q05, ymax=q95, group = scenario)) +
  geom_ribbon(aes(fill = scenario, alpha=0.3)) +
  geom_line(aes(color=scenario), lwd = 1) +
  ggtitle(id) +
  scale_y_continuous(name="tonnes") + 
  geom_hline(yintercept= advice.ctrl.msy$mur$ref.pts['Btrigger',1]) 
print(p)


## ----  fig.height= 10/2.54, fig.width =  16/2.54, fig.cap = 'TAC advice time series obtained in each of the scenarios. The shaded area correspond with the 90% confidence interval.'----
id <- 'tac'

p <- ggplot(subset(advQ, indicator == id), aes(x=year, y=q50, ymin=q05, ymax=q95, group = scenario)) +
  geom_ribbon(aes(fill = scenario, alpha=0.3)) +
  geom_line(aes(color=scenario), lwd = 1) +
  ggtitle(id) +
  scale_y_continuous(name="tonnes") + 
  geom_hline(yintercept= advice.ctrl.msy$mur$ref.pts['Btrigger',1]) 
print(p)

