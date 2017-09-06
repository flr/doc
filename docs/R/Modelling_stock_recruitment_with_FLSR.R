## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("ggplot2"))
## install.packages(c("FLCore"), repos="http://flr-project.org/R")
## install.packages(c("ggplotFL"), repos="http://flr-project.org/R")

## ---- pkgs---------------------------------------------------------------
library(FLCore)
library(ggplotFL)

## ---- data---------------------------------------------------------------
# Load the ple4 FLStock object 
data(ple4)

## ----figA----------------------------------------------------------------
# Plot the assesment output
plot(ple4)

## ----figB----------------------------------------------------------------
# Plot the SSB-Recruits graph
ggplot(aes(ssb,rec), data=model.frame(FLQuants(ple4, "ssb", "rec")))+geom_point()+geom_smooth()

## ---- FLSRobject1--------------------------------------------------------
sr1 <- FLSR()

## ---- FLSRobject2--------------------------------------------------------
p4sr <- as.FLSR(ple4)

## ---- FLSRobject2_contents-----------------------------------------------
summary(p4sr)

## ---- rec_ssb_lag--------------------------------------------------------
# Outputs the contents of the first year of the rec and ssb slots of the FLSR object
ssb(p4sr)[,1]
rec(p4sr)[,1]

## ---- set_rec_age1-------------------------------------------------------
# You can set a different recruitment age, e.g. age 2, 
# by trimming the FLStock object as follows:
p4sr2 <-as.FLSR(ple4[-1])

## ---- set_rec_age2-------------------------------------------------------
ssb(p4sr2)[,1]
rec(p4sr2)[,1]

## ---- fit_SR_model, results="hide"---------------------------------------
# Assign a Ricker SR model and fit it with fmle (which uses logl and R's optim model fitting through MLE)
model(p4sr) <- ricker()
p4sr<-fmle(p4sr)
## model formula
# model(p4sr)
## log-likelihood
# logl(p4sr)

## ---- SR_init_params_lmts------------------------------------------------
# initial values for the optimiser
initial(p4sr)
# lower and upper limits for the parameters
lower(p4sr)
upper(p4sr)

## ---- figC---------------------------------------------------------------
plot(p4sr)

## ---- nsher_ricker-------------------------------------------------------
data(nsher)
summary(nsher)

## ---- nsher_bh_cs, results="hide"----------------------------------------
# Assign nsher with ricker model to a new object
nsher_ri <- nsher
# change model to bevholt
model(nsher) <- bevholt()
# fit through MLE
nsher_bh <- fmle(nsher)
# change model to cushing
model(nsher) <- cushing()
# fit through MLE
nsher_cs <- fmle(nsher)

## ---- ri_bh_cs_plots-----------------------------------------------------
plot(nsher_ri)
plot(nsher_bh)
plot(nsher_cs)

## ---- ri_bh_cs_AIC-------------------------------------------------------
print(paste0('Ricker: ',round(AIC(nsher_ri),4),' ',
             'Beverton-Holt: ',round(AIC(nsher_bh),4),' ',
             'Cushing: ',round(AIC(nsher_cs),4)))

## ---- ri_bh_cs_BIC-------------------------------------------------------
# this chunk plots the fits from the 3 different SR models
print(paste0('Ricker: ',round(BIC(nsher_ri),4),' ',
             'Beverton-Holt: ',round(BIC(nsher_bh),4),' ',
             'Cushing: ',round(BIC(nsher_cs),4)))

## ---- figD---------------------------------------------------------------
# Profile the likelihood to check the fit
par(mfrow=c(1,3))
profile(nsher_ri, main="Ricker")
profile(nsher_bh, main="Beverton-Holt")
profile(nsher_cs, main="Cushing")

## ---- figE1, results="hide"----------------------------------------------
# Fit a bevholtSV model with fixed steepness at 0.8
model(p4sr) <- bevholtSV
p4sr <- fmle(p4sr, fixed = list(s = 0.8))

## ---- figE2--------------------------------------------------------------
# Plot the SR model and show parameters
par(mfrow=c(1,1))
plot(p4sr)
params(p4sr)

## ---- SR_custom1---------------------------------------------------------
# Define a custom SR model (Deriso Schnute)
dersch<-function(){
  ## log-likelihood
  logl <- function(a,b,c,rec,ssb) {
          res<-loglAR1(log(rec), log(a*ssb*(1-b*c*ssb)^(1/c)))
          return(res)
          }
  ## initial parameter values
  initial <- structure(function(rec, ssb){
      slopeAt0 <- max(quantile(c(rec)/c(ssb), 0.9, na.rm = TRUE))
      maxRec   <- max(quantile(c(rec), 0.75, na.rm = TRUE))
      ### Bevholt by default c=-1
      return(FLPar(a=slopeAt0, b=1/maxRec, c=-1))},
    lower=rep(-Inf, 3),
	  upper=rep( Inf, 3))
  ## model to be fitted
  model  <- rec~a*ssb*(1-b*c*ssb)^(1/c)
  return(list(logl = logl, model = model, initial = initial))}

## ---- SR_custom2, results="hide"-----------------------------------------
# Fit the custom SR model
model(nsher)<-dersch()
nsher_dersch<-fmle(nsher,fixed=list(c=-1))

## ---- SR_custom3---------------------------------------------------------
# Plot the custom SR model
plot(nsher_dersch)

## ---- SR_custom_AR1------------------------------------------------------
# Define a custom SR AR1 model
rickerAR1 <- function()
  {
  ## log-likelihood
  logl <- function(a, b, rho, rec, ssb)
          loglAR1(log(rec), log(a*ssb*exp(-b*ssb)), rho=rho)
  ## initial parameter values
  initial <- structure(function(rec, ssb) {
      res  <-coefficients(lm(c(log(rec/ssb))~c(ssb)))
      return(FLPar(a=max(exp(res[1])), b=-max(res[2]), rho=0))},
	  lower=rep(-Inf, 3),
	  upper=rep( Inf, 3))
  ## model to be fitted
	model  <- rec~a*ssb*exp(-b*ssb)
	return(list(logl=logl, model=model, initial=initial))}

## ---- SR_custom_AR2, results="hide"--------------------------------------
# Fit the custom SR AR1 model
model(nsher)<-rickerAR1()
nsherAR1 <-fmle(nsher)

## ---- SR_custom_AR3------------------------------------------------------
# Plot the custom SR AR1 model
plot(nsherAR1)

## ---- SR_custom_covars1--------------------------------------------------
# Read in the data to represent the covariate
nao  <-read.table(url("https://www.esrl.noaa.gov/psd/data/correlation/nao.data"), 
       skip=1, nrow=62, na.strings="-99.90")
dnms <-list(quant="nao", year=1948:2009, unit="unique", season=1:12, area="unique")
nao  <-FLQuant(unlist(nao[,-1]), dimnames=dnms, units="nao")

# Include NAO as the covariate (covar) and adjust the model.
# (Note that covar must be an FLQuants with a single component called `covar`
# that matches the year span of the data.)
nsherCovA <- nsher
nsherCovA <- transform(nsherCovA,ssb=ssb/1000,rec=rec/1000)

# Define the custom SR model with covariate
# (modified so temperature affects larval survival)
rickerCovA <- function(){
  ## log likelihood
  logl <- function(a, b, c, rec, ssb, covar){
          loglAR1(log(rec), log(a*(1+c*covar[[1]])*ssb*exp(-b*ssb)))}
  ## initial parameter values
  initial <- structure(function(rec, ssb, covar) {
      res  <-coefficients(lm(c(log(rec/ssb))~c(ssb)))
      return(FLPar(a=max(exp(res[1])), b=-max(res[2]), c=0.0))},
	  lower=rep(-Inf, 3),
	  upper=rep( Inf, 3))
  ## model to be fitted
	model  <- rec~a*(1+c*covar[[1]])*ssb*exp(-b*ssb)
	return(list(logl=logl, model=model, initial=initial))}

## ---- SR_custom_covars2, results="hide"----------------------------------
# Fit the custom SR model with covariate
model(nsherCovA)<-rickerCovA()
covar(nsherCovA)<-FLQuants(covar=seasonMeans(trim(nao, year=dimnames(ssb(nsherCovA))$year)))
nsherCovA       <-fmle(nsherCovA,fixed=list(c=0))

## ---- SR_custom_covars3--------------------------------------------------
# Plot the custom SR model with covariate
plot(nsherCovA)

