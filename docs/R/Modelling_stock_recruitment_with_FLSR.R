## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("ggplot2"))
## install.packages(c("FLCore"), repos="http://flr-project.org/R")
## install.packages(c("ggplotFL"), repos="http://flr-project.org/R")

## ---- pkgs---------------------------------------------------------------
# This chunk loads all necessary packages, trims pkg messages
library(FLCore)
library(ggplotFL)

## ---- data---------------------------------------------------------------
# This chunk loads the ple4 FLStock object 
data(ple4)

## ----figA----------------------------------------------------------------
# This chunk plots the assesment output
plot(ple4)

## ----figB----------------------------------------------------------------
# This chunk plots the SSB-Recruits graph
#plot(FLQuants(ple4, "ssb", "rec"))
ggplot(aes(ssb,rec), data=model.frame(FLQuants(ple4, "ssb", "rec")))+geom_point()+geom_smooth()

## ---- FLSRobject1--------------------------------------------------------
# This chunk creates an empty FLSR object
sr1 <- FLSR()

## ---- FLSRobject2--------------------------------------------------------
# This chunk converts an FLStock object into an FLSR object
p4sr <- as.FLSR(ple4)

## ---- FLSRobject2_contents-----------------------------------------------
# This chunk outputs the summary of the FLSR object
summary(p4sr)

## ---- rec_ssb_lag--------------------------------------------------------
# This chunk outputs the contents of the rec and ssb slots of the FLSR object
ssb(p4sr)[,1]
rec(p4sr)[,1]

## ---- set_rec_age1-------------------------------------------------------
# This chunk shows how to set a different recruitment age than the default, 
# e.g. set the recruitment age at age=2
# this can be done by trimming the FLStock object as follows
p4sr2 <-as.FLSR(ple4[-1])

## ---- set_rec_age2-------------------------------------------------------
# Note the shift in years, reflecting that recruitment is now at age 2
ssb(p4sr2)[,1]
rec(p4sr2)[,1]

## ---- fit_SR_model-------------------------------------------------------
# This chunk assigns a Ricker SR model and fits it
model(p4sr) <- ricker()
model(p4sr)
# the fmle method then fits the SR model using logl and R's optim model fitting through MLE
p4sr<-fmle(p4sr)
# log-likelihood
# logl(p4sr)

## ---- SR_init_params_lmts------------------------------------------------
# initial values for the optimiser
initial(p4sr)
# lower and upper limits for the parameters
lower(p4sr)
upper(p4sr)

## ---- figC---------------------------------------------------------------
# Diagnostics plots of the fitted SR model
plot(p4sr)

## ---- nsher_ricker-------------------------------------------------------
# This chunk loads, plots and prints a summary of the nsher FLSR object 
# (a ricker SR model has already been fitted)
data(nsher)
plot(nsher)
summary(nsher)

## ---- nsher_bh_cs--------------------------------------------------------
# This chunk fits and plots a bevholt SR model and a cushing SR model on nsher data
#assign nsher with ricker model to a new object
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
# this chunk plots the fits from the 3 different SR models
plot(nsher_ri)
plot(nsher_bh)
plot(nsher_cs)

## ---- ri_bh_cs_AIC-------------------------------------------------------
# this chunk plots the fits from the 3 different SR models
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
profile(nsher_ri)
profile(nsher_bh)
profile(nsher_cs)

## ---- figE---------------------------------------------------------------
# Fit a bevholtSV model with fixed steepness at 0.8
par(mfrow=c(1,1))
model(p4sr) <- bevholtSV
p4sr <- fmle(p4sr, fixed = list(s = 0.8))
plot(p4sr)
params(p4sr)

## ---- SR_custom----------------------------------------------------------
# Fit a custom SR model (Deriso Schnute)

dersch<-function(){
  logl <- function(a,b,c,rec,ssb) {
          res<-loglAR1(log(rec), log(a*ssb*(1-b*c*ssb)^(1/c)))
          return(res)
          }

  ## initial parameter values
  initial <- structure(function(rec, ssb){
     slopeAt0 <- max(quantile(c(rec)/c(ssb), 0.9, na.rm = TRUE))
     maxRec   <- max(quantile(c(rec), 0.75, na.rm = TRUE))

     ## Bevholt by default c=-1
     return(FLPar(a=slopeAt0, b=1/maxRec, c=-1))},

  lower=rep(-Inf, 3),
	upper=rep( Inf, 3))

  model  <- rec~a*ssb*(1-b*c*ssb)^(1/c)

  return(list(logl = logl, model = model, initial = initial))}

model(nsher)<-dersch()
nsher_dersch<-fmle(nsher,fixed=list(c=-1))
plot(nsher_dersch)

## ---- SR_custom_AR1------------------------------------------------------
# Fit a custom SR AR1 model
rickerAR1 <- function()
  {
  ## log likelihood, assuming normal log.
  logl <- function(a, b, rho, rec, ssb)
      loglAR1(log(rec), log(a*ssb*exp(-b*ssb)), rho=rho)

  ## initial parameter values
  initial <- structure(function(rec, ssb) {
		# The function to provide initial values
    res  <-coefficients(lm(c(log(rec/ssb))~c(ssb)))
    return(FLPar(a=max(exp(res[1])), b=-max(res[2]), rho=0))
	},
  # lower and upper limits for optim()
	lower=rep(-Inf, 3),
	upper=rep( Inf, 3)
	)

  ## model to be fitted
	model  <- rec~a*ssb*exp(-b*ssb)

	return(list(logl=logl, model=model, initial=initial))}

#### Fit
model(nsher)<-rickerAR1()
nsherAR1 <-fmle(nsher)

plot(nsherAR1)

## ---- SR_custom_covars---------------------------------------------------
# Fit a custom SR model with covariates
nao     <-read.table(url("https://www.esrl.noaa.gov/psd/data/correlation/nao.data"),
  skip=1, nrow=62, na.strings="-99.90")
dnms    <-list(quant="nao", year=1948:2009, unit="unique", season=1:12, area="unique")
nao     <-FLQuant(unlist(nao[,-1]), dimnames=dnms, units="nao")

# include NAO as covar (note that it must be a FLQuants with a single component
# called “covar” that matches the year span of the data) and adjust the model.

nsherCovA <- nsher
nsherCovA <- transform(nsherCovA,ssb=ssb/1000,rec=rec/1000)

#### Modified so temperature affects larval survival
rickerCovA <- function(){
  logl <- function(a, b, c, rec, ssb, covar){
              loglAR1(log(rec), log(a*(1+c*covar[[1]])*ssb*exp(-b*ssb)))}

  initial <- structure(function(rec, ssb, covar) {
		# The function to provide initial values
    res  <-coefficients(lm(c(log(rec/ssb))~c(ssb)))
    return(FLPar(a=max(exp(res[1])), b=-max(res[2]), c=0.0))},

  # lower and upper limits for optim()
	lower=rep(-Inf, 3),
	upper=rep( Inf, 3))

	model  <- rec~a*(1+c*covar[[1]])*ssb*exp(-b*ssb)

	return(list(logl=logl, model=model, initial=initial))}


model(nsherCovA)<-rickerCovA()
covar(nsherCovA)<-FLQuants(covar=seasonMeans(trim(nao, year=dimnames(ssb(nsherCovA))$year)))
nsherCovA       <-fmle(nsherCovA,fixed=list(c=0))
plot(nsherCovA)

