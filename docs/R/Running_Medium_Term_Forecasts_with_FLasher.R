## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("FLCore"), repos="http://flr-project.org/R")
## install.packages(c("FLasher"), repos="http://flr-project.org/R")
## install.packages(c("FLFishery"), repos="http://flr-project.org/R")

## ---- pkgs---------------------------------------------------------------
# Load all necessary packages, trim pkg messages
library(FLCore)
library(FLasher)

## ---- ple4---------------------------------------------------------------
data(ple4)

## ---- stf_condition------------------------------------------------------
ple4_mtf <- stf(ple4, nyears = 10)
# Now the stock goes up to 2018
summary(ple4_mtf)

## ---- fitSRR-------------------------------------------------------------
ple4_sr <- fmle(as.FLSR(ple4, model="bevholt"), control=list(trace=0))

## ---- plotSRR, fig.cap="Fitted Beverton-Holt stock-recruitment relationship for the *ple4* stock object"----
plot(ple4_sr)

## ---- ex1a---------------------------------------------------------------
f_status_quo <- mean(fbar(ple4)[,as.character(2005:2008)])
f_status_quo

## ---- ex1b---------------------------------------------------------------
ctrl_target <- data.frame(year = 2009:2018,
			                    quant = "f",
			                    value = f_status_quo)

## ---- ex1c---------------------------------------------------------------
ctrl_f <- fwdControl(ctrl_target)
ctrl_f

## ------------------------------------------------------------------------
ctrl_f <- fwdControl(list(year=2009:2018, quant="f", value=f_status_quo))
ctrl_f

## ---- ex1d---------------------------------------------------------------
ple4_f_sq <- fwd(ple4_mtf, control = ctrl_f, sr = ple4_sr)
# What just happened? We plot the stock from the year 2000.
plot(window(ple4_f_sq, start=2000))

## ---- ex1e---------------------------------------------------------------
fbar(ple4_f_sq)[,ac(2005:2018)]

## ---- ex1f---------------------------------------------------------------
rec(ple4_f_sq)[,ac(2005:2018)]

## ---- ex2a---------------------------------------------------------------
future_catch <- c(catch(ple4)[,"2008"]) * 0.9^(1:10)
future_catch

## ---- ex2b---------------------------------------------------------------
ctrl_catch <- fwdControl(list(year=2009:2018, quant = "catch", value=future_catch))
# The control object has the desired catch target values
ctrl_catch

## ---- ex2c---------------------------------------------------------------
ple4_catch <- fwd(ple4_mtf, control = ctrl_catch, sr = ple4_sr)
catch(ple4_catch)[,ac(2008:2018)]
plot(window(ple4_catch, start=2000))

## ---- ex3a---------------------------------------------------------------
final_ssb <- 100000
ctrl_ssb <- fwdControl(list(year=2009, quant = "ssb_end", value=final_ssb))
ple4_ssb <- fwd(ple4_mtf, control=ctrl_ssb, sr = ple4_sr)
# Calculate the final SSB to check the target has been hit
survivors <- stock.n(ple4_ssb) * exp(-harvest(ple4_ssb) - m(ple4_ssb))
quantSums((survivors * stock.wt(ple4_ssb) * mat(ple4_ssb))[,ac(2009)])

## ---- ex3b, warning=TRUE-------------------------------------------------
spawn_ssb <- 100000
ctrl_ssb <- fwdControl(list(year=2009, quant = "ssb_spawn", value=spawn_ssb))
ple4_ssb <- fwd(ple4_mtf, control=ctrl_ssb, sr = ple4_sr)
# Using the `ssb()` method to get the SSB at the time of spawning, we can see that the projection failed
ssb(ple4_ssb)[,ac(2009)]

## ---- ex3c---------------------------------------------------------------
m.spwn(ple4_mtf)[,ac(2009)] <- 0.5
harvest.spwn(ple4_mtf)[,ac(2009)] <- 0.5
spawn_ssb <- 100000
ctrl_ssb <- fwdControl(data.frame(year=2009, quant = "ssb_spawn", value=spawn_ssb))
ple4_ssb <- fwd(ple4_mtf, control=ctrl_ssb, sr = ple4_sr)
# We hit the target
ssb(ple4_ssb)[,ac(2009)]

## ---- ex3d---------------------------------------------------------------
srp <- 100000
ctrl_ssb <- fwdControl(data.frame(year=2009, quant = "srp", value=srp))
ple4_ssb <- fwd(ple4_mtf, control=ctrl_ssb, sr = ple4_sr)
# We hit the target
ssb(ple4_ssb)[,ac(2009)]

## ---- ex3e---------------------------------------------------------------
# Force spawning to happen half way through the year
# and fishing to start at the beginning of the year
m.spwn(ple4_mtf)[,ac(2009)] <- 0.5
harvest.spwn(ple4_mtf)[,ac(2009)] <- 0.5
flash_ssb <- 150000
ctrl_ssb <- fwdControl(data.frame(year=2009, quant = "ssb_flash", value=flash_ssb))
ple4_ssb <- fwd(ple4_mtf, control=ctrl_ssb, sr = ple4_sr)
# Hit the target? Yes
ssb(ple4_ssb)[,ac(2009)]

## ---- ex3f---------------------------------------------------------------
# Force spawning to happen at the start of the year before fishing
m.spwn(ple4_mtf)[,ac(2009)] <- 0.0
harvest.spwn(ple4_mtf)[,ac(2009)] <- 0.0
flash_ssb <- 150000
ctrl_ssb <- fwdControl(data.frame(year=2009, quant = "ssb_flash", value=flash_ssb))
ple4_ssb <- fwd(ple4_mtf, control=ctrl_ssb, sr = ple4_sr)
# We did hit the SSB target, but not until 2010.
ssb(ple4_ssb)[,ac(2009:2010)]

## ---- ex3g---------------------------------------------------------------
# Force spawning to happen at the start of the year before fishing
m.spwn(ple4_mtf)[,ac(2009)] <- 0.0
harvest.spwn(ple4_mtf)[,ac(2009)] <- 0.0
future_ssb <- 200000
ctrl_ssb <- fwdControl(data.frame(year=2009:2018, quant = "ssb_flash", value=future_ssb))
ple4_ssb <- fwd(ple4_mtf, control = ctrl_ssb, sr = ple4_sr)

## ---- ex3h---------------------------------------------------------------
ssb(ple4_ssb)[,ac(2009:2018)]
fbar(ple4_ssb)[,ac(2009:2018)]
plot(window(ple4_ssb, start=2000, end=2017))

## ---- ex4a---------------------------------------------------------------
ctrl_rel_catch <- fwdControl(
	data.frame(year = 2009:2018,
		   quant = "catch",
		   value = 0.9,
		   relYear = 2008:2017))
# The relative year appears in the control object summary
ctrl_rel_catch

## ---- ex4b---------------------------------------------------------------
ple4_rel_catch <- fwd(ple4_mtf, control = ctrl_rel_catch, sr = ple4_sr)
catch(ple4_rel_catch)
catch(ple4_rel_catch)[,ac(2008:2018)] / catch(ple4_rel_catch)[,ac(2007:2017)]

## ---- ex4c, fig.cap="Relative catch example"-----------------------------
plot(window(ple4_rel_catch, start = 2001, end = 2018))

## ---- ex5a---------------------------------------------------------------
f01 <- 0.1

## ---- ex5b---------------------------------------------------------------
min_catch <- mean(catch(ple4_mtf)[,as.character(2006:2008)])
min_catch

## ---- ex5c---------------------------------------------------------------
ctrl_min_catch <- fwdControl(
    list(year=2009:2018, quant="f", value=f01),
    list(year=2009:2018, quant="catch", min=min_catch))
ctrl_min_catch

## ---- ex5e---------------------------------------------------------------
ple4_min_catch <- fwd(ple4_mtf, control = ctrl_min_catch, sr = ple4_sr)
fbar(ple4_min_catch)[,ac(2008:2018)]
catch(ple4_min_catch)[,ac(2008:2018)]

## ---- ex5f, fig.cap="Example with a minimum catch bound and constant F target"----
plot(window(ple4_min_catch, start = 2001, end = 2018))

## ---- ex6a---------------------------------------------------------------
current_fbar <- c(fbar(ple4)[,"2008"])
f_target <- c(seq(from = current_fbar, to = f01, length = 8)[-1], rep(f01, 3))
f_target

## ---- ex6b---------------------------------------------------------------
rel_catch_bound <- 0.10

## ---- ex6c---------------------------------------------------------------
ctrl_rel_min_max_catch <- fwdControl(
    list(year=2009:2018, quant="f", value=f_target),
    list(year=2009:2018, quant="catch", relYear=2008:2017, max=1+rel_catch_bound, min=1-rel_catch_bound))
ctrl_rel_min_max_catch

## ---- ex6e---------------------------------------------------------------
recovery<-fwd(ple4_mtf, control=ctrl_rel_min_max_catch, sr=ple4_sr)

## ---- ex6f---------------------------------------------------------------
plot(window(recovery, start = 2001, end = 2018))

## ---- ex6g---------------------------------------------------------------
catch(recovery)[,ac(2009:2018)] / catch(recovery)[,ac(2008:2017)]

## ---- niter--------------------------------------------------------------
niters <- 200
ple4_mtf <- stf(ple4, nyears = 10)
ple4_mtf <- propagate(ple4_mtf, niters)

## ---- prop---------------------------------------------------------------
summary(ple4_mtf)

## ---- res----------------------------------------------------------------
rec_residuals <- FLQuant(NA, dimnames = list(year=2009:2018, iter=1:niters))

## ---- res2---------------------------------------------------------------
residuals(ple4_sr)

## ---- res3---------------------------------------------------------------
sample_years <- sample(dimnames(residuals(ple4_sr))$year, niters * 10, replace = TRUE)

## ---- res4---------------------------------------------------------------
rec_residuals[] <- exp(residuals(ple4_sr)[,sample_years])

## ---- res5---------------------------------------------------------------
rec_residuals

## ---- res6---------------------------------------------------------------
ple4_stoch_rec <- fwd(ple4_mtf, control = ctrl_catch, sr = ple4_sr, residuals = rec_residuals) 

## ---- res7, fig.cap="Example projection with stochasticity in the recruitment residuals"----
plot(window(ple4_stoch_rec, start = 2001, end = 2018))

## ---- res8---------------------------------------------------------------
rec(ple4_stoch_rec)[,ac(2008:2018)]
fbar(ple4_stoch_rec)[,ac(2008:2018)]
ssb(ple4_stoch_rec)[,ac(2008:2018)]

## ---- stv1---------------------------------------------------------------
ctrl_catch

## ---- stv2---------------------------------------------------------------
slotNames(ctrl_catch)

## ---- stv3---------------------------------------------------------------
ctrl_catch@iters

## ---- stv4---------------------------------------------------------------
class(ctrl_catch@iters)
dim(ctrl_catch@iters)

## ------------------------------------------------------------------------
stoch_catch  <- rlnorm(10*niters, meanlog=log(future_catch), sdlog=0.3)
length(stoch_catch)
ctrl_catch_iters <- fwdControl(list(year=2009:2018, quant="catch", value=stoch_catch))

## ------------------------------------------------------------------------
ctrl_catch_iters

## ---- stv10--------------------------------------------------------------
ple4_catch_iters <- fwd(ple4_mtf, control=ctrl_catch_iters, sr = ple4_sr)

## ---- stv11--------------------------------------------------------------
plot(window(ple4_catch_iters, start = 2001, end = 2018))

## ---- stv12--------------------------------------------------------------
catch(ple4_catch_iters)[,ac(2008:2018)]

## ------------------------------------------------------------------------
params(ple4_sr)

## ------------------------------------------------------------------------
sr_iters <- FLPar(NA, dimnames=list(params=c("a","b"), iter=1:niters))

## ------------------------------------------------------------------------
aiters <- rlnorm(niters, meanlog=log(params(ple4_sr)["a"]), sdlog=0.5)
biters <- rlnorm(niters, meanlog=log(params(ple4_sr)["b"]), sdlog=0.01)

## ------------------------------------------------------------------------
sr_iters["a"] <- aiters
sr_iters["b"] <- biters
sr_iters

## ------------------------------------------------------------------------
ple4_sr_iters <- fwd(ple4_mtf, control=ctrl_catch, sr = list(model="bevholt", params=sr_iters))
plot(window(ple4_sr_iters, start = 2001, end = 2018))

## ---- stv14--------------------------------------------------------------
ple4_iters <- fwd(ple4_mtf, control=ctrl_catch_iters, sr = list(model="bevholt", params=sr_iters), residuals = rec_residuals)

## ---- stv15--------------------------------------------------------------
plot(window(ple4_iters, start = 2001, end = 2018))

## ---- stv16--------------------------------------------------------------
catch(ple4_catch_iters)[,ac(2008:2018)]
rec(ple4_catch_iters)[,ac(2008:2018)]

