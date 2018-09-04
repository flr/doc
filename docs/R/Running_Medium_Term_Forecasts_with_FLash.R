## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("FLCore"), repos="http://flr-project.org/R")
## install.packages(c("FLash"), repos="http://flr-project.org/R")
## install.packages(c("FLBRP"), repos="http://flr-project.org/R")
## install.packages(c("FLAssess"), repos="http://flr-project.org/R")

## ---- pkgs---------------------------------------------------------------
# This chunk loads all necessary packages, trims pkg messages
library(FLCore)
library(FLash)
library(FLBRP)
library(FLAssess)

## ---- ple4---------------------------------------------------------------
data(ple4)

## ---- stf_condition------------------------------------------------------
# Set up a 10 year MTF
ple4_mtf <- stf(ple4, nyears = 10)

## ---- summarymtf---------------------------------------------------------
summary(ple4_mtf)

## ---- fitSRR-------------------------------------------------------------
ple4_sr <- fmle(as.FLSR(ple4, model="bevholt"), control=list(trace=0))

## ---- plotSRR, fig.cap="Fitted Beverton-Holt stock-recruitment relationship for the *ple4* stock object"----
plot(ple4_sr)

## ---- feg1---------------------------------------------------------------
f_status_quo <- mean(fbar(ple4)[,as.character(2005:2008)])
f_status_quo

## ---- feg2---------------------------------------------------------------
ctrl_target <- data.frame(year = 2009:2018,
			  quantity = "f",
			  val = f_status_quo)

## ---- feg3---------------------------------------------------------------
ctrl_f <- fwdControl(ctrl_target)

## ---- reg31--------------------------------------------------------------
ctrl_f

## ---- feg4---------------------------------------------------------------
ple4_f_sq <- fwd(ple4_mtf, ctrl = ctrl_f, sr = ple4_sr)

## ---- fegplot------------------------------------------------------------
plot(window(ple4_f_sq, start=2000))

## ---- feg5---------------------------------------------------------------
fbar(ple4_f_sq)[,ac(2005:2018)]

## ---- feg6---------------------------------------------------------------
rec(ple4_f_sq)[,ac(2005:2018)]

## ---- ceg1---------------------------------------------------------------
future_catch <- c(catch(ple4)[,"2008"]) * 0.9^(1:10)
future_catch

## ---- ceg2---------------------------------------------------------------
ctrl_catch <- fwdControl(
	data.frame(
		year=2009:2018,
		quantity = "catch",
		val=future_catch))

## ---- ceg3---------------------------------------------------------------
ctrl_catch

## ---- ceg4---------------------------------------------------------------
ple4_catch <- fwd(ple4_mtf, ctrl_catch, sr = ple4_sr)

## ---- cegplot------------------------------------------------------------
plot(window(ple4_catch, start=2000))

## ---- seg2---------------------------------------------------------------
future_ssb <- 150000
ctrl_ssb <- fwdControl(data.frame(year=2008, quantity = "ssb", val=future_ssb))
ctrl_ssb
ple4_ssb <- fwd(ple4_mtf, ctrl_ssb, sr = ple4_sr)

## ---- seg3---------------------------------------------------------------
ssb(ple4_ssb)[,ac(2005:2009)]

## ---- seg4---------------------------------------------------------------
future_ssb <- 300000
ctrl_ssb <- fwdControl(data.frame(year=2008:2017, quantity = "ssb", val=future_ssb))
ple4_ssb <- fwd(ple4_mtf, ctrl_ssb, sr = ple4_sr)

## ---- seg5---------------------------------------------------------------
ssb(ple4_ssb)[,ac(2005:2018)]

## ---- seg6---------------------------------------------------------------
plot(window(ple4_ssb, start=2000, end=2017))

## ---- rceg1--------------------------------------------------------------
ctrl_rel_catch <- fwdControl(
	data.frame(year = 2009:2018,
		   quantity = "catch",
		   val = 0.9,
		   rel.year = 2008:2017))

## ---- rceg2--------------------------------------------------------------
ctrl_rel_catch

## ---- rceg3--------------------------------------------------------------
ple4_rel_catch <- fwd(ple4_mtf, ctrl_rel_catch, sr = ple4_sr)

## ---- rceg4--------------------------------------------------------------
catch(ple4_rel_catch)
catch(ple4_rel_catch)[,ac(2009:2018)] / catch(ple4_rel_catch)[,ac(2008:2017)]
plot(window(ple4_rel_catch, start = 2001, end = 2018))

## ---- meg1---------------------------------------------------------------
f01 <- c(refpts(brp(FLBRP(ple4)))["f0.1","harvest"])
f01

## ---- meg2---------------------------------------------------------------
min_catch <- mean(catch(ple4_mtf)[,as.character(2006:2008)])
min_catch

## ---- meg3---------------------------------------------------------------
ctrl_target <- rbind(
    f_df <- data.frame(
        year = 2009:2018,
        quantity = "f",
        val = f01,
        min = NA),
    catch_df <- data.frame(
        year = 2009:2018,
        quantity = "catch",
        val = NA,
        min = min_catch)
)

## ---- meg4---------------------------------------------------------------
ctrl_target <- ctrl_target[order(ctrl_target$year),]
ctrl_target

## ---- meg5---------------------------------------------------------------
ctrl_min_catch <- fwdControl(ctrl_target)

## ---- meg6---------------------------------------------------------------
ctrl_min_catch

## ---- meg7---------------------------------------------------------------
ple4_min_catch <- fwd(ple4_mtf, ctrl_min_catch, sr = ple4_sr)

## ---- meg8---------------------------------------------------------------
plot(window(ple4_min_catch, start = 2001, end = 2018))

## ---- rtbeg1-------------------------------------------------------------
current_fbar <- c(fbar(ple4)[,"2008"])
f_target <- c(seq(from = current_fbar, to = f01, length = 8)[-1], rep(f01, 3))
f_target

## ---- rtbeg2-------------------------------------------------------------
rel_catch_bound <- 0.10

## ---- rtbeg3-------------------------------------------------------------
ctrl_target <- rbind(
    f_df <- data.frame(
        year = 2009:2018,
        rel.year = NA,
        quantity = "f",
        val = f_target,
        max = NA,
        min = NA),
    catch_df <- data.frame(
        year = 2009:2018,
        rel.year = 2008:2017,
        quantity = "catch",
        val = NA,
        max = 1 + rel_catch_bound,
        min = 1 - rel_catch_bound)
)

## ---- rtbeg4-------------------------------------------------------------
ctrl_target <- ctrl_target[order(ctrl_target$year),]
ctrl_target

## ---- rtbeg5-------------------------------------------------------------
ctrl_rel_min_max_catch <- fwdControl(ctrl_target)
ctrl_rel_min_max_catch

## ---- rtbeg6-------------------------------------------------------------
recovery<-fwd(ple4_mtf, ctrl=ctrl_rel_min_max_catch, sr=ple4_sr)

## ---- rtbeg7-------------------------------------------------------------
plot(window(recovery, start = 2001, end = 2018))

## ---- rtbeg8-------------------------------------------------------------
catch(recovery)[,ac(2009:2018)] / catch(recovery)[,ac(2008:2017)]

## ---- niter--------------------------------------------------------------
niters <- 1000
ple4_mtf <- stf(ple4, nyears = 10)
ple4_mtf <- propagate(ple4_mtf, niters)

## ---- prop---------------------------------------------------------------
summary(ple4_mtf)

## ---- res----------------------------------------------------------------
multi_rec_residuals <- FLQuant(NA, dimnames = list(year=2009:2018, iter=1:niters))

## ---- res2---------------------------------------------------------------
residuals(ple4_sr)

## ---- res3---------------------------------------------------------------
sample_years <- sample(dimnames(residuals(ple4_sr))$year, niters * 10, replace = TRUE)

## ---- res4---------------------------------------------------------------
multi_rec_residuals[] <- exp(residuals(ple4_sr)[,sample_years])

## ---- res5---------------------------------------------------------------
multi_rec_residuals

## ---- res6---------------------------------------------------------------
ple4_stoch_rec <- fwd(ple4_mtf, ctrl = ctrl_catch, sr = ple4_sr, sr.residuals = multi_rec_residuals, sr.residuals.mult = TRUE)

## ---- res7---------------------------------------------------------------
plot(window(ple4_stoch_rec, start = 2001, end = 2018))

## ---- res8---------------------------------------------------------------
rec(ple4_stoch_rec)[,ac(2008:2018)]
fbar(ple4_stoch_rec)[,ac(2008:2018)]
ssb(ple4_stoch_rec)[,ac(2008:2018)]

## ---- stv1---------------------------------------------------------------
ctrl_catch
ctrl_catch_iters <- ctrl_catch

## ---- stv2---------------------------------------------------------------
slotNames(ctrl_catch_iters)

## ---- stv3---------------------------------------------------------------
ctrl_catch_iters@trgtArray

## ---- stv4---------------------------------------------------------------
class(ctrl_catch_iters@trgtArray)
dim(ctrl_catch_iters@trgtArray)

## ---- stv5---------------------------------------------------------------
new_trgtArray <- array(NA, dim=c(10,3,niters), dimnames = list(1:10, c("min","val","max"),iter=1:niters))
dim(new_trgtArray)

## ---- stv6---------------------------------------------------------------
future_catch_iters <- ctrl_catch_iters@trgtArray[,"val",] * rlnorm(10 * niters, meanlog = 0, sdlog=0.3)

## ---- stv7---------------------------------------------------------------
new_trgtArray[,"val",] <- future_catch_iters

## ---- stv8---------------------------------------------------------------
ctrl_catch_iters@trgtArray <- new_trgtArray

## ---- stv9---------------------------------------------------------------
ctrl_catch_iters

## ---- stv10--------------------------------------------------------------
ple4_catch_iters <- fwd(ple4_mtf, ctrl_catch_iters, sr = ple4_sr)

## ---- stv11--------------------------------------------------------------
plot(window(ple4_catch_iters, start = 2001, end = 2018))

## ---- stv12--------------------------------------------------------------
catch(ple4_catch_iters)[,ac(2008:2018)]

## ---- stv13--------------------------------------------------------------
rec(ple4_catch_iters)[,ac(2008:2018)]

## ---- stv14--------------------------------------------------------------
ple4_catch_iters <- fwd(ple4_mtf, ctrl_catch_iters, sr = ple4_sr, sr.residuals = multi_rec_residuals, sr.residuals.mult = TRUE)

## ---- stv15--------------------------------------------------------------
plot(window(ple4_catch_iters, start = 2001, end = 2018))

## ---- stv16--------------------------------------------------------------
catch(ple4_catch_iters)[,ac(2008:2018)]
rec(ple4_catch_iters)[,ac(2008:2018)]

