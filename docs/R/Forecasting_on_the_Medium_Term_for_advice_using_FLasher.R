## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("FLCore"), repos="http://flr-project.org/R")
## install.packages(c("FLasher"), repos="http://flr-project.org/R")
## install.packages(c("FLAssess"), repos="http://flr-project.org/R")

## ---- pkgs---------------------------------------------------------------
# This chunk loads all necessary packages, trims pkg messages
library(FLCore)
library(FLasher)

## ---- stf, echo=FALSE----------------------------------------------------
setGeneric("stf", function(object,...)
	standardGeneric("stf"))

## stf(FLStock) {{{
setMethod('stf', signature(object='FLStock'),
  function(object, nyears=3, wts.nyears=3, fbar.nyears=wts.nyears, f.rescale=FALSE,
    arith.mean=TRUE, na.rm=TRUE, end=dims(object)$maxyear + nyears, disc.nyears=wts.nyears)
  {
    dims <- dims(object)

    # check nyears and end match
    if(missing(nyears))
      nyears <- as.numeric(end) - dims$maxyear
    else if(dims$maxyear + nyears != end)
      stop("'nyears' and 'end' do not match: ", dims$maxyear + nyears, " vs. ", end)

    # years
    years      <- ac((dims$maxyear+1):end)
    wts.years  <- ac(seq(dims$maxyear-wts.nyears+1, dims$maxyear))
    disc.years  <- ac(seq(dims$maxyear-disc.nyears+1, dims$maxyear))
    fbar.years <- ac(seq(dims$maxyear-fbar.nyears+1, dims$maxyear))
    fbar.ages  <- ac(range(object, 'minfbar'):range(object, 'maxfbar'))

    # arith or geometric
    if(arith.mean)
      fmean <- mean
    else  
      fmean <- function(x) exp(mean(log(x)))

    # window object
    res <- window(object, end=end)

    # average slots
    # *.wt, mat, m and *.spwn as average over wts.years
    for (i in c('catch.wt', 'landings.wt', 'discards.wt', 'stock.wt', 'mat', 'm', 'harvest.spwn', 'm.spwn')){
      flq<- apply(slot(res, i)[,wts.years], c(1,3:6),fmean, na.rm=na.rm)
      for (j in years)
         slot(res, i)[,j] <-flq
      }

    # landings.n and discards.n as proportions of disc.years
    for (i in years)
       slot(res, 'discards.n')[,i] <- apply(slot(res, 'discards.n')[, disc.years]/slot(res, 'catch.n')[, disc.years], c(1,3:6), mean)
    slot(res, 'landings.n')[,years] <- 1 - slot(res, 'discards.n')[,years]

    # harvest as mean over fbar.nyears
    f <-apply(slot(res, 'harvest')[,fbar.years], c(1,3:6), fmean, na.rm=na.rm)
    for (i in years)
       slot(res, 'harvest')[,i] <- f

    # f.rescale
    if(f.rescale == TRUE)
    {
      # mean f over fbar ages and years
      fbar <- mean(apply(slot(res, 'harvest')[fbar.ages, fbar.years], c(2:6), mean,
        na.rm=na.rm))
      # fbar for last REAL year
      lastfbar <- apply(slot(res, 'harvest')[fbar.ages, ac(dims$maxyear)], 3:6, mean,
        na.rm=na.rm)

      # divide by fbar and multiply by lastfbar
      slot(res, 'harvest')[, years] <- sweep(slot(res, 'harvest')[, years], 3:6, fbar, '/')
      slot(res, 'harvest')[, years] <- sweep(slot(res, 'harvest')[, years], 3:6, lastfbar, '*')
    }
    return(res)
  }
) # }}}

## stf(FLBiol) {{{
setMethod('stf', signature(object='FLBiol'),
  function(object, nyears=3, wts.nyears=3, arith.mean=TRUE, na.rm=TRUE,
    end=dims(object)$maxyear + nyears)
  {
    dims <- dims(object)
    
    # check nyears and end match
    if(missing(nyears))
      nyears <- as.numeric(end) - dims$maxyear
    else if(dims$maxyear + nyears != end)
      stop("'nyears' and 'end' do not match: ", dims$maxyear + nyears, " vs. ", end)

    # years
    years <- ac((dims$maxyear+1):end)
    wts.years <- ac(seq(dims$maxyear-wts.nyears+1, dims$maxyear))

    # arith or geometric
    if(arith.mean)
      fmean <- mean
    else  
      fmean <- function(x) exp(mean(log(x)))

    # window object
    res <- window(object, end=end)

    # average slots
    # *.wt, mat, m and *.spwn as average over wts.years
    for (i in c('wt', 'fec', 'm', 'spwn'))
      slot(res, i)[,years] <- apply(slot(res, i)[,wts.years], c(1,3:6), fmean, na.rm=TRUE)
    
    return(res)
  }
) # }}}

## ---- ple4---------------------------------------------------------------
data(ple4)

## ---- stf_condition------------------------------------------------------
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
			  quant = "f",
			  value = f_status_quo)

## ---- feg3---------------------------------------------------------------
ctrl_f <- fwdControl(ctrl_target)

## ---- reg31--------------------------------------------------------------
ctrl_f

## ---- feg4---------------------------------------------------------------
ple4_f_sq <- fwd(ple4_mtf, control = ctrl_f, sr = ple4_sr)

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
		quant = "catch",
		value=future_catch))

## ---- ceg3---------------------------------------------------------------
ctrl_catch

## ---- ceg4---------------------------------------------------------------
ple4_catch <- fwd(ple4_mtf, control = ctrl_catch, sr = ple4_sr)

## ---- cegplot------------------------------------------------------------
plot(window(ple4_catch, start=2000))

## ---- seg2---------------------------------------------------------------
future_ssb <- 250000
ctrl_ssb <- fwdControl(data.frame(year=2008, quant = "ssb", value=future_ssb))
ctrl_ssb
ple4_ssb <- fwd(ple4_mtf, control=ctrl_ssb, sr = ple4_sr)

## ---- seg3---------------------------------------------------------------
ssb(ple4_ssb)[,ac(2005:2009)]

## ---- seg4---------------------------------------------------------------
future_ssb <- 250000
ctrl_ssb <- fwdControl(data.frame(year=2008:2017, quant = "ssb", value=future_ssb))
ple4_ssb <- fwd(ple4_mtf, control = ctrl_ssb, sr = ple4_sr)

## ---- seg5---------------------------------------------------------------
ssb(ple4_ssb)[,ac(2005:2018)]

## ---- seg6---------------------------------------------------------------
plot(window(ple4_ssb, start=2000, end=2017))

## ---- rceg1--------------------------------------------------------------
ctrl_rel_catch <- fwdControl(
	data.frame(year = 2009:2018,
		   quant = "catch",
		   value = 0.9,
		   relYear = 2008:2017))

## ---- rceg2--------------------------------------------------------------
ctrl_rel_catch

## ---- rceg3--------------------------------------------------------------
ple4_rel_catch <- fwd(ple4_mtf, control = ctrl_rel_catch, sr = ple4_sr)

## ---- rceg4--------------------------------------------------------------
catch(ple4_rel_catch)
catch(ple4_rel_catch)[,ac(2008:2018)] / catch(ple4_rel_catch)[,ac(2007:2017)]

## ---- plotrelC, fig.cap="Relative catch example"-------------------------
plot(window(ple4_rel_catch, start = 2001, end = 2018))

## ---- meg1---------------------------------------------------------------
f01 <- 0.1

## ---- meg2---------------------------------------------------------------
min_catch <- mean(catch(ple4_mtf)[,as.character(2006:2008)])
min_catch

## ---- meg3---------------------------------------------------------------
ctrl_target <- rbind(
    f_df <- data.frame(
        year = 2009:2018,
        quant = "f",
        value = f01,
        min = NA),
    catch_df <- data.frame(
        year = 2009:2018,
        quant = "catch",
        value = NA,
        min = min_catch)
)

## ---- meg5---------------------------------------------------------------
ctrl_min_catch <- fwdControl(ctrl_target)

## ---- meg6---------------------------------------------------------------
ctrl_min_catch

## ---- meg7---------------------------------------------------------------
ple4_min_catch <- fwd(ple4_mtf, control = ctrl_min_catch, sr = ple4_sr)

## ---- megc---------------------------------------------------------------
#catch(ple4_min_catch)[,ac(2008:2018)]
#fbar(ple4_min_catch)[,ac(2008:2018)]

## ---- meg8, fig.cap="Example with a minimum catch bound and constant F target"----
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
        relYear = NA,
        quant = "f",
        value = f_target,
        max = NA,
        min = NA),
    catch_df <- data.frame(
        year = 2009:2018,
        relYear = 2008:2017,
        quant = "catch",
        value = NA,
        max = 1 + rel_catch_bound,
        min = 1 - rel_catch_bound)
)

## ---- rtbeg5-------------------------------------------------------------
ctrl_rel_min_max_catch <- fwdControl(ctrl_target)
ctrl_rel_min_max_catch

## ---- rtbeg6-------------------------------------------------------------
recovery<-fwd(ple4_mtf, control=ctrl_rel_min_max_catch, sr=ple4_sr)

## ---- rtbeg7-------------------------------------------------------------
plot(window(recovery, start = 2001, end = 2018))

## ---- rtbeg8-------------------------------------------------------------
catch(recovery)[,ac(2009:2018)] / catch(recovery)[,ac(2008:2017)]

## ---- niter--------------------------------------------------------------
niters <- 200
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

## ---- res_hack-----------------------------------------------------------
dim(multi_rec_residuals)
dim(stock(ple4_mtf))
yrs <- range(as.numeric(dimnames(stock(ple4_mtf))$year))
multi_rec_residuals <- window(multi_rec_residuals, start=yrs[1], end=yrs[2])

## ---- res6---------------------------------------------------------------
#ple4_stoch_rec <- fwd(ple4_mtf, control = ctrl_catch, sr = ple4_sr, residuals = multi_rec_residuals, sr_residuals_mult = TRUE)
ple4_stoch_rec <- fwd(ple4_mtf, control = ctrl_catch, sr = ple4_sr, residuals = multi_rec_residuals) 

## ---- res7, fig.cap="Example projection with stochasticity in the recruitment residuals"----
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
ctrl_catch_iters@iters

## ---- stv4---------------------------------------------------------------
class(ctrl_catch_iters@iters)
dim(ctrl_catch_iters@iters)

## ---- stv5---------------------------------------------------------------
new_iters <- array(NA, dim=c(10,3,niters), dimnames = list(1:10, c("min","value","max"),iter=1:niters))
dim(new_iters)

## ---- stv6---------------------------------------------------------------
future_catch_iters <- ctrl_catch_iters@iters[,"value",] * rlnorm(10 * niters, meanlog = 0, sdlog=0.3)

## ---- stv7---------------------------------------------------------------
new_iters[,"value",] <- future_catch_iters

## ---- stv8---------------------------------------------------------------
ctrl_catch_iters@iters <- new_iters

## ---- stv9---------------------------------------------------------------
ctrl_catch_iters

## ---- stv10--------------------------------------------------------------
ple4_catch_iters <- fwd(ple4_mtf, control=ctrl_catch_iters, sr = ple4_sr)

## ---- stv11--------------------------------------------------------------
plot(window(ple4_catch_iters, start = 2001, end = 2018))

## ---- stv12--------------------------------------------------------------
catch(ple4_catch_iters)[,ac(2008:2018)]

## ---- stv13--------------------------------------------------------------
rec(ple4_catch_iters)[,ac(2008:2018)]

## ---- stv14--------------------------------------------------------------
ple4_catch_iters <- fwd(ple4_mtf, control=ctrl_catch_iters, sr = ple4_sr, residuals = multi_rec_residuals)

## ---- stv15--------------------------------------------------------------
plot(window(ple4_catch_iters, start = 2001, end = 2018))

## ---- stv16--------------------------------------------------------------
catch(ple4_catch_iters)[,ac(2008:2018)]
rec(ple4_catch_iters)[,ac(2008:2018)]

