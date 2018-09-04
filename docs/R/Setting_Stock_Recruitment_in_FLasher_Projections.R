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

## ------------------------------------------------------------------------
data(ple4)

## ------------------------------------------------------------------------
slots <- c("landings.n", "discards.n", "catch.n", "stock.n")
for (i in slots){
    slot(ple4, i) <- slot(ple4, i) / 1000
    # Correct units too
    units(slot(ple4, i)) <- "10^6"
}
# Recalc aggregate slots
catch(ple4) <- computeCatch(ple4)
landings(ple4) <- computeLandings(ple4)
discards(ple4) <- computeDiscards(ple4)
stock(ple4) <- computeStock(ple4)

## ------------------------------------------------------------------------
ple4mtf <- stf(ple4, 10)

## ------------------------------------------------------------------------
years <- 2009:2018
catch_target <- 100
control <- fwdControl(data.frame(year=years, quant="catch", value=catch_target))

## ------------------------------------------------------------------------
ple4_srr <- fmle(as.FLSR(ple4, model="bevholt"), control=list(trace=0))
plot(ple4_srr)

## ------------------------------------------------------------------------
test <- fwd(ple4mtf, control=control, sr=ple4_srr)

## ------------------------------------------------------------------------
rec(test)[,ac(years)]

## ------------------------------------------------------------------------
plot(test)

## ------------------------------------------------------------------------
predict(ple4_srr, ssb=ssb(test)[,ac(years-1)])

## ------------------------------------------------------------------------
bh_params <- params(ple4_srr)
bh_params

## ------------------------------------------------------------------------
test <- fwd(ple4mtf, control=control, sr=list(model = "bevholt", params=bh_params))

## ------------------------------------------------------------------------
rec(test)[,ac(years)]

## ------------------------------------------------------------------------
geomeanrec <- exp(mean(log(rec(ple4)[,ac(2006:2008)])))
geomeanrec
test <- fwd(ple4mtf, control=control, sr=list(model = "geomean", params=FLPar(a=geomeanrec)))
rec(test)[,ac(years)]

## ------------------------------------------------------------------------
decrec <- seq(900, 500, length=10)
decpar <- FLPar(decrec, dimnames=list(params="a", year=2009:2018, iter=1))
test <- fwd(ple4mtf, control=control, sr=list(model = "geomean", params=decpar))
# Warnings - but recruitment looks OK
rec(test)[,ac(years)]

## ---- eval=FALSE---------------------------------------------------------
## badpar <- FLPar(1000, dimnames=list(params="a", year=2009, iter=1))
## test <- fwd(ple4mtf, control=control, sr=list(model = "geomean", params=badpar))

## ------------------------------------------------------------------------
a <- c(params(ple4_srr)["a"])
acycle <- a * (1 + sin(seq(from=0,to=2*pi,length=10))/10)
srpar <- FLPar(NA, dimnames=list(params=c("a","b"), year=2009:2018, iter=1))
srpar["a"] <- acycle
srpar["b"] <- c(params(ple4_srr)["b"])
srpar

## ------------------------------------------------------------------------
test <- fwd(ple4mtf, control=control, sr=list(model = "bevholt", params=srpar))
rec(test)[,ac(years)]

## ------------------------------------------------------------------------
niters <- 200
ple4mtfp <- propagate(ple4mtf, niters)

## ------------------------------------------------------------------------
test <- fwd(ple4mtfp, control=control, sr=list(model = "bevholt", params=bh_params))
rec(test)[,ac(years)]

## ------------------------------------------------------------------------
res <- FLQuant(NA, dimnames=list(year=years, iter=1))
# Sample from SRR params with replacement
# Residuals n SRR are on log scale and must be transformed
res[] <- sample(c(exp(residuals(ple4_srr))), prod(dim(res)), replace=TRUE)
res

## ------------------------------------------------------------------------
test <- fwd(ple4mtf, control=control, sr=ple4_srr, residuals=res)
rec(test)[,ac(years)]
# Predicted recruitment * residuals
predict(ple4_srr, ssb=ssb(test)[,ac(years-1)]) %*% res

## ------------------------------------------------------------------------
plot(test)

## ------------------------------------------------------------------------
# Residuals with multiple iterations - more useful
res <- FLQuant(NA, dimnames=list(year=years, iter=1:niters))
res[] <- sample(c(exp(residuals(ple4_srr))), prod(dim(res)), replace=TRUE)
# Project with residuals
test <- fwd(ple4mtfp, control=control, sr=ple4_srr, residuals=res)
rec(test)[,ac(years)]
# Check the predicted recruitment * residuals
predict(ple4_srr, ssb=ssb(test)[,ac(years-1)]) %*% res

## ------------------------------------------------------------------------
plot(test)

## ------------------------------------------------------------------------
vc <- vcov(ple4_srr)[,,1]
vc
params(ple4_srr)

## ------------------------------------------------------------------------
# Get correlation matrix
invsd <- solve(sqrt(diag(diag(vc))))
corr_matrix <- invsd %*% vc %*% invsd
# Assume CoV of 25% on each parameter
# So SDs of the params will be
newsd <- diag(c(params(ple4_srr)) * 0.25)
newvc <- newsd %*% corr_matrix %*% newsd
# Sample from a multivariate norm to generate new parameters with the appropriate correlation
newparams <- mvrnorm(niters, mu = c(params(ple4_srr)), Sigma=newvc)
# Hope none of them are -ve...
# Replace -ve values with something small - hacky but whatever
newparams[newparams<=0] <- 1

## ------------------------------------------------------------------------
iter_params <- propagate(params(ple4_srr),niters)
iter_params["a",] <- newparams[,1]
iter_params["b",] <- newparams[,2]
iter_params

## ------------------------------------------------------------------------
test <- fwd(ple4mtfp, control=control, sr=list(model="bevholt", params=iter_params))
rec(test)[,ac(years)]

## ------------------------------------------------------------------------
test <- fwd(ple4mtfp, control=control, sr=list(model="bevholt", params=iter_params), residuals=res)

## ------------------------------------------------------------------------
plot(test)

## ------------------------------------------------------------------------
biol <- as(ple4mtf, "FLBiol")
fishery <- as(ple4mtf, "FLFishery")

## ------------------------------------------------------------------------
is(biol@rec)

## ------------------------------------------------------------------------
biol@rec@params
biol@rec@model

## ------------------------------------------------------------------------
bevholt()[["model"]]
biol@rec@model <- bevholt()[["model"]]

## ------------------------------------------------------------------------
biol@rec@model <- model(ple4_srr)

## ------------------------------------------------------------------------
biol@rec@params <- params(ple4_srr)

## ------------------------------------------------------------------------
test <- fwd(biol, fishery=fishery, control=control)
n(test[["biols"]])[1,ac(years)]

## ------------------------------------------------------------------------
biol@rec <- predictModel(model=model(ple4_srr), params=params(ple4_srr))

## ------------------------------------------------------------------------
test <- fwd(biol, fishery=fishery, control=control)
n(test[["biols"]])[1,ac(years)]

## ------------------------------------------------------------------------
decrec <- seq(900, 500, length=10)
decpar <- FLPar(decrec, dimnames=list(params="a", year=2009:2018, iter=1))
biol@rec@model <- geomean()[["model"]]
biol@rec@params <- decpar
test <- fwd(biol, fishery=fishery, control=control)
n(test[["biols"]])[1,ac(years)]

## ------------------------------------------------------------------------
biolp <- biol
# Just propagate the n slot
biolp@n <- propagate(biolp@n, niters)
# Do all slots of fishery - need to do Catch separately - a bug
fisheryp <- propagate(fishery, niters)
fisheryp[[1]] <- propagate(fishery[[1]], niters)

## ------------------------------------------------------------------------
biolp@rec@model <- model(ple4_srr)
biolp@rec@params <- params(ple4_srr)

## ------------------------------------------------------------------------
test <- fwd(biolp, fishery=fisheryp, control=control, residuals=res)
n(test[["biols"]])[1,ac(years)]

## ------------------------------------------------------------------------
biolp@rec@params <- iter_params
test <- fwd(biolp, fishery=fisheryp, control=control)
n(test[["biols"]])[1,ac(years)]

## ------------------------------------------------------------------------
test <- fwd(biolp, fishery=fisheryp, control=control, residuals=res)
n(test[["biols"]])[1,ac(years)]

