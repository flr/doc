## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("copula","triangle", "coda", "XML", "reshape2", "latticeExtra"))
## # from FLR
## install.packages(c("FLCore", "FLa4a"), repos="http://flr-project.org/R")

## ---- pkgs---------------------------------------------------------------
# This chunk loads all necessary packages, trims pkg messages
library(FLa4a)
library(XML)
library(reshape2)
library(latticeExtra)
# datasets
data(ple4)
data(ple4.indices)
data(ple4.index)
data(rfLen)

## ---- show_a4aGr---------------------------------------------------------
showClass("a4aGr")

## ---- a4aGr_vB_example---------------------------------------------------
vbObj <- a4aGr(
	grMod=~linf*(1-exp(-k*(t-t0))),      
	grInvMod=~t0-1/k*log(1-len/linf),      
	params=FLPar(linf=58.5, k=0.086, t0=0.001, units=c('cm','year-1','year'))     
)

# Check the model and its inverse
lc=20
predict(vbObj, len=lc)
predict(vbObj, t=predict(vbObj, len=lc))==lc

## ---- predict_araGr_example----------------------------------------------
predict(vbObj, len=5:10+0.5)
predict(vbObj, t=5:10+0.5)

## ---- set_vcov_example---------------------------------------------------
# Make an empty cor matrix
cm <- diag(c(1,1,1))
# k and linf are negatively correlated while t0 is independent
cm[1,2] <- cm[2,1] <- -0.5
# scale cor to var using CV=0.2
cv <- 0.2
p <- c(linf=60, k=0.09, t0=-0.01)
vc <- matrix(1, ncol=3, nrow=3)
l <- vc
l[1,] <- l[,1] <- p[1]*cv
k <- vc
k[,2] <- k[2,] <- p[2]*cv
t <- vc
t[3,] <- t[,3] <- p[3]*cv
mm <- t*k*l
diag(mm) <- diag(mm)^2
mm <- mm*cm
# check that we have the intended correlation
all.equal(cm, cov2cor(mm))

## ---- making_vcov_example------------------------------------------------
vbObj <- a4aGr(grMod=~linf*(1-exp(-k*(t-t0))), grInvMod=~t0-1/k*log(1-len/linf), 
               params=FLPar(linf=p['linf'], k=p['k'], t0=p['t0'], 
                            units=c('cm','year-1','year')), vcov=mm)

## ---- simulate_vcov_example----------------------------------------------
# Note that the object we have just created has a single iteration of each parameter
vbObj@params
dim(vbObj@params)
# We simulate 10000 iterations from the a4aGr object by calling mvrnorm() using the 
# variance-covariance matrix we created earlier.
vbNorm <- mvrnorm(10000,vbObj)
# Now we have 10000 iterations of each parameter, randomly sampled from the 
# multivariate normal distribution
vbNorm@params
dim(vbNorm@params)

## ------------------------------------------------------------------------
ages <- predict(vbNorm, len=5:10+0.5)
dim(ages)
# We show the first ten iterations only as an illustration
ages[,1:10]

## ---- plot_norm_params, fig.cap="The marginal distributions of each of the parameters from using a multivariate normal distribution.", echo=FALSE----
par(mfrow=c(3,1))
hist(c(params(vbNorm)['linf',]), main='linf', prob=TRUE, xlab='')
hist(c(params(vbNorm)['k',]), main='k', prob=TRUE, xlab='')
hist(c(params(vbNorm)['t0',]), main='t0', prob=TRUE, xlab='')

## ---- plot_norm_scatter, fig.cap="Scatter plot of the 10000 samples parameter from the multivariate normal distribution.", echo=FALSE----
splom(data.frame(t(params(vbNorm)@.Data)), par.settings=list(plot.symbol=list(pch=19, cex=0.1, col=1)))

## ---- plot_mv_growth, fig.cap="Growth curves using parameters simulated from a multivariate normal distribution.", echo=FALSE----
#df0 <- melt(predict(vbNorm, t=0:50+0.5))
bwplot(value~factor(Var1), data=melt(predict(vbNorm, t=0:50+0.5)), par.settings=list(plot.symbol=list(cex=0.2, col='gray50'), box.umbrella=list(col='gray40'), box.rectangle=list(col='gray30')), ylab='length (cm)', xlab='age (years)', scales=list(x=list(rot=90)))
#boxplot(t(predict(vbNorm, t=0:50+0.5)))

## ---- tri_example--------------------------------------------------------
# The web address for the growth parameters for redfish (Sebastes norvegicus)
addr <- 'http://www.fishbase.org/PopDyn/PopGrowthList.php?ID=501'
# Scrape the data
tab <- try(readHTMLTable(addr))
# Interrogate the data table and get vectors of the values
linf <- as.numeric(as.character(tab$dataTable[,2]))
k <- as.numeric(as.character(tab$dataTable[,4]))
t0 <- as.numeric(as.character(tab$dataTable[,5]))
# Set the min (a), max (b) and median (c) values for the parameter as a list of lists
# Note that t0 has no 'c' (median) value. This makes the distribution symmetrical
triPars <- list(list(a=min(linf), b=max(linf), c=median(linf)),
             list(a=min(k), b=max(k), c=median(k)),
             list(a=median(t0, na.rm=T)-IQR(t0, na.rm=T)/2, b=median(t0, na.rm=T)+
                    IQR(t0, na.rm=T)/2))
# Simulate 10000 times using mvrtriangle
vbTri <- mvrtriangle(10000, vbObj, paramMargins=triPars)

## ---- plot_tri_params, echo=FALSE, fig.cap="The marginal distributions of each of the parameters from using a multivariate triangle distribution."----
par(mfrow=c(3,1))
hist(c(params(vbTri)['linf',]), main='linf', prob=TRUE, xlab='')
hist(c(params(vbTri)['k',]), main='k', prob=TRUE, xlab='')
hist(c(params(vbTri)['t0',]), main='t0', prob=TRUE, xlab='')

## ---- plot_tri_scatter, echo=FALSE, fig.cap="Scatter plot of the 10000 samples parameter from the multivariate triangle distribution."----
splom(data.frame(t(params(vbTri)@.Data)), par.settings=list(plot.symbol=list(pch=19, cex=0.1, col=1)))

## ---- plot_tri_growth, echo=FALSE, fig.cap="Growth curves using parameters simulated from a multivariate triangle distribution."----
#df0 <- melt(predict(vbTri, t=0:50+0.5))
bwplot(value~factor(Var1), data=melt(predict(vbTri, t=0:50+0.5)), par.settings=list(plot.symbol=list(cex=0.2, col='gray50'), box.umbrella=list(col='gray40'), box.rectangle=list(col='gray30')), ylab='length (cm)', xlab='age (years)', scales=list(x=list(rot=90)))
#boxplot(t(predict(vbTri, t=0:20+0.5)))

## ---- copula_triangle_example--------------------------------------------
vbCop <- mvrcop(10000, vbObj, copula='archmCopula', family='clayton', param=2, 
                margins='triangle', paramMargins=triPars)

## ---- plot_cop_tri_scatter, echo=FALSE, fig.cap="Scatter plot of the 10000 samples parameter from the using an archmCopula copula with triangle margins."----
splom(data.frame(t(params(vbCop)@.Data)), par.settings=list(plot.symbol=list(pch=19, cex=0.1, col=1)))

## ---- plot_cop_tri_growth, fig.cap="Growth curves from the using an archmCopula copula with triangle margins.", echo=FALSE----
#boxplot(t(predict(vbCop, t=0:20+0.5)))
#df0 <- melt(predict(vbCop, t=0:50+0.5))
bwplot(value~factor(Var1), data=melt(predict(vbCop, t=0:50+0.5)), par.settings=list(plot.symbol=list(cex=0.2, col='gray50'), box.umbrella=list(col='gray40'), box.rectangle=list(col='gray30')), ylab='length (cm)', xlab='age (years)', scales=list(x=list(rot=90)))


## ---- FLQ_l2a, message=FALSE---------------------------------------------
vbTriSmall <- mvrtriangle(10, vbObj, paramMargins=triPars)
cth.n <- l2a(catch.n(rfLen.stk), vbTriSmall)

## ---- example_flq_slice--------------------------------------------------
dim(cth.n)

## ---- FLS_FLI_l2a, message=FALSE, warning=TRUE---------------------------
aStk <- l2a(rfLen.stk, vbTriSmall, plusgroup=14)
aIdx <- l2a(rfTrawl.idx, vbTriSmall)

