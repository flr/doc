## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")

## ----  eval=FALSE--------------------------------------------------------
## install.packages("ggplot2")
## install.packages(c("ggplotFL"), repos="http://flr-project.org/R")

## ---- echo=TRUE, results='hide', message=FALSE---------------------------
# This chunk loads all necessary packages, trims pkg messages
library(FLCore)
library(ggplotFL)

# Load datasets for tutorial

data("ple4")
data("ple4sex")
data("nsher")

## ------------------------------------------------------------------------
# summary of the FLStock
summary(ple4)

## ------------------------------------------------------------------------
head(as.data.frame(ple4))

## ------------------------------------------------------------------------

ggplot(data = catch(ple4), aes(year, data)) + geom_point() + geom_line() + ylab("Catch (t)") + xlab("Year")

## ---- ggflqs, echo=TRUE, fig.cap="Facet wrap line plot of time series from an FLQuants object."----
ggplot(data=FLQuants(Yield=catch(ple4), SSB=ssb(ple4), F=fbar(ple4)), aes(year, data)) + 
  geom_line() + facet_wrap(~qname, scales="free_y", nrow=3) + labs(x="", y="")

## ---- ggfls, echo=TRUE, fig.cap="Overall `ggplot` of an `FLStock` object, faceted by slot.", fig.width=24, fig.height=6----
ggplot(data=ple4, aes(year, data)) + geom_line(aes(group=age, colour=factor(age))) + 
  facet_wrap(~slot, scales="free", nrow=3) + labs(x="", y="") + theme(legend.position = "none")


## ---- dimflq, echo=TRUE--------------------------------------------------
dim(catch.n(ple4))

## ---- pflq, echo=TRUE, fig.cap="Standard ggplot2-based plot for an FLQuant object with multiple *year*s and *age*s."----
plot(catch.n(ple4))

## ---- pflq2, echo=TRUE, fig.cap="Standard ggplot2-based plot for an FLQuant object with multiple iterations."----
plot(rlnorm(200, fbar(ple4), 0.15))

## ---- pflq3, echo=TRUE, fig.cap="Standard ggplot2-based plot for an FLQuant object with multiple iterations and user-specified quantiles."----
plot(rlnorm(200, fbar(ple4), 0.15), probs=c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95))

## ---- pflqs, echo=TRUE, fig.cap="Standard ggplot2-based plot for an FLQuants object with multiple iterations, and consisting of three elements."----
fqs <- FLQuants(F = rlnorm(200, fbar(ple4), 0.15), SSB = ssb(ple4), Rec = rec(ple4), Catch = catch(ple4))
plot(fqs)

## ---- pflqsg, echo=TRUE, fig.cap="Wrap-based ggplot2-based plot for an FLQuants object with multiple iterations, and consisting of three elements."----
fqs <- FLQuants(F = rlnorm(200, fbar(ple4), 0.15), SSB = ssb(ple4), Rec = rec(ple4), Catch = catch(ple4))
plot(fqs) + facet_wrap(~qname, scales="free")

## ---- pfls,  fig.cap="ggplot2 version of the standard plot() for FLStock, as applied to `ple4`"----
plot(ple4)

## ---- pflss, fig.cap="ggplot2 version of the standard plot() for FLStocks, as applied to the sex-separated FLStock object `ple4sex`"----
plot(FLStocks(Male=ple4sex[,,'male'], Female=ple4sex[,,'female'])) + theme(legend.position="top")

## ---- pflsr, echo=TRUE, fig.cap="Standard ggplot2-based plot for an object of class FLSR."----
plot(nsher)

## ---- flsrs--------------------------------------------------------------
srs <- FLSRs(sapply(c('ricker', 'bevholt'), function(x) {
                                                    y <- nsher
                                                    model(y) <- x
                                                    return(fmle(y))
}))

## ---- pflsrs, echo=TRUE, fig.cap="Standard ggplot2-based plot for an FLSRs object, using default legend labels."----
plot(srs)

## ---- pflsrs2, echo=TRUE, fig.cap="Standard ggplot2-based plot for an FLSRs object, using model names as legend labels."----
plot(srs, legend_label=modlabel) 

## ---- pflsrs3, echo=TRUE, fig.cap="Standard ggplot2-based plot for an FLSRs object, using model names as legend labels."----
plot(srs) + scale_color_discrete(name="SR models", breaks=c('ricker', 'bevholt'),
labels=c("Ricker", "Beverton & Holt"))

## ---- exsim1, echo=TRUE, cache=TRUE, fig.cap="Distribution of values of a simulated time series plotted using geom_boxplot()"----
fla <- rlnorm(100, FLQuant(exp(cumsum(rnorm(25, 0, 0.1)))), 0.1) 
ggplot(fla, aes(factor(year), data)) + geom_boxplot() + xlab("")

## ---- exsim2, echo=TRUE, cache=TRUE--------------------------------------
flq <- quantile(fla, c(0.10, 0.25, 0.50, 0.75, 0.90))

## ---- exsim3, echo=TRUE, cache=TRUE--------------------------------------
fdf <- as.data.frame(flq, drop=TRUE)

## ---- exsim4, cache=TRUE, results='markup'-------------------------------
head(fdf, 3)

## ---- exsim5, echo=TRUE, cache=TRUE--------------------------------------
fdw <- reshape(fdf, timevar = "iter", idvar = c("year"), direction = "wide")

## ---- exsim6, echo=TRUE, cache=TRUE--------------------------------------
levels(fdf[,'iter'])

## ---- exsim7, echo=TRUE, cache=TRUE, fig.cap="Time series with 75% and 90% credibility intervals plotted using geom_ribbon."----
p <- ggplot(data=fdw, aes(x=year, y=`data.50%`)) +
geom_ribbon(aes(x=year, ymin = `data.10%`, ymax = `data.90%`), fill="red", alpha = .15) +
geom_ribbon(aes(x=year, ymin = `data.25%`, ymax = `data.75%`), fill="red", alpha = .25) +
geom_line() + ylab("data")
print(p)

## ---- exspa, echo=TRUE, cache=TRUE, fig.cap="Spaghetti plot of an stochastic simulation, by calling geom_line on top of the stored ribbon plot."----
fds  <- as.data.frame(iter(fla, c(1, 4, 23)))

p + geom_line(data=fds, aes(year, data, colour=iter), size=0.5) +
theme(legend.position = "none")

## ---- exbub, echo=TRUE, cache=TRUE, fig.cap="Bubble plot of catch by age in numbners for North Sea plaice."----
ggplot(catch.n(ple4), aes(year, as.factor(age), size=data)) + geom_point(shape=21) + 
  scale_size(range = c(1, 20)) + ylab("age") + theme(legend.position = "none")

## ---- exres, echo=TRUE, cache=TRUE, fig.cap=""---------------------------
dat <- as.data.frame(catch.n(ple4))
dat$resid <- dat$data - mean(dat$data)

ggplot(dat, aes(year, as.factor(age), size=resid)) +
geom_point(shape=21, aes(colour=factor(sign(resid)), fill=factor(sign(resid)))) +
scale_size(range = c(1, 20)) +
scale_colour_manual(values=c("black", "white")) +
scale_fill_manual(values=c("lightgray", "black")) +
ylab("age")

