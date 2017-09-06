## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("lattice"))
## install.packages(c("FLCore"), repos="http://flr-project.org/R")

## ---- pkgs---------------------------------------------------------------
library(FLCore)
library(lattice)

## ---- datasets-----------------------------------------------------------
#Read FLR examples
data(ple4)
data(nsher)

## ---- fig1---------------------------------------------------------------
# Plot FLStock
plot(ple4)

## ---- fig2---------------------------------------------------------------
# plot FLQuant
plot(catch.n(ple4)/1000, ylab="Catch numbers (thousands)",
      scales = list(y = list(relation = 'free')))

## ---- fig3---------------------------------------------------------------
# Plot FLSR
plot(nsher)

## ---- dataframe----------------------------------------------------------
head(as.data.frame(catch(ple4)))

## ---- fig4---------------------------------------------------------------
xyplot(data/1000~year, data=catch(ple4), type='b', pch=19,
       ylab="catch (thousand tonnes)",xlab='')

## ---- fig5---------------------------------------------------------------
xyplot(data/1000~year, groups=age, data=catch.n(ple4), type='l', 
  auto.key=list(space='bottom',columns=5, cex=0.7),
  ylab='Catch numbers at age (10^6)',xlab='')

## ---- fig6---------------------------------------------------------------
xyplot(data/1000~year|factor(age), data=catch.n(ple4), type='l',
  scales = list(y = list(relation = 'free')), ylab='Catch numbers (10^6)',xlab='')

## ---- fig7---------------------------------------------------------------
xyplot(data~year|qname, data=FLQuants(SSB=ssb(ple4), Yield=catch(ple4), 
  Landings=landings(ple4)),xlab='',type='l')

## ---- fig8---------------------------------------------------------------
barchart(data/1000~factor(year), 
         data=landings(ple4),
         ylab =list(label="thousand tonnes",cex=0.8),scales=list(x=list(rot=90)),
         type="v", main = "Total landings" )

## ---- fig10--------------------------------------------------------------
bwplot(data~year, rlnorm(200, fbar(ple4), 0.15),scales=list(x=list(rot=90)), ylab="Fbar")

## ---- fig11--------------------------------------------------------------
dotplot(data~year,groups=age,harvest(ple4),
        scales=list(x=list(rot=90)), auto.key=list(space='bottom',columns=5, cex=0.7),ylab="F at age")

## ---- fig12--------------------------------------------------------------
histogram(~data|age, catch.n(ple4), xlab='Catch numbers')

## ---- fig13--------------------------------------------------------------
wireframe(data~age+year, data=harvest(ple4),zlab="F",drape = TRUE,
        col.regions = colorRampPalette(c("green", "red"))(100))

## ---- fig14--------------------------------------------------------------
bubbles(age~year, data=catch.n(ple4), xlab='', bub.scale=5)

## ---- fig15--------------------------------------------------------------
bubbles(age~year, data=catch.n(ple4)[5:10,], xlab='', bub.scale=10)

