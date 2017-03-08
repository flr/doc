## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("ggplot2"))
## install.packages(c("ggplotFL"), repos="http://flr-project.org/R")
## install.packages(c("FLBRP"), repos="http://flr-project.org/R")
## 

## ---- pkgs---------------------------------------------------------------
# This chunk loads all necessary packages, trims pkg messages
library(FLCore)
library(ggplotFL)

## ------------------------------------------------------------------------
library(FLBRP)

## ------------------------------------------------------------------------
data(ple4)
brp4 <- FLBRP(ple4)


## ------------------------------------------------------------------------
summary(brp4)

## ------------------------------------------------------------------------
fbar(brp4)

## ------------------------------------------------------------------------
model(brp4)
params(brp4)

## ------------------------------------------------------------------------
ple4SR <- transform(as.FLSR(ple4, model=ricker), ssb=ssb/100, rec=rec/100)
ple4SR <- fmle(ple4SR,control=list(silent=T))
params(ple4SR)['b',] <- params(ple4SR)['b',] / 100
ple4SR <- transform(ple4SR, ssb=ssb*100, rec=rec*100)
brp4Ri <- FLBRP(ple4, sr=ple4SR)


## ------------------------------------------------------------------------
brp4Ri <- brp(brp4Ri)
refpts(brp4Ri)


## ------------------------------------------------------------------------
plot(brp4Ri)

## ------------------------------------------------------------------------
plot(brp4Ri,obs=T)

## ------------------------------------------------------------------------
# price of fish at age
price(brp4Ri) <- c(rep(1.19,3),rep(1.34,2),rep(1.57,5))
price(brp4Ri)@units <- "keuro/ton"

# variable costs per F 
vcost(brp4Ri) <- 62000
vcost(brp4Ri)@units <- "keuro/unit F"

# variable costs per F 
fcost(brp4Ri) <- 25000
fcost(brp4Ri)@units <- "keuro/unit F"


## ------------------------------------------------------------------------
brp4Eco <- brp(brp4Ri)
refpts(brp4Eco)


## ------------------------------------------------------------------------
plot(brp4Eco)

## ------------------------------------------------------------------------
(rge4 <- msyRange(brp4Eco,range=0.05))

## ------------------------------------------------------------------------
p <- plot(brp4Eco,obs=T)

p$data <- within(p$data,{minrge <- NA
                         minrge <- replace(minrge,grep("v. F",pnl),rge4@.Data["min","harvest",])
                         minrge <- replace(minrge,grep("v. SSB",pnl),rge4@.Data["max","ssb",])
                         maxrge <- NA
                         maxrge <- replace(maxrge,grep("v. F",pnl),rge4@.Data["max","harvest",])
                         maxrge <- replace(maxrge,grep("v. SSB",pnl),rge4@.Data["min","ssb",])})


p+ geom_area(aes(x = ifelse(x>=minrge & x<=maxrge , x, NA),y=y,group=iter),fill="grey",alpha=0.5)    


