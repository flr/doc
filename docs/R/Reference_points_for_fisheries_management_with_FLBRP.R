## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("ggplot2"))
## install.packages(c("ggplotFL"), repos="http://flr-project.org/R")
## install.packages(c("FLBRP"), repos="http://flr-project.org/R")
## 

## ---- pkgs---------------------------------------------------------------
# Loads all necessary packages
library(FLCore)
library(ggplotFL)
library(FLBRP)

## ---- brp_ex1------------------------------------------------------------
data(ple4)
brp4 <- FLBRP(ple4)

## ---- brp_ex2------------------------------------------------------------
summary(brp4)

## ---- brp_ex3------------------------------------------------------------
fbar(brp4)

## ---- brp_ex4------------------------------------------------------------
model(brp4)
params(brp4)

## ---- brp_ex5------------------------------------------------------------
ple4SR <- transform(as.FLSR(ple4, model=ricker), ssb=ssb/100, rec=rec/100)
ple4SR <- fmle(ple4SR,control=list(silent=T))
params(ple4SR)['b',] <- params(ple4SR)['b',] / 100
ple4SR <- transform(ple4SR, ssb=ssb*100, rec=rec*100)
brp4Ri <- FLBRP(ple4, sr=ple4SR)

## ---- brp_ex6------------------------------------------------------------
brp4Ri <- brp(brp4Ri)
refpts(brp4Ri)

## ---- brp_ex7------------------------------------------------------------
plot(brp4Ri)

## ---- brp_ex8------------------------------------------------------------
plot(brp4Ri,obs=T)

## ---- erp_ex1------------------------------------------------------------
# price of fish at age
price(brp4Ri) <- c(rep(1.15,3),rep(1.3,2),rep(1.55,5))
price(brp4Ri)@units <- "keuro/ton"
# variable costs per F 
vcost(brp4Ri) <- 70000
vcost(brp4Ri)@units <- "keuro/unit F"
# fixed costs per F 
fcost(brp4Ri) <- 35000
fcost(brp4Ri)@units <- "keuro/unit F"

## ---- erp_ex2------------------------------------------------------------
brp4Eco <- brp(brp4Ri)
refpts(brp4Eco)

## ---- erp_ex3------------------------------------------------------------
plot(brp4Eco)

## ---- pgy_ex1------------------------------------------------------------
(rge4 <- msyRange(brp4Eco,range=0.05))

## ---- pgy_ex2------------------------------------------------------------
p <- plot(brp4Eco,obs=T)
p$data <- within(p$data,{minrge <- NA
                         minrge <- replace(minrge,grep("v. F",pnl),rge4@.Data["min","harvest",])
                         minrge <- replace(minrge,grep("v. SSB",pnl),rge4@.Data["max","ssb",])
                         maxrge <- NA
                         maxrge <- replace(maxrge,grep("v. F",pnl),rge4@.Data["max","harvest",])
                         maxrge <- replace(maxrge,grep("v. SSB",pnl),rge4@.Data["min","ssb",])})
p+ geom_area(aes(x = ifelse(x>=minrge & x<=maxrge , x, NA),y=y,group=iter),fill="grey",alpha=0.5)    

