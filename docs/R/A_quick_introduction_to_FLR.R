## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("FLCore"), repos="http://flr-project.org/R")

## ---- flcore-------------------------------------------------------------
library(FLCore)

## ---- getfiles, message=FALSE--------------------------------------------
dir <- tempdir()
download.file("http://www.flr-project.org/doc/src/ple4.csv.zip", file.path(dir, "ple4.csv.zip"))
unzip(file.path(dir, "ple4.csv.zip"), exdir=dir)

## ---- loadple4-----------------------------------------------------------
dat <- read.csv(file.path(dir, "ple4.csv"))
head(dat)

## ---- subsetlandingsn----------------------------------------------------
landn <- subset(dat, slot=="landings.n", select=-slot)

## ---- convertlandingsn---------------------------------------------------
landsn <- as.FLQuant(landn)

## ---- plotlandings.n-----------------------------------------------------
summary(landsn)

plot(landsn)

## ---- convertple4--------------------------------------------------------
ple4 <- as.FLStock(dat)

summary(ple4)

## ---- ple4m--------------------------------------------------------------
m(ple4) <- 0.1

## ---- ple4spwn-----------------------------------------------------------
m.spwn(ple4) <- harvest.spwn(ple4) <- 0

## ---- ple4mat------------------------------------------------------------
mat(ple4) <- c(0, 0.5, 0.5, rep(1, 7))

## ---- ple4compute--------------------------------------------------------
landings(ple4) <- computeLandings(ple4)
discards(ple4) <- computeDiscards(ple4)

## ---- ple4catch----------------------------------------------------------
catch(ple4) <- computeCatch(ple4, slot="all")

## ---- ple4range----------------------------------------------------------
range(ple4, c("minfbar", "maxfbar")) <- c(2, 6)

## ---- ple4---------------------------------------------------------------
summary(ple4)

plot(metrics(ple4, Catch=catch, Landings=landings))

