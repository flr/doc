## ---- pkgs, echo=FALSE, message=FALSE, warnings=FALSE--------------------
library(FLasher)
library(knitr)
opts_chunk$set(dev='png', cache=FALSE, fig.width=10, fig.height=10, tidy=TRUE, dpi=72)
options(width=60)

## ---- plotfuncs----------------------------------------------------------
# Handy function to get F from OP from FLasher
getf <- function(op, fn=1, cn=1, bn=1, age_range = c(2,6)){
    # f = alpha * sel * effort
    flf <- op[["fisheries"]][[fn]]
    flc <- flf[[cn]]
    b <- op[["biols"]][[bn]]
    f <- ((flc@catch.q[1,] * quantSums(b@n * b@wt) ^ (-1*flc@catch.q[2,])) * flf@effort) %*% flc@catch.sel
    fbar <- apply(f[age_range[1]:age_range[2],],2:6,mean)
    return(fbar)
}

getrevcatch <- function(catch){
    return(quantSums(catch@price * catch@landings.n * catch@landings.wt))
}

getrev <- function(fishery, catch=NA){
    if(is.na(catch)){
        # get all catches
        revs <- lapply(fishery, function(x) getrevcatch(x))
        rev <- Reduce("+",revs)
    }
    else {
        rev <- getrevcatch(fishery[[catch]])
    }
    return(rev)
}

# Plotting functions
plot_biomass <- function(biol, stock_name, years=2:20){
    tb <- c(quantSums(biol@n * biol@wt)[,ac(years)])
    plot(years, tb, type="l", xlab="Year", ylab="Biomass", main=paste(stock_name, " biomass", sep=""))
}

plot_catch <- function(biol_no, op, FCB, stock_name, years=2:20, legpos = "topleft"){
    fcbf <- FCB[FCB[,"B"] == biol_no,,drop=FALSE]
    partialc <- list()
    for (i in 1:nrow(fcbf)){
        catch_name <- names(op[["fisheries"]][[fcbf[i,"F"]]])[fcbf[i,"C"]]
        partialc[[catch_name]] <- c(catch(op[["fisheries"]][[fcbf[i,"F"]]][[fcbf[i,"C"]]])[,ac(years)])
    }
    totalc <- Reduce("+",partialc)
    minc <- min(unlist(lapply(partialc, min))) * 0.9
    maxc <- max(totalc) * 1.1
    colours <- c("blue","red")
    plot(years, totalc, type="l", xlab="Year", ylab="Catch", main=paste(stock_name, " catch", sep=""), ylim=c(minc, maxc))
    if (length(partialc) > 1){
        legend_names <- "Total"
        legend_cols <- "black"
        for (i in 1:length(partialc)){
            lines(years,partialc[[i]],col=colours[i])
            legend_names <- c(legend_names, names(partialc)[i])
            legend_cols <- c(legend_cols, colours[i])
        }
        legend(legpos, legend=legend_names, col=legend_cols, lty=1)
    }
}

plot_f <- function(biol_no, op, FCB, stock_name, years=2:20, legpos = "topleft"){
    fcbf <- FCB[FCB[,"B"] == biol_no,,drop=FALSE]
    partialf <- list()
    for (i in 1:nrow(fcbf)){
        catch_name <- names(op[["fisheries"]][[fcbf[i,"F"]]])[fcbf[i,"C"]]
        partialf[[catch_name]] <- c(getf(op, fn=fcbf[i,"F"], cn=fcbf[i,"C"], bn=biol_no)[,ac(years)])
    }
    totalf <- Reduce("+",partialf)
    minf <- min(unlist(lapply(partialf, min))) * 0.9
    maxf <- max(totalf) * 1.1
    colours <- c("blue","red")
    plot(years, totalf, type="l", xlab="Year", ylab="F", main=paste(stock_name, " F", sep=""), ylim=c(minf, maxf), col="black")
    if (length(partialf) > 1){
        legend_names <- "Total"
        legend_cols <- "black"
        for (i in 1:length(partialf)){
            lines(years,partialf[[i]],col=colours[i])
            legend_names <- c(legend_names, names(partialf)[i])
            legend_cols <- c(legend_cols, colours[i])
        }
        legend(legpos, legend=legend_names, col=legend_cols, lty=1)
    }
}

plot_revenue <- function(fisheries, years=2:20, legpos="topleft"){
    nf <- length(fisheries)
    revs <- list()
    for (i in 1:nf){
        revs[[i]] <- c(getrev(fisheries[[i]])[,ac(years)])
    }
    minr <- min(unlist(lapply(revs, min))) * 0.9
    maxr <- max(unlist(lapply(revs, max))) * 1.1
    colours <- c("blue","red")
    plot(years, revs[[1]], type="l", ylim=c(minr, maxr), xlab = "Year", ylab="Revenue", main="Fishery revenue", col=colours[1])
    if(length(revs)>1){
        legend_names <- names(fisheries)[1]
        legend_cols <- colours[1]
        for (i in 2:length(revs)){
            lines(years, revs[[i]],col=colours[i])
            legend_names <- c(legend_names, names(fisheries)[i])
            legend_cols <- c(legend_cols, colours[i])
        }
        legend(legpos, legend=legend_names, col=legend_cols, lty=1)
    }
}

plot_effort <- function(fisheries, years=2:20, legpos="topleft"){
    nf <- length(fisheries)
    eff <- list()
    for (i in 1:nf){
        eff[[i]] <- c(fisheries[[i]]@effort[,ac(years)])
    }
    mine <- min(unlist(lapply(eff, min))) * 0.9
    maxe <- max(unlist(lapply(eff, max))) * 1.1
    colours <- c("blue","red")
    plot(years, eff[[1]], type="l", ylim=c(mine, maxe), xlab = "Year", ylab="Effort", main="Fishery relative effort", col=colours[1])
    if(length(eff)>1){
        legend_names <- names(fisheries)[1]
        legend_cols <- colours[1]
        for (i in 2:length(eff)){
            lines(years, eff[[i]],col=colours[i])
            legend_names <- c(legend_names, names(fisheries)[i])
            legend_cols <- c(legend_cols, colours[i])
        }
        legend(legpos, legend=legend_names, col=legend_cols, lty=1)
    }
}

## ---- load_om------------------------------------------------------------
data(mixed_fishery_example_om)

## ---- Srs----------------------------------------------------------------
# Plaice
biols[["ple"]]@rec
biols[["sol"]]@rec

## ---- plotSRRs, echo=FALSE, fig.cap = "The stock-recruitment relationships of the plaice and sole stocks"----
ssb_ple <- FLQuant(seq(from=0, to=9e5, length=100))
rec_ple <- predict(biols[["ple"]]@rec@model, biols[["ple"]]@rec@params, ssb=ssb_ple)
ssb_sol <- FLQuant(seq(from=0, to=130000, length=100))
rec_sol <- predict(biols[["sol"]]@rec@model, biols[["sol"]]@rec@params, ssb=ssb_sol)
par(mfrow=c(2,1))
plot(c(ssb_ple), c(rec_ple), type="l", col="blue", xlab="SSB", ylab="Recruitment", main="Plaice")
plot(c(ssb_sol), c(rec_sol), type="l", col="blue", xlab="SSB", ylab="Recruitment", main="Sole")

## ---- plot_sel_pattern, echo=FALSE, fig.height = 8, fig.cap="Selection patterns for the beam trawl (black) and gillnet (blue) fisheries on sole and plaice."----
par(mfrow=c(2,1))
plot(1:10, c(flfs[["bt"]][["pleBT"]]@catch.sel[,1]), type="l", col="black",ylim=c(0,1),xlab="Ages",
    ylab="Selectivity", main="Plaice")
lines(1:10, c(flfs[["gn"]][["pleGN"]]@catch.sel[,1]), col="blue")
legend("right", legend=c("Beam trawl", "Gillnet"), col=c("black", "blue"), lty=1)
plot(1:10,c(flfs[["bt"]][["solBT"]]@catch.sel[,1]), type="l", col="black", ylim=c(0,1),xlab="Ages",
    ylab="Selectivity", main="Sole")
lines(1:10,c(flfs[["gn"]][["solGN"]]@catch.sel[,1]), col="blue")
legend("right", legend=c("Beam trawl", "Gillnet"), col=c("black", "blue"), lty=1)

## ---- make_single_f_s----------------------------------------------------
# First pull out a single FLCatch from the BT fishery using [[]]
pleBT <- flfs[["bt"]][["pleBT"]]
# Make a single beam trawl FLFishery with 1 FLCatch
bt1 <- FLFishery(pleBT=pleBT)
# Set the initial effort
bt1@effort[] <- 1
# Make an FLFisheries from it
flfs1 <- FLFisheries(bt=bt1)
# Make an FLBiols with a single FLBiol
biols1 <- FLBiols(ple=biols[["ple"]])

## ------------------------------------------------------------------------
fcb <- matrix(1, nrow=1, ncol=3, dimnames=list(1,c("F","C","B")))
fcb

## ------------------------------------------------------------------------
catch_target <- 100000
flasher_ctrl_target <- data.frame(year = 2:20,
                        quant = "catch",
                        biol = "ple",
                        value = catch_target)
flasher_ctrl <- fwdControl(flasher_ctrl_target, FCB=fcb)

## ------------------------------------------------------------------------
flasher_ctrl <- fwdControl(list(year=2:20, quant="catch", biol="ple", value=catch_target),
                           FCB = fcb)

## ---- fig.cap="A single Fishery with 1 Catch fishing on a single Biol"----
draw(flasher_ctrl, fisheryNames="Beam", catchNames="Plaice catch", biolNames="Plaice")

## ---- project1-----------------------------------------------------------
test <- fwd(object=biols1, fishery=flfs1, control=flasher_ctrl)

## ---- op-----------------------------------------------------------------
is(test)
names(test)

## ---- flag---------------------------------------------------------------
test[["flag"]]

## ---- plotex1, fig.cap="Summary results of projecting a single stock with a single fishery with a constant catch target.", echo=FALSE, fig.height=8----
par(mfrow=c(3,2))
plot_catch(1, test, flasher_ctrl@FCB, stock_name="Plaice")
plot_f(1, test, flasher_ctrl@FCB, stock_name="Plaice")
plot_effort(test[["fisheries"]])
plot_revenue(test[["fisheries"]])
plot_biomass(test[["biols"]][[1]],stock_name="Plaice")

## ---- set_up_1f2s--------------------------------------------------------
# Extract the beam trawl fishery from the FLFisheries loaded above
bt <- flfs[["bt"]]
# Create a new FLFisheries with the single FLFishery
flfs2 <- FLFisheries(bt=bt)
# It has only one FLFishery
length(flfs2)
# That FLFishery has 2 FLCatches
length(flfs2[["bt"]])
# The FLBiols has 2 FLBiol objects
length(biols)
# The names of the FLBiol objects are
names(biols)

## ----1f2s1---------------------------------------------------------------
fcb <- matrix(c(1,1,1,2,1,2), nrow=2, ncol=3, dimnames=list(1:2,c("F","C","B")))
plaice_catch_target <- 250000
flasher_ctrl <- fwdControl(
    list(year = 2:20, quant = "catch", biol = "ple", value = plaice_catch_target),
    FCB = fcb)

## ----fcb1----------------------------------------------------------------
flasher_ctrl@FCB

## ---- fig.cap="A single Fishery with 2 Catches, each fishing on a single Biol"----
draw(flasher_ctrl, fisheryNames="Beam trawl", catchNames=c("Ple catch", "Sol catch"), biolNames=c("Ple", "Sol"))

## ------------------------------------------------------------------------
test <- fwd(object=biols, fishery=flfs2, control=flasher_ctrl)

## ----plotex2, fig.cap="Summary results of projecting two stocks with a single fishery with a constant catch target on plaice.", echo=FALSE, fig.height=8----
par(mfrow=c(4,2))
plot_catch(biol_no=1, test, flasher_ctrl@FCB, stock_name="Plaice")
plot_catch(biol_no=2, test, flasher_ctrl@FCB, stock_name="Sole")
plot_f(biol_no=1, test, flasher_ctrl@FCB, stock_name="Plaice")
plot_f(biol_no=2, test, flasher_ctrl@FCB, stock_name="Sole")
plot_effort(test[["fisheries"]])
plot_revenue(test[["fisheries"]])
plot_biomass(test[["biols"]][[1]],stock_name="Plaice")
plot_biomass(test[["biols"]][[2]],stock_name="Sole")

## ----1f2s2---------------------------------------------------------------
plaice_catch_target <- 250000
sole_f_limit <- 0.2
flasher_ctrl <- fwdControl(
    list(year = 2:20, quant = "catch", biol = "ple", value = plaice_catch_target),
    list(year = 2:20, quant = "f", biol = "sol", max = sole_f_limit, minAge=2, maxAge=6),
    FCB = fcb)
test <- fwd(object=biols, fishery=flfs2, control=flasher_ctrl)

## ----plotex3, fig.cap="Summary results of projecting two stocks with a single fishery with a constant catch target on plaice and a maximum F limit on sole.", echo=FALSE, fig.height=8----
par(mfrow=c(4,2))
plot_catch(biol_no=1, test, flasher_ctrl@FCB, stock_name="Plaice")
plot_catch(biol_no=2, test, flasher_ctrl@FCB, stock_name="Sole")
plot_f(biol_no=1, test, flasher_ctrl@FCB, stock_name="Plaice")
plot_f(biol_no=2, test, flasher_ctrl@FCB, stock_name="Sole")
plot_effort(test[["fisheries"]])
plot_revenue(test[["fisheries"]])
plot_biomass(test[["biols"]][[1]],stock_name="Plaice")
plot_biomass(test[["biols"]][[2]],stock_name="Sole")

## ----set_up_2f1s---------------------------------------------------------
# Two fisheries on a single stock 
# Make the plaice fishery with a single FLCatch
bt3 <- FLFishery(solBT=flfs[["bt"]][["solBT"]], desc="")
bt3@effort[] <- 1
# Make the sole fishery with a single FLCatch
gn3 <- FLFishery(solGN=flfs[["gn"]][["solGN"]], desc="")
gn3@effort[] <- 1
# Make the FLFisheries
flfs3 <- FLFisheries(bt=bt3, gn=gn3)
# Make the FLBiols with a single FLBiol
biols3 <- FLBiols(sol=biols[["sol"]])

## ------------------------------------------------------------------------
# Names of the FLFishery objects
names(flfs3)
# Names of their FLCatch objects
names(flfs3[["bt"]])
names(flfs3[["gn"]])

## ------------------------------------------------------------------------
sole_bt_catch <- 10000
sole_gn_catch <- 5000
fcb <- matrix(c(1,2,1,1,1,1), nrow=2, ncol=3, dimnames=list(1:2,c("F","C","B")))
flasher_ctrl <- fwdControl(
    list(year=2:20, quant="catch", fishery="bt", catch="solBT", value=sole_bt_catch),
    list(year=2:20, quant="catch", fishery="gn", catch="solGN", value=sole_gn_catch),
    FCB = fcb)

## ----fcb3----------------------------------------------------------------
flasher_ctrl@FCB

## ---- fig.cap="Two Fisheries with one Catch each fishing on a single Biol"----
draw(flasher_ctrl, fisheryNames=names(flfs3), catchNames=unlist(lapply(flfs3, names)), biolNames=names(biols3))

## ------------------------------------------------------------------------
test <- fwd(object=biols3, fishery=flfs3, control=flasher_ctrl)

## ---- plotex4, fig.cap="Summary results of projecting one stock with two fisheries with a constant catch target on both fisheries", echo=FALSE, fig.height=8----
par(mfrow=c(3,2))
plot_catch(biol_no=1, test, flasher_ctrl@FCB, stock_name="Sole", legpos = "topleft")
plot_f(biol_no=1, test, flasher_ctrl@FCB, stock_name="Sole", legpos = "topleft")
plot_effort(test[["fisheries"]], legpos="topleft")
plot_revenue(test[["fisheries"]], legpos="bottomleft")
plot_biomass(test[["biols"]][[1]],stock_name="Sole")

## ------------------------------------------------------------------------
test[["flag"]]

## ---- 2f1s11-------------------------------------------------------------
sole_catch_target <- 12000
sole_bt_gn_catch_relative <- 2
flasher_ctrl <-fwdControl(list(year=2:20, quant="catch", biol="sol", value=sole_catch_target),
                           list(year=2:20, quant="catch",relYear=2:20, fishery="bt", catch="solBT", relFishery="gn", relCatch="solGN", value=sole_bt_gn_catch_relative),
                           FCB=fcb)
test <- fwd(object=biols3, fishery=flfs3, control=flasher_ctrl)

## ---- plotex5, fig.cap="Summary results of projecting one stock with two fisheries with a constant total catch target and relative catch", echo=FALSE, fig.height=8----
par(mfrow=c(3,2))
plot_catch(biol_no=1, test, flasher_ctrl@FCB, stock_name="Sole", legpos = "topleft")
plot_f(biol_no=1, test, flasher_ctrl@FCB, stock_name="Sole", legpos = "topleft")
plot_effort(test[["fisheries"]], legpos="topleft")
plot_revenue(test[["fisheries"]], legpos="right")
plot_biomass(test[["biols"]][[1]],stock_name="Sole")

## ---- nobs---------------------------------------------------------------
fcb <- matrix(c(1,1,1,1,2,2,2,1,1,2,2,2), byrow=TRUE, ncol=3, dimnames=list(1:4,c("F","C","B")))
sole_catch_target <- 12000
plaice_catch_target <- 5000
flasher_ctrl <- fwdControl(list(year=2:20, quant="catch",biol="sol",value=sole_catch_target),
                           list(year=2:20, quant="catch",biol="ple",value=plaice_catch_target),
           FCB=fcb)

## ------------------------------------------------------------------------
flasher_ctrl@FCB

## ---- fig.cap="Two Fisheries, each with 2 Catches fishing on two Biols"----
draw(flasher_ctrl, fisheryNames=names(flfs), catchNames=unlist(lapply(flfs, names)), biolNames=names(biols))

## ------------------------------------------------------------------------
test <- fwd(object=biols, fishery=flfs, control=flasher_ctrl)

## ------------------------------------------------------------------------
test[["flag"]]

## ------------------------------------------------------------------------
catch(test[["fisheries"]][["bt"]][["pleBT"]]) + catch(test[["fisheries"]][["gn"]][["pleGN"]])
catch(test[["fisheries"]][["bt"]][["solBT"]]) + catch(test[["fisheries"]][["gn"]][["solGN"]])

## ---- mf-----------------------------------------------------------------
sole_catch_target <- 12000
plaice_bt_gn_catch_relative <- 1.5

flasher_ctrl <- fwdControl(list(year=2:20, quant="catch",biol="sol",value=sole_catch_target),
           list(year=2:20, quant="catch", relYear=2:20, fishery="bt", catch="pleBT", relFishery="gn", relCatch="pleGN", value=plaice_bt_gn_catch_relative),
           FCB=fcb)

## ------------------------------------------------------------------------
test <- fwd(object=biols, fishery=flfs, control=flasher_ctrl)

## ---- plotex6, fig.cap="Summary results of projecting a mixed fishery with two stocks and two fleets with a constant total sole catch target and a relative plaice catch", echo=FALSE, fig.height=9----
par(mfrow=c(4,2))
plot_catch(biol_no=1, test, flasher_ctrl@FCB, stock_name="Plaice", legpos = "topleft")
plot_catch(biol_no=2, test, flasher_ctrl@FCB, stock_name="Sole", legpos = "topleft")
plot_f(biol_no=1, test, flasher_ctrl@FCB, stock_name="Plaice", legpos = "topleft")
plot_f(biol_no=2, test, flasher_ctrl@FCB, stock_name="Sole", legpos = "topleft")
plot_effort(test[["fisheries"]], legpos="topleft")
plot_revenue(test[["fisheries"]], legpos="bottomleft")
plot_biomass(test[["biols"]][[1]],stock_name="Plaice")
plot_biomass(test[["biols"]][[2]],stock_name="Sole")

## ---- complicated--------------------------------------------------------
sole_catch_target_initial <- 20000
sole_catch_decrease <- 0.9
plaice_bt_gn_catch_relative <- 1.5

flasher_ctrl <- fwdControl(
    list(year=2, quant="catch", biol="sol", value=sole_catch_target_initial),
    list(year=3:20, quant="catch", relYear=2:19, biol="sol", relBiol="sol", value=sole_catch_decrease),
    list(year=2:20, quant="catch", relYear=2:20, fishery="bt", catch="pleBT", relFishery="gn", relCatch="pleGN",value=plaice_bt_gn_catch_relative),
    FCB=fcb)
test <- fwd(object=biols, fishery=flfs, control=flasher_ctrl)

## ---- plotex7, fig.cap="Summary results of projecting a mixed fishery with two stocks and two fleets with a decreasing total sole catch target and a relative plaice catch", echo=FALSE, fig.height=9----
par(mfrow=c(4,2))
plot_catch(biol_no=1, test, flasher_ctrl@FCB, stock_name="Plaice", legpos = "topleft")
plot_catch(biol_no=2, test, flasher_ctrl@FCB, stock_name="Sole", legpos = "topleft")
plot_f(biol_no=1, test, flasher_ctrl@FCB, stock_name="Plaice", legpos = "topleft")
plot_f(biol_no=2, test, flasher_ctrl@FCB, stock_name="Sole", legpos = "topleft")
plot_effort(test[["fisheries"]], legpos="topleft")
plot_revenue(test[["fisheries"]], legpos="bottomleft")
plot_biomass(test[["biols"]][[1]],stock_name="Plaice")
plot_biomass(test[["biols"]][[2]],stock_name="Sole")

## ---- revtest------------------------------------------------------------
sole_catch_target_initial <- 20000
sole_catch_decrease <- 0.9
plaice_bt_gn_catch_relative <- 1.5
bt_min_revenue <- 150000
gn_min_revenue <- 100000

flasher_ctrl <- fwdControl(
    list(year=2, quant="catch", biol="sol", value=sole_catch_target_initial),
    list(year=3:20, quant="catch", relYear=2:19, biol="sol", relBiol="sol", value=sole_catch_decrease),
    list(year=2:20, quant="catch", relYear=2:20, fishery="bt", catch="pleBT", relFishery="gn", relCatch="pleGN",value=plaice_bt_gn_catch_relative),
    list(year=2:20, quant="revenue", fishery="bt", min=bt_min_revenue),
    list(year=2:20, quant="revenue", fishery="gn", min=gn_min_revenue),
    FCB=fcb)
test <- fwd(object=biols, fishery=flfs, control=flasher_ctrl)

## ---- plotex8, fig.cap="Summary results of projecting a mixed fishery with two stocks and two fleets with a decreasing sole catch target and a relative plaice catch. Minimum limits to the revenues of both fleets are also included.", echo=FALSE, fig.height=9----
par(mfrow=c(4,2))
plot_catch(biol_no=1, test, flasher_ctrl@FCB, stock_name="Plaice", legpos = "topleft")
plot_catch(biol_no=2, test, flasher_ctrl@FCB, stock_name="Sole", legpos = "topleft")
plot_f(biol_no=1, test, flasher_ctrl@FCB, stock_name="Plaice", legpos = "topleft")
plot_f(biol_no=2, test, flasher_ctrl@FCB, stock_name="Sole", legpos = "topleft")
plot_effort(test[["fisheries"]], legpos="topleft")
plot_revenue(test[["fisheries"]], legpos="bottomleft")
plot_biomass(test[["biols"]][[1]],stock_name="Plaice")
plot_biomass(test[["biols"]][[2]],stock_name="Sole")

## ------------------------------------------------------------------------
# Extract the beam trawl fishery
bt <- flfs["bt"]

## ------------------------------------------------------------------------
joint_tac <- 200000
years <- 2:5
fcb <- matrix(c(1,1,1,2,1,2), nrow=2, ncol=3, dimnames=list(1:2,c("F","C","B")))
ctrl <- fwdControl(list(year=years, quant="catch", value=joint_tac, biol=G("ple", "sol")),
           FCB=fcb)

## ------------------------------------------------------------------------
ctrl

## ------------------------------------------------------------------------
test <- fwd(object=biols, fishery=bt, control=ctrl)

## ------------------------------------------------------------------------
# plaice catch
catch_ple <- catch(test[["fisheries"]][["bt"]][["pleBT"]])[,ac(years)]
# sole catch
catch_sol <- catch(test[["fisheries"]][["bt"]][["solBT"]])[,ac(years)]
catch_sol + catch_ple

## ------------------------------------------------------------------------
catch_sol  / (catch_sol + catch_ple)
catch_ple  / (catch_sol + catch_ple)

## ------------------------------------------------------------------------
rel_sol_catch <- 0.8
total_ple_catch <- 200000 
years <- 2:5
fcb <- matrix(c(1,1,1,1,2,2,2,1,1,2,2,2), byrow=TRUE, ncol=3, dimnames=list(1:4,c("F","C","B")))
ctrl <- fwdControl(
        list(year=years, quant="catch", value=total_ple_catch, fishery=G("bt","gn"), catch=G("pleBT", "pleGN")),
        list(year=years, quant="catch", value = 0.8, fishery="bt", catch="solBT", relYear=years, relFishery="gn", relCatch="solGN"),
        FCB=fcb)

## ------------------------------------------------------------------------
ctrl

## ------------------------------------------------------------------------
draw(ctrl, fisheryNames=names(flfs), catchNames=unlist(lapply(flfs, names)), biolNames=names(biols))

## ------------------------------------------------------------------------
test <- fwd(object=biols, fishery=flfs, control=ctrl)

## ------------------------------------------------------------------------
catch_ple_bt <- catch(test[["fisheries"]][["bt"]][["pleBT"]])[,ac(years)]
catch_ple_gn <- catch(test[["fisheries"]][["gn"]][["pleGN"]])[,ac(years)]
catch_sol_bt <- catch(test[["fisheries"]][["bt"]][["solBT"]])[,ac(years)]
catch_sol_gn <- catch(test[["fisheries"]][["gn"]][["solGN"]])[,ac(years)]
catch_ple_bt + catch_ple_gn
catch_sol_bt / catch_sol_gn

## ------------------------------------------------------------------------
effort(test[["fisheries"]][["bt"]])[,ac(years)]
effort(test[["fisheries"]][["gn"]])[,ac(years)]

## ------------------------------------------------------------------------
total_max_effort <- 2
years <- 2:5
# targets + constraints (max effort, min biomass)
ctrl <- fwdControl(
  list(year=years, quant="catch", value=total_ple_catch,
    fishery=G("bt","gn"), catch=G("pleBT", "pleGN")),
  list(year=years, quant="catch", value = 0.8, fishery="bt",
    catch="solBT", relYear=years, relFishery="gn", relCatch="solGN"),
  list(year=years, quant="effort", max=total_max_effort, fishery=G("bt","gn")),
  list(year=years, quant="biomass_end", min=1.0, biol="ple"),
        FCB=fcb)
ctrl

## ------------------------------------------------------------------------
test <- fwd(object=biols, fishery=flfs, control=ctrl)

## ------------------------------------------------------------------------
# Check that all the target pairs in each year solved
test[["flag"]]
# Look at effort
effort_bt <- (test[["fisheries"]][["bt"]]@effort)[,ac(years)]
effort_gn <- (test[["fisheries"]][["gn"]]@effort)[,ac(years)]
effort_bt + effort_gn

## ------------------------------------------------------------------------
catch_ple_bt <- catch(test[["fisheries"]][["bt"]][["pleBT"]])[,ac(years)]
catch_ple_gn <- catch(test[["fisheries"]][["gn"]][["pleGN"]])[,ac(years)]
catch_sol_bt <- catch(test[["fisheries"]][["bt"]][["solBT"]])[,ac(years)]
catch_sol_gn <- catch(test[["fisheries"]][["gn"]][["solGN"]])[,ac(years)]
catch_ple_bt + catch_ple_gn
catch_sol_bt / catch_sol_gn

