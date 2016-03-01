# STF.R
# Projecting_with_FLR
# 1. Short Term Forecasts

# Copyright 2014 JRC Distributed under the GPL 2 or later
# Maintainer: FLR team
# Created: 18/08/2014

#---------------------------------------------------------------
# Basics
#---------------------------------------------------------------

# Load the libraries
library(FLCore)
library(FLAssess)
library(FLash)
library(ggplotFL)

# As ever, load the ple4 data
data(ple4)

#---------------------------------------------------------------
# Introduction to projections
#---------------------------------------------------------------

# There are three necessary ingredients when projecting in FLR
#     stock object - that you will forecast and about which you have made some assumptions about what will happen in the future
#     a stock-recruitment relationship (even with STF)
#     projection control - specify what targets and when to hit them

# In FLR, there is a method fwd()
# This takes these three ingredients as arguments
#     FLStock 
#     FLSR
#     fwdControl()

#---------------------------------------------------------------
# Setting up the stock - assumptions 
#---------------------------------------------------------------

summary(ple4)
# Our ple4 stock goes up to 2008
# We are running a STF forecast - 3 years
# In this example we want to make a 3 year projection
# So we need to extend the stock by 3 years

# The projection will predict abundances in the future
# But what about all the other stock information?
# What will the future:
#   stock weights
#   maturity
#   natural mortality
#   etc.
# look like?

# You need to decide on your assumptions because they will affect the outcome of the projection

# We could use window() or trim() but that makes all future data NA
# For example the future stock weights
stock.wt(window(ple4, end= 2011))


# Instead we use the stf() function (short term forecast)
# This has several options that allow you to control the assumptions about the future
?stf
# These assumptions specify how many years you want to average over to set future values
# For example, wts.nyears is the number of years over which to calculate the *.wt, *.spwn, mat and m slots
# By default this is 3 years
# This is a fairly standard assumption for STFs
# i.e. the future mean weights at age will be the same as the mean of the last 3 years
# This is what we mean by:
# *What is going to happen in the next few years is probably similar to what happened in the last few years* 

# Keep things simple so do a 3 year forecast
ple4_stf <- stf(ple4, nyears = 3)
# Now the stock goes up to 2011
summary(ple4_stf)
# You can see the future (2009:2011) weights are the mean of the last 3 years (2006:2008)
stock.wt(ple4_stf)
apply(stock.wt(ple4)[,ac(2006:2008)], c(1,3:6), mean)

# Same for maturity etc.
mat(ple4_stf)
apply(mat(ple4)[,ac(2006:2008)], c(1,3:6), mean)

# Another thing important to notice
# The future fishing mortality has also been set (mean of the last 3 years, by default).
harvest(ple4_stf)
apply(harvest(ple4)[,ac(2006:2008)], c(1,3:6), mean)
# In the projections, the harvest slot is used as the selectivity 
# For example you can rescale by the maximum F in each year to look at the future selectivity ogive
sel <- sweep(harvest(ple4_stf), 2:6, apply(harvest(ple4_stf), 2:6, max), "/")
ggplot(sel[,ac(2001:2011)]) + geom_line(aes(x=age, y=data)) + facet_wrap(~year)
ggplot(sel[,ac(2001:2011)]) + geom_line(aes(x=age, y=data, colour = as.factor(year)))

# The final thing to notice is what happened to abundances
# We have no stock abundances - that is the whole point of the projection!
stock.n(ple4_stf)
# We also have no catch abundances
catch.n(ple4_stf)

# But there is something in the landings.n and discards.n slots
landings.n(ple4_stf)
discards.n(ple4_stf)
# The future values (2009 to 2011) are not abundances but ratios
# i.e. what proportion of catch numbers are landed and what are discarded
landings.n(ple4_stf)[,ac(2009:2011)] + discards.n(ple4_stf)[,ac(2009:2011)]
# Int the projection Catch numbers are calculated from F and stock abundance
# These are then split into landings and discards using these ratios

#---------------------------------------------------------------
# The stock-recruitment relationship (SRR)
#---------------------------------------------------------------

# A STF does not use a SRR 
# Instead it assumes that recruitment in the future is the mean of the last X years (normally 3)
# However, we still need to pass the projection a SRR that contains the mean value
# Here we use the mean of the last 3 years

mean_rec <- mean(rec(ple4)[,ac(2006:2008)])

# set up an FLSR object with a geometric mean model
ple4_sr <- as.FLSR(ple4, model="geomean")
# We're not going to fit this model - we're just going to fix the mean value
params(ple4_sr)
params(ple4_sr)['a',] <- mean_rec


#---------------------------------------------------------------
# The control object
#---------------------------------------------------------------

# The final thing we need to set up is the control object
# This tells the projection what to do, i.e. what level of fishing mortality to use

# A standard scenario for a STF is the 'status quo' scenario
# This assumes that the future mean fishing mortality will be the same as the mean of the last X years (X depends on the stock)
# We will set up this scenario as a simple example using a 4 year mean

# Calculate historic Fbar level
# fbar range set in 'range' slot
range(ple4)
fbar(ple4)
ggplot(fbar(ple4), aes(x=year,y=data)) + geom_line()
fbar_status_quo <- mean(fbar(ple4)[,as.character(2005:2008)])

# Now we introduce the control object: fwdControl()
# This takes 1 argument - a data.frame that sets:
#   The year the Fbar target is to be hit 
#   The quantity (or type) of the target (we are using fbar but can also set catch etc. see MTF)
#   The value of the target
#   Some other things that we will ignore for now
# Make the data.frame
ctrl_target <- data.frame(year = 2009:2011,
			  quantity = "f",
			  val = fbar_status_quo)
ctrl_target
# Set the control object - year, quantity and value for the moment
ctrl_f <- fwdControl(ctrl_target)

# Quick tour of the control object
ctrl_f
# We see that we have what looks like our ctrl_target, but now it has two more columns (min and max)
# Also there is another table underneath - ignore this for now - this is for uncertainty which we ignore in STF

#---------------------------------------------------------------
# Running the STF
#---------------------------------------------------------------

# Here we run a simple STF with 'status quo' future fishing mortality
# Remember we had to make assumptions about the future (weights, selection pattern, discard ratio etc.)

# Run fwd() with our three ingredients: the stock, the control object, the SRR
ple4_sq <- fwd(ple4_stf, ctrl = ctrl_f, sr = ple4_sr)
# It returns an updated FLStock object

# What just happened?
summary(ple4_sq)
# fbar in the future is what we wanted in the control object
fbar(ple4_sq)
fbar_status_quo
# And we have predicted abundances
stock.n(ple4_sq)
# So we can calculate predicted abundances (which depends on future assumptions of maturity and stock weight)
ssb(ple4_sq)
# Recruitment in the future years are what we wanted
rec(ple4_sq)
mean_rec

# The future harvest slot is now different to the one we set up
harvest(ple4_stf)[,ac(2009:2011)]
harvest(ple4_sq)[,ac(2009:2011)]
# But the selection pattern is the same - the relative Fs
harvest(ple4_stf)[,ac(2009:2011)] / harvest(ple4_sq)[,ac(2009:2011)]
# F at age has been multiplied by the same value to give us the target Fbar value

# The catch numbers come from the predicted abundance and harvest rates
# Using Baranov
((harvest(ple4_sq) / z(ple4_sq)) * (1 - exp(-z(ple4_sq))) * stock.n(ple4_sq))[,ac(2009:2011)]
catch.n(ple4_sq)[,ac(2009:2011)]

# The catch numbers are then split into landings and discards using the ratios that stf() gave us
# Also landings and discards numbers
landings.n(ple4_sq)
discards.n(ple4_sq)
# These are based on the predicted catch numbers and the discard ratio that stf() gave us
landings.n(ple4_stf)[,ac(2009:2011)]
landings.n(ple4_stf)[,ac(2009:2011)] * catch.n(ple4_sq)[,ac(2009:2011)]
landings.n(ple4_sq)[,ac(2009:2011)]

# We can see the projection here
plot(window(ple4_sq, start = 1991, end = 2011))

#---------------------------------------------------------------
# Short term forecast with many F scenarios
#---------------------------------------------------------------

# Typically when running STF you explore several different future F scenarios

# We are going to run several F scenarios for the STF
# The scenarios are based on 'F status quo', which we calculated above as the mean F of the last X years
# For a 3 year STF the F pattern is:
# year 1: fbar_status_quo
# year 2: fbar_status_quo * fbar_multiplier
# year 3: fbar_status_quo * fbar_multiplier
# The fbar_multiplier is the same for years 2 and 3

# We are going to run several STFs with different values for the fbar_multiplier
# The fbar_multiplier ranges from 0.1 to 2 by 0.1
fbar_multiplier <- seq(from = 0, to = 2, by = 0.1)

# We are going to build a data.frame that builds these scenarios
# Each column in the dataframe is a year
# Each row is a scenario
# Set up the fbar scenarios - note that if you project for more than 3 years you will need to add more columns / years to the matrix
fbar_scenarios <- cbind(rep(fbar_status_quo,length(fbar_multiplier)),
                        fbar_multiplier*fbar_status_quo,
                        fbar_multiplier*fbar_status_quo)

# Another scenario we are interested in is F0.1
# We can calculate this using FLBRP (or maybe you already have a value)
library(FLBRP)
f01 <- c(refpts(brp(FLBRP(ple4)))["f0.1","harvest"])
# Add some names
colnames(fbar_scenarios) <- c("2009","2010","2011")
rownames(fbar_scenarios) <- c(fbar_multiplier, "f01")
fbar_scenarios

# Add the F0.1 scenario as a final scenario
fbar_scenarios <- rbind(fbar_scenarios, c(fbar_status_quo,f01,f01))

# There are various results we want to extract from the STF
# Like predicted Catch, SSB and the relative change in these
# The following is what we calculate in the STECF Med. WG
# Make an empty matrix in which to store the results
stf_results <- matrix(NA,nrow = nrow(fbar_scenarios),ncol = 10)
# Set some column names
final_year <- 2008
colnames(stf_results) <- c('Ffactor',
    'Fbar',
    paste('Catch',final_year,sep="_"),
    paste('Catch',final_year+1,sep="_"), 
    paste('Catch',final_year+2,sep="_"),
    paste('Catch',final_year+3,sep="_"),
    paste('SSB',final_year+2,sep="_"),
    paste('SSB',final_year+3,sep="_"),
    paste('Change_SSB_',final_year+2,'-',final_year+3,'(%)',sep=""),
    paste('Change_Catch_',final_year,'-',final_year+2,'(%)',sep=""))
stf_results

# Store the resulting FLStock each time
stk_stf <- FLStocks()
# Loop over the scenarios (each row in the fbar_scenarios table)
for (scenario in 1:nrow(fbar_scenarios)) {
    cat("Scenario: ", scenario, "\n")
    # Make a target object withe F values for that scenario
    ctrl_target <- data.frame(year = 2009:2011,
                              quantity = "f",
                              val = fbar_scenarios[scenario,])
    # Set the control object - year, quantity and value for the moment
    ctrl_f <- fwdControl(ctrl_target)
    # ctrl_target
    # Run the forward projection. We include an additional argument, maxF.
    # By default the value of maxF is 2.0
    # Here we increase it to 10.0 so that F is not limited
    ple4_fwd <- fwd(ple4_stf, ctrl = ctrl_f, sr = ple4_sr)#, maxF = 10.0)
    ## Check it has worked - uncomment out to check scenario by scenario
    # plot(ple4_fwd[,ac(2001:2011)])
    # Store the result - if you want to, comment out if unnecessary
    stk_stf[[as.character(scenario)]] <- ple4_fwd

    # Fill results table
    stf_results[scenario,1] <- fbar_scenarios[scenario,2] / fbar_scenarios[scenario,1] # fbar status quo ratio
    stf_results[scenario,2] <- fbar(ple4_fwd)[,ac(2011)] # final stf year
    stf_results[scenario,3] <- catch(ple4_fwd)[,ac(final_year)] # last 'true' year
    stf_results[scenario,4] <- catch(ple4_fwd)[,ac(final_year+1)] # 1st stf year
    stf_results[scenario,5] <- catch(ple4_fwd)[,ac(final_year+2)] # 2nd stf year
    stf_results[scenario,6] <- catch(ple4_fwd)[,ac(final_year+3)] # final stf year
    stf_results[scenario,7] <- ssb(ple4_fwd)[,ac(final_year+2)] # 2nd stf year
    stf_results[scenario,8] <- ssb(ple4_fwd)[,ac(final_year+3)] # final stf year
    # Change in SSB
    stf_results[scenario,9] <- (ssb(ple4_fwd)[,ac(final_year+3)]-ssb(ple4_fwd)[,ac(final_year+2)])/ssb(ple4_fwd)[,ac(final_year+2)]*100 # change in ssb in last two stf years
    stf_results[scenario,10] <- (catch(ple4_fwd)[,ac(final_year+2)]-catch(ple4_fwd)[,ac(final_year)])/catch(ple4_fwd)[,ac(final_year)]*100 # change in catch from true year, to 2nd to last stf year
}
# Give the FLStocks object some names
names(stk_stf) <- rownames(fbar_scenarios)

# Plotting
plot(window(stk_stf, start=2001, end=final_year+3))

# Look at the table of results
stf_results
# export this if necessary
#write.csv(stf_results, file="stf_results.csv")



