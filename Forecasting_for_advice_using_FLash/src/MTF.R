# MTF.R
# Projecting_with_FLR
# 2. Medium Term Forecasts

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
library(FLBRP)

#---------------------------------------------------------------
# Introduction to Medium Term Forecasts
#---------------------------------------------------------------

# Running a MTF is similar to the STF in that we need:
# 1. An FLStock object set up for the future (assumptions)
# 2. A stock-recruiment relationship
# 3. A projection control object

# However, there are some significant differences between an MTF and an STF:
# 1. An MTF is normally run for 5 to 10 years
# 2. An MTF can use different target types (e.g. setting catch targets), not just setting future F targets
# 3. A 'real' SRR should be used (the assumption of mean recruitment is not a good one for more than 3 years)
# 4. We can include uncertainty in the recruitment and target values

# In this session we will cover all these.
# We will build a 10 year projection. introduce a range of target types, include minimum and maximum target values, relative target values, and uncertainty.


#---------------------------------------------------------------
# Step 1. Preparing the future stock
#---------------------------------------------------------------

# As ever, load the ple4 data
data(ple4)

# We again use stf() to set up a future stock
# Remember all of the assumptions that are being made about selectivity, weights, etc
# We may want to change some of these but for the moment we will use the defaults.

# Set up a 10 year MTF
ple4_mtf <- stf(ple4, nyears = 10)
# Now the stock goes up to 2018
summary(ple4_mtf)

#---------------------------------------------------------------
# Step 2 - the stock-recruitment relationship
#---------------------------------------------------------------

# Here we use a Beverton-Holt model
ple4_sr <- fmle(as.FLSR(ple4, model="bevholt"))
plot(ple4_sr)

#---------------------------------------------------------------
# Example 1: F targets
#---------------------------------------------------------------

# We saw in the STF session how to set an F target
# Here is some quick revision
# We will set the future F at F status quo (again) and we assume that F status quo is the mean of the last 4 years
f_status_quo <- mean(fbar(ple4)[,as.character(2005:2008)])

# Make the data.frame
ctrl_target <- data.frame(year = 2009:2018,
			  quantity = "f",
			  val = f_status_quo)

# Set the control object - year, quantity and value for the moment
ctrl_f <- fwdControl(ctrl_target)

ctrl_f
# We see that we have what looks like our ctrl_target, but now it has two more columns (min and max)
# Also there is another table underneath - ignore this for now

# Run fwd() with our three ingredients
ple4_f_sq <- fwd(ple4_mtf, ctrl = ctrl_f, sr = ple4_sr)

# What just happened?
# Future Fs are OK
fbar(ple4_f_sq)
# What about recruitment - remember we are now using a Beverton-Holt model
rec(ple4_f_sq)
ssb(ple4_f_sq)
# Recruitment looks flat but that's because the fitted model looks flat
plot(window(ple4_f_sq, start = 2001, end = 2018))

#---------------------------------------------------------------
# Example 2:  Catch target
#---------------------------------------------------------------

# Here we set catch as the target
# We can use to explore the consequences of different TAC strategies
# At the moment, catch is empty
catch(ple4_mtf)

# Plan is to reduce the catch by 10% each year for 10 years
future_catch <- c(catch(ple4)[,"2008"]) * 0.9^(1:10)
future_catch

ctrl_catch <- fwdControl(
	data.frame(
		year=2009:2018,
		quantity = "catch",
		val=future_catch))
ctrl_catch
ple4_catch <- fwd(ple4_mtf, ctrl_catch, sr = ple4_sr)
catch(ple4_catch)
plot(window(ple4_catch, start = 2001, end = 2018))


#---------------------------------------------------------------
# Example 3:  SSB target
#---------------------------------------------------------------

# Here we set SSB as the target
# We have to consider the timing here
# In an FLStock, abundances are at the beginning of the year
# So if you call:
stock(ple4)
# You get the stock at the beginning of each year.
# However, the stock at the start of the year is the result of fishing in the previous year.
# If you set an abundance based target, you are really finding the F that will give you that target in the following year.
# So you have to be careful with the years in the control object.
# This is best illustrated with an example.

# If we want to hit an SSB target in 2009:2018, we actually set it for 2008:2017 as these are the years that F will be set to hit the target
# We want the future SSB to be high (we could have used FLBRP to come up with a suitable value, e.g. Bmsy but here we just pick a value)
future_ssb <- 300000
ctrl_ssb <- fwdControl(data.frame(year=2008:2017, quantity = "ssb", val=future_ssb))
ctrl_ssb
ple4_ssb <- fwd(ple4_mtf, ctrl_ssb, sr = ple4_sr)
 
# Remember - 
# the target year in the control object is the year that F is set
# SSB in year Y depends on F in Y-1

# SSB has been hit, but not until 2009 even though we set the target for 2008
ssb(ple4_ssb)
# But this has required a large decrease in F and Catch in 2008
fbar(ple4_ssb)
catch(ple4_ssb)
plot(window(ple4_ssb, start = 1991, end = 2017))

#---------------------------------------------------------------
# Example 4:  Relative catch target
#---------------------------------------------------------------

# The examples above have dealt with ABSOLUTE target values
# We now introduce the idea of RELATIVE values
# This allows us to set the target value RELATIVE to the value in another year

# Here we set catches relative to the catch value in another year

ctrl_rel_catch <- fwdControl(
	data.frame(year = 2009:2018,
		   quantity = "catch",
		   val = 0.9,
		   rel.year = 2008:2017))
# Note the introduction of the rel.year column
# This means that we want the value in 2009 to be 0.9 * value in 2008 etc
ctrl_rel_catch
# An extra column has appeared - rel.year, the relative year
# Put it into fwd()
ple4_rel_catch <- fwd(ple4_mtf, ctrl_rel_catch, sr = ple4_sr)
catch(ple4_rel_catch)
catch(ple4_rel_catch)[,ac(2009:2018)] / catch(ple4_rel_catch)[,ac(2008:2017)]
plot(window(ple4_rel_catch, start = 2001, end = 2018))

# This is the same as the Catch Example 2 above but without using absolute values

#---------------------------------------------------------------
# Example 5: Minimum and Maximum targets
#---------------------------------------------------------------

# In this Example we introduce 2 new things:
# Multiple targets
# Targets with BOUNDS

# Here we set an F target so that F in 2015 = F0.1 (a proxy for Fmsy)
# But we don't want the Catch to fall below a minimum level
# Let's see if it is possible
# So we can set a MINIMUM value to the catch. 

# Calculate the target F0.1
f01 <- c(refpts(brp(FLBRP(ple4)))["f0.1","harvest"])
# Set up the F target sequence
current_fbar <- c(fbar(ple4)[,"2008"])
# Set up F sequence that decreases from current fbar in 2008 to f01 in 2015, then f01 until 2018
f_target <- c(seq(from = current_fbar, to = f01, length = 8)[-1], rep(f01, 3))
f_target

# We'll set our minimum catch to be the mean catch of the last 3 years
min_catch <- mean(catch(ple4_mtf)[,as.character(2006:2008)])

# To make the control object we can bind together two data.frames - 1 for each target type
# Note that we include min = NA as a column - this is necessary to bind it to the catch data.frame
ctrl_target <- rbind(
    f_df <- data.frame(
        year = 2009:2018,
        quantity = "f",
        val = f_target,
        min = NA),
    catch_df <- data.frame(
        year = 2009:2018,
        quantity = "catch",
        val = NA,
        min = min_catch)
)
ctrl_target
# This looks sort of right but we need to order the data.frame so that:
# the years are sequential
# And within each year, minimum / maximum targets come after the absolute one
ctrl_target <- ctrl_target[order(ctrl_target$year),]
ctrl_target

# Make the control object
ctrl_min_catch <- fwdControl(ctrl_target)
               
# What did we create?
ctrl_min_catch
# Again, ignore the second table for the moment

# And project
ple4_min_catch <- fwd(ple4_mtf, ctrl_min_catch, sr = ple4_sr)
# Catches hit the minimum bound in all projection years
catch(ple4_min_catch) 
# So the fbar target is never hit (though Fbar does decrease)
fbar(ple4_min_catch) 
# We can see this in the plot
plot(window(ple4_min_catch, start = 2001, end = 2018))
				       
#---------------------------------------------------------------
# Example 6 - Relative targets and bounds
#---------------------------------------------------------------

# Here we use a combination of RELATIVE targets and BOUNDS

# This kind of approach can be used to model a recovery plan
# For example, we want to decrease F to F0.1 by 2015 (absolute target value)
# But catches cannot change by more than 15% each year (relative bound)

# This requires careful setting up of the control object
# Again, we'll bind two data.frames

# We can use the F values from the previous example
# But set relative catch bounds
rel_catch_bound <- 0.15

# To make the control object we can bind together two data.frames - 1 for each target type
# Note that we include min = NA as a column - this is necessary to bind it to the catch data.frame
ctrl_target <- rbind(
    f_df <- data.frame(
        year = 2009:2018,
        rel.year = NA,
        quantity = "f",
        val = f_target,
        max = NA,
        min = NA),
    catch_df <- data.frame(
        year = 2009:2018,
        rel.year = 2008:2017,
        quantity = "catch",
        val = NA,
        max = 1 + rel_catch_bound,
        min = 1 - rel_catch_bound)
)

# Remember to reorder
ctrl_target <- ctrl_target[order(ctrl_target$year),]
ctrl_target
# Make the control object
ctrl_rel_min_catch <- fwdControl(ctrl_target)
ctrl_rel_min_catch

# Project
recovery<-fwd(ple4_mtf, ctrl=ctrl_rel_min_catch, sr=ple4_sr)

# What happened?
plot(window(recovery, start = 2001, end = 2018))

# The bounds have only been an issue in 2015
catch(recovery)[,ac(2008:2018)]
catch(recovery)[,ac(2009:2018)] / catch(recovery)[,ac(2008:2017)]


#---------------------------------------------------------------
# Interlude
#---------------------------------------------------------------

# So far we have looked at combinations of:
#     absolute target values,
#     relative target values,
#     bounds on targets, and
#     mixed target types.

# But all of the projections have been deterministic.
# That is they all have only one iteration.
# Now, we are going start looking at projecting with multiple iterations
# This is important because it can help you understand the impact of
# uncertainty (e.g. in the stock-recruitment relationship)

# fwd() is happy to work over iterations.
# It treats each iteration separately. 
# 'All' you need to do is set the arguments correctly 
# There are two main ways of introducing iterations into fwd():
	# 1. By passing in residuals to the stock-recruitment function (as another argument to fwd())
	# 2. Through the control object (by setting target values as multiple values)
# You can actually use both of these methods at the same time.
# As you can probably imagine, this can quickly become very complicated
# so we'll just do some simple examples

#---------------------------------------------------------------
# Preparation for projecting with iterations
#---------------------------------------------------------------

# You need a stock with multiple iterations.
# If you are using the output of a stock assessment method such as a4a  then you may have one already.
# Here we use the 'propagate' method to give us multiple iterations.

# Choose the number of iterations 
niters <- 1000
# We'll use the ten year projection as before (remember that we probably should change the assumptions)
ple4_mtf <- stf(ple4, nyears = 10)
ple4_mtf <- propagate(ple4_mtf, niters)
# You can see that the 6th dimension, iterations, now has length 1000
summary(ple4_mtf)

#---------------------------------------------------------------
# Example 7: Stochastic recruitment
#---------------------------------------------------------------

# Stochastic recruitment
# There are two arguments to fwd() that we haven't used yet:
# sr.residuals and sr.residuals.mult
# These are used for specifying the recruitment residuals (sr.residuals)
# and whether these residuals are multiplicative (sr.residuals.mult=TRUE)  or additive (FALSE)

# In this example we'll use multiplicative residuals.
# i.e. the 'true' recruitment in projection = recruitment predicted by FLSR * residuals
# Residuals are passed in as an FLQuant with years and iterations.
# First we have to set up an FLQuant of recruitment residuals
multi_rec_residuals <- FLQuant(NA, dimnames = list(year=2009:2018, iter=1:niters))

# We're going to use residuals from the stock-recruitment relationship we fitted at the beginning.
# We can access these using:
residuals(ple4_sr)
# These residuals are on a log scale i.e.
# log_residuals = log(true_recruitment) - log(predicted_recruitment)
# To use these log residuals multiplicatively we need to transform them with exp()

# We want to fill up our multi_rec_residuals FLQuant by randomly sampling from these log residuals
# We can do this with the sample() function
# We want to sample with replacement (i.e. if a residual is chosen, it gets put back in the pool to be chosen again)

# First we get generate the samples of the years (indices of the residuals we will pick)
sample_years <- sample(dimnames(residuals(ple4_sr))$year, niters * 10, replace = TRUE)
# Fill up the FLQuant we made earlier
multi_rec_residuals[] <- exp(residuals(ple4_sr)[,sample_years])
# What have we got?
multi_rec_residuals
# What do those brackets mean?
# It's a way of summarising the iterations - we have 1000 iterations but don't want to see all of them - just a summary

# We now have the recruitment residiuals
# We still need a control object
# We'll use one we made earlier with decreasing catch  
ctrl_catch

# Call fwd() as usual, only now we have sr.residuals and sr.residuals.mult
ple4_catch_rec <- fwd(ple4_mtf, ctrl = ctrl_catch, sr = ple4_sr, sr.residuals = multi_rec_residuals, sr.residuals.mult = TRUE)
# What just happened?
rec(ple4_catch_rec)[,ac(2008:2018)]
fbar(ple4_catch_rec)[,ac(2008:2018)]
ssb(ple4_catch_rec)[,ac(2008:2018)]
plot(window(ple4_catch_rec, start = 2001, end = 2018))
# A projection with random recruitment

#---------------------------------------------------------------
# Example 8: stochastic target values
#---------------------------------------------------------------

# In this example we introduce uncertainty by including uncertainty in our target values
# This will be a simple example with catch as the target
# Except now, catch will be stochastic

# We will use the ctrl_catch object from above
ctrl_catch
# Take a look at what else is in the control object
slotNames(ctrl_catch)
# The iterations of the target value are set in the trgtArray slot
# This is the other table that gets printed when you call the control object
ctrl_catch@trgtArray
# What is this?
class(ctrl_catch@trgtArray)
dim(ctrl_catch@trgtArray)
# It's a 3D array (target no x value x iteration)
# And it's in here that we set the stochastic catch
# Each row of trgtArray corresponds to a row in the data.frame we passed in

# Here we set 10 targets (for 10 years), so the first dimension of trgtArray has length 10.
# The second dimension always has length 3
# The third dimension is where the iterations are stored.
# This is currently length 1. We have 1000 iterations and therefore we need to expand the array along the iter dimension so it can store the 1000 iterations we want
# Simplest way is just to make a new array with the right dimensions
# Note that we need to put in dimnames and the iter dimension must be named
new_trgtArray <- array(NA, dim=c(10,3,niters), dimnames = list(1:10, c("min","val","max"),iter=1:niters))
dim(new_trgtArray)
# Now we can fill it up with new data

# We need to generate random catch target data.
# This could be from reference points or something.
# In this example we make it very simple, by using a fixed standard deviation
# We assume a lognormal distribution and multiply it by the values in the target array
future_catch_iters <- ctrl_catch@trgtArray[,"val",] * rlnorm(10 * niters, meanlog = 0, sdlog=0.2)
# Fill up our target array
# Just fill up the 'val' column (you can also use min and max for stochastic bounds)
new_trgtArray[,"val",] <- future_catch_iters
# put this into the control object
ctrl_catch@trgtArray <- new_trgtArray
ctrl_catch

# And project
ple4_catch_iters <- fwd(ple4_mtf, ctrl_catch, sr = ple4_sr)
# What happened?
plot(window(ple4_catch_iters, start = 2001, end = 2018))
catch(ple4_catch_iters)[,ac(2008:2018)]

# What is going on with recruitment?
rec(ple4_catch_iters)[,ac(2008:2018)]
# Remember that here recruitment is not being driven by random residuals
# It is only be driven by SSB
# The recruitment in year Y is a result of the SSB in year Y-1
# The SSB in year Y-1 is a result of the Catch in year Y-2
# So if Catch is stochastic in 2009, we don't see the impact on recruitment until 2011
# Seems unlikely so we can also put in recruitment residuals
# (We already made them for Example 7)
ple4_catch_iters <- fwd(ple4_mtf, ctrl_catch, sr = ple4_sr, sr.residuals = multi_rec_residuals, sr.residuals.mult = TRUE)
catch(ple4_catch_iters)
plot(window(ple4_catch_iters, start = 1991, end = 2011))
rec(ple4_catch_iters)

# A projection with stochastic catch and recruiment


#---------------------------------------------------------------
# Putting all this together
#---------------------------------------------------------------

# Here we show an example of the type MTFs that are run in the STECF Med group
# (using ple4...)

library(FLAssess)
library(FLa4a)

data(ple4)
data(ple4.indices)

# Run an assessment with default options
fit <- a4aSCA(stock = ple4,
    indices = ple4.indices,
    fit="assessment")

niters <- 1000
# Use simulate to generate 1000 iterations
fits <- simulate(fit, niters)
ple4_fits <- ple4 + fits
ple4_fit <- ple4 + fit

# We now an assessment with uncertainty
summary(ple4_fits)
plot(ple4_fits)

# Set up the MTF
# The MTF will run from the first year after the final year in our stock object upto 2020
final_year <- 2020
# The start year is 2009
mtf_years <- 2009:final_year
no_mtf_years <- length(mtf_years)

# Set up the future stock object using the assessed stock with uncertainty
# Here we use the default assumptions (e.g. weights are means of the last 3 years)
ple4_mtf <- stf(ple4_fits, nyears = no_mtf_years)
summary(ple4_mtf)

# Get the reference points
library(FLBRP)
# We could get a reference point for each iteration
# But generally reference points are agreed as a single value without uncertainty
# You can calculate the reference point or use one agreed by an Expert WG
ple4_brp <- brp(FLBRP(ple4_fit))
# e.g. MSY is
f01 <- c(refpts(ple4_brp)["f0.1","harvest"])

# We also Fbar status quo - the geometric mean of the last 3 years
# This will have iterations
fbar_sq <- exp(apply(log(fbar(ple4_fits)[,ac(2006:2008)]), c(1,3:6), mean))

# The MTF performs forecasts with different fishing strategies including:
#     F status quo
#     F01,
#     decrease 10% pa, 
#     decrease to F01 by 2014
#     decrease to F01 by 2015
#     decrease to F01 by 2020
no_scenarios <- 6
# We make a array that holds the F values of each scenario and iteration
# Dim 1 is scenarios, Dim 2 is years, Dim 3 is iterations
fbar_scenarios <- array(NA, dim = c(no_scenarios, no_mtf_years, niters), dimnames = list(scenario = 1:no_scenarios, year = mtf_years, iter = 1:niters))

# Fill up the scenarios
fbar_scenarios[1,,] <- fbar_sq # status quo
fbar_scenarios[2,,] <- f01 # F01
fbar_scenarios[3,,] <- sapply(fbar_sq, function(x) x * 0.9^(0:(no_mtf_years-1))) # decrease 10% every year
# Fill up everything else with f01 to start with
fbar_scenarios[4:6,,] <- f01
fbar_scenarios[4,1:6,] <- sweep(sapply((f01 - fbar_sq) / (2014 - 2009), function(x) x * (0:5)), 2, c(fbar_sq), "+") # decrease to F01 by 2014
fbar_scenarios[5,1:7,] <- sweep(sapply((f01 - fbar_sq) / (2015 - 2009), function(x) x * (0:6)), 2, c(fbar_sq), "+") # decrease to F01 by 2015
fbar_scenarios[6,,] <- sweep(sapply((f01 - fbar_sq) / (2020 - 2009), function(x) x * (0:11)), 2, c(fbar_sq), "+") # decrease to F01 by 2020
      
# Have a quick look
test <- melt(fbar_scenarios)
test$scenario <- factor(test$scenario)
ggplot(test, aes(x=year, y=value)) + geom_line(aes(group=iter)) + facet_wrap(~scenario)

# Fit the SRR
# We could fit all of them but that takes a long time
#ple4_srs <- fmle(as.FLSR(ple4_fits, model="bevholt"), control=list(trace=0))
# Fit a single SRR from the deterministic assessment
ple4_sr <- fmle(as.FLSR(ple4_fit, model="bevholt"))

# We're going to include stochasticity on the SRR residuals too

# Set up some stochasticity on the SRR residuals
# Make an empty FLQuant to store the residuals
multi_rec_residuals <- FLQuant(NA, dimnames = list(year=mtf_years, iter=1:niters))
# Sample with replacement from the residuals in the SRR object
sample_years <- sample(dimnames(residuals(ple4_sr))$year, niters * length(mtf_years), replace = TRUE)
# The residuals are on a log scale so we take exp() to scale them up
multi_rec_residuals[] <- exp(residuals(ple4_sr)[,sample_years])

# Now do the projections

# Store the results from each scenario in a list
mtf_results <- list()
# Loop over the scenarios
for (scenario in 1:nrow(fbar_scenarios)) {
    cat("Scenario: ", scenario, "\n")
    # Make the target object for the projection
    # Use the first iteration (it doesn't matter - it doesn't actually get used)
    ctrl_target <- data.frame(year = mtf_years,
                              quantity = "f",
                              val = fbar_scenarios[scenario,,1])
    # Set the control object - year, quantity and value for the moment
    ctrl_f <- fwdControl(ctrl_target)
    # Fix the target array to include stochasticity
    new_trgtArray <- array(NA, dim=c(12,3,niters), dimnames = list(1:12, c("min","val","max"),iter=1:niters))
    new_trgtArray[,"val",] <- fbar_scenarios[scenario,,]
    ctrl_f@trgtArray <- new_trgtArray
    # Project
    ple4_mtf_fwd <- fwd(ple4_mtf, ctrl = ctrl_f, sr = ple4_sr, sr.residuals = multi_rec_residuals, sr.residuals.mult = TRUE, maxF = 10.0)
    # Check it has worked - uncomment out to check scenario by scenario
    #plot(ple4_mtf_fwd)
    # Results
    mtf_results[[scenario]] <- ple4_mtf_fwd
}

# Name the results list
names(mtf_results) <- c("Constant status quo",
                        "Constant F0.1",
                        "Decrease by 10% p.a.",
                        "Decrease to F0.1 by 2014",
                        "Decrease to F0.1 by 2015",
                        "Decrease to F0.1 by 2020")

# Plot a scenario just to check it out
plot(mtf_results[[4]])

#--------------------------------------------------------------------------

# Summaries and plots

# Generate summaries of these results: SSB, Fbar, Rec and Catch
# Pull out the median and some other quantile levels (here the 10% and 90%)
quantile_levels <- c(0.1,0.5,0.9)

summary_table <- ldply(mtf_results, function(x){
    ssbs <- ssb(x)
    ssb_ql <- apply(ssbs,1:5,quantile, probs=quantile_levels)
    ssb_ql <- cbind(measure = "SSB",melt(drop(ssb_ql)))
    recs <- rec(x)
    rec_ql <- apply(recs,1:5,quantile, probs=quantile_levels)
    rec_ql <- cbind(measure = "Recruitment",melt(drop(rec_ql)))
    fbars <- fbar(x)
    fbar_ql <- apply(fbars,1:5,quantile, probs=quantile_levels)
    fbar_ql <- cbind(measure = "Fbar",melt(drop(fbar_ql)))
    catchs <- catch(x)
    catch_ql <- apply(catchs,1:5,quantile, probs=quantile_levels)
    catch_ql <- cbind(measure = "Catch",melt(drop(catch_ql)))
    output <- rbind(ssb_ql, rec_ql, fbar_ql, catch_ql)
    return(output)})

# Flatten this for plotting purposes 
stc <- dcast(summary_table, .id + measure + year ~ Var1)
# Having a '%' in your column name is a problem
# So we replace them
names(stc)[names(stc) %in% paste(quantile_levels * 100,"%",sep="")] <- paste("q",quantile_levels*100,sep="")

# A selection of plots

# Summary plot
min_year <- 2005
legend_title <- "F scenario"
p <- ggplot(stc[stc$year>=min_year,])
p <- p + geom_line(aes(x=year,y=q50, colour = .id)) + facet_wrap(~measure, scales="free")
p <- p + geom_ribbon(aes(x=year,ymin = q10, ymax = q90, fill=.id), alpha = 0.3)
p <- p + labs(colour = legend_title, fill=legend_title)
p <- p + scale_y_continuous(name="")
p <- p + theme(legend.position="bottom", legend.direction = "vertical", legend.title.align=0.5)
p <- p + guides(colour = guide_legend(nrow=2))
p
# Looks a bit crowded
# Export the plot
#png(file="example_mtf_plot.png")
#print(p)
#dev.off()
# Or use ggsave
ggsave(filename="example_mtf_plot.png", plot=p)

# Plot one scenario with a few iterations
iters <- 1:5
plotting_scenario <- "Constant F0.1"
# Make the data
iter_data <- rbind(
    cbind(measure = "SSB",as.data.frame(ssb(mtf_results[[plotting_scenario]])[,ac(min_year:final_year),,,,iters])[,c("year","iter","data")]),
    cbind(measure = "Recruitment",as.data.frame(rec(mtf_results[[plotting_scenario]])[,ac(min_year:final_year),,,,iters])[,c("year","iter","data")]),
    cbind(measure = "Fbar",as.data.frame(fbar(mtf_results[[plotting_scenario]])[,ac(min_year:final_year),,,,iters])[,c("year","iter","data")]),
    cbind(measure = "Catch",as.data.frame(catch(mtf_results[[plotting_scenario]])[,ac(min_year:final_year),,,,iters])[,c("year","iter","data")]))
p <- ggplot(stc[stc$year>=min_year & stc$.id == plotting_scenario,])
p <- p + geom_line(aes(x=year,y=q50)) + facet_wrap(~measure, scales="free")
p <- p + geom_ribbon(aes(x=year,ymin = q10, ymax = q90), alpha = 0.1)
p <- p + scale_y_continuous(name="")
p <- p + theme(legend.position = "none")
# Superimpose a few iterations
p <- p + geom_line(data=iter_data, aes(x=year,y=data, colour = iter))
p

# Export the plot
ggsave(filename="example_mtf_plot2.png", plot=p)
#png(file="example_mtf_plot2.png")
#print(p)
#dev.off()


