# Tutorial: Introuduction to growth and slicing using the a4a framework
# Copyright 2014 FLR Core Development Team
# Distributed under the GPL 2 or later

#-------------------------------------------------------
# Preliminaries
#-------------------------------------------------------

install.packages("FLa4a", repos="http://flr-project.org/R")

# Load the FLa4a library
library(FLa4a)

#-------------------------------------------------------
# The a4aGr class - a simple example
#-------------------------------------------------------

showClass("a4aGr")

# We're going to focus on the grMod, grInvMod
# and params slots to start with

# grMod and grInvMod are formulae for t~len and len~t
vb <- ~linf*(1-exp(-k*(t-t0)))
inverse_vb <- ~t0-1/k*log(1-len/linf)
# What are they?
class(vb)

# We also create some parameters (matching those
# in the formulae, other than len and t)
# We set some units too (not essential)
vb_params <- FLPar(linf=58.5, k=0.086, t0=0.001,
                   units=c("cm","ano-1","ano"))

# Make the a4aGr object
vb_gr <- a4aGr(grMod=vb, grInvMod=inverse_vb,
               params=vb_params)

# What did we make?
vb_gr
# Look at the slots we set
grMod(vb_gr)
grInvMod(vb_gr)
params(vb_gr)
# Note only 1 iteration in the params - no uncertainty (yet...)
dim(params(vb_gr))

# What can we do with this object?
# Convert length-to-ages and ages-to-lengths
# Works with a single value
predict(vb_gr, len=20) #  note that the argument has the same name as the parameter in the growth model
predict(vb_gr, t=5) # Using the inverse model
predict(vb_gr, t=predict(vb_gr, len=20)) # model into the inverse model (sanity check)

# Also works with vectors of lengths or ages
predict(vb_gr, len=5:10+0.5)
predict(vb_gr, t=5:10+0.5)
predict(vb_gr, t=predict(vb_gr, len=5:10+0.5))

# So far, so good
# The model is deterministic with no uncertainty.
# Now we start to get a little mode complicated
# and add parameter uncertainty

#-------------------------------------------------------
# Adding multivariate normal parameter uncertainty
#-------------------------------------------------------

# We need to construct and populate a variance-covariance matrix
# We have 3 parameters in our vonB model: linf, k and t0

# Make an empty vcov matrix
varcov <- matrix(NA, ncol=3, nrow=3)
# Fill it up with made up values (though these should come from fitting a von B model or some other source)
diag(varcov) <- c(100, 0.001,0.001)
varcov[upper.tri(varcov)] <- varcov[lower.tri(varcov)] <- c(0.1,0.1,0.0003)
varcov
# Note that the order of columns and rows in the matrix
# should match that in the params slot of the a4aGr object
# i.e. here the order is linf, k, t0

# Create the a4aGr object as before but now we also include the variance-covariance matrix
# Make the a4aGr object
vb_gr <- a4aGr(grMod=vb, grInvMod=inverse_vb,
               params=vb_params, vcov=varcov)
vb_gr

# Note that we still only have 1 iteration of the parameters
dim(params(vb_gr))

# We can simulate (sample) from this model using mvrnorm() (from the MASS package - see help page if interested)
# Here we sample 10000 times
vb_norm <- mvrnorm(10000,vb_gr)
class(vb_norm)

# Now we have 10000 iterations of each parameter, randomly sampled from the multivariate normal distribution
params(vb_norm) # note the brackets indicating iterations
dim(params(vb_norm))

# We can look at the maginal distributions of the  parameters
par(mfrow=c(3,1))
hist(c(params(vb_norm)["linf",]), main="linf", xlab="")
hist(c(params(vb_norm)["k",]), main="k", prob=TRUE, xlab="")
hist(c(params(vb_norm)["t0",]), main="t0", xlab="")

# And the spread of them
splom(data.frame(t(params(vb_norm)@.Data)), pch='.')

# We can now convert from length to ages data based on the parameter iterations.
ages <- predict(vb_norm, len = 20)
# We get one value of t (age) for every parameter iteration at for length = 20
# It also works for vectors of length (or age)
ages <- predict(vb_norm, len=(5:10)+0.5)
# which gives us 10000 samples for every length
dim(ages)
# look at the first few for example
ages[,1:5] 

# We can plot the growth curve by passing a vector of ages and plotting the simulated lengths
boxplot(t(predict(vb_norm, t=0:50+0.5)))

# Some of the growth curves may be a little weird because of the normally distributed random parameters
# This is because we are using a multivariate normal, i.e. theoretically there are no bounds on the parameter values
# Let's try a different distribution

#-------------------------------------------------------
# Adding multivariate triangular parameter uncertainty
#-------------------------------------------------------

# For a triangular distribution we need to set a
# minimum and maximum and (optionally) a median
# value of the parameters
# Here we scrape the data from FishBase

addr <- "http://www.fishbase.org/PopDyn/PopGrowthList.php?ID=501"
# Scrape the data - you will need to install the package XML if you don't yet have it
install.packages("XML")
library(XML)
tab <- try(readHTMLTable(addr))
# What did we get?
tab
class(tab)
# A list of stuff. We want the dataTable element
tab$dataTable
# Interrogate the data table and get vectors of the values
linf <- as.numeric(as.character(tab$dataTable[,2]))
k <- as.numeric(as.character(tab$dataTable[,4]))
t0 <- as.numeric(as.character(tab$dataTable[,5]))
# We want the min, max and median of these vales
# Set the min (a) , max (b) and median (c) values for each parameters as a list of lists
# Note that t0 has no 'c' (max) value and the min and max are taken from the Interquartile Range.
# With no median the distribution is symmetrical
tri_pars <- list(list(a=min(linf), b=max(linf), c=median(linf)), # linf
             list(a=min(k), b=max(k), c=median(k)), # k
             list(a=median(t0, na.rm=T)-IQR(t0, na.rm=T)/2, b=median(t0, na.rm=T)+IQR(t0, na.rm=T)/2)) # t0

# We use the same vb_gr object as above with the same variance-covariance matrix
# To simulate a triangular distribution we call the mvrtriangle() function and also pass in the triangular parameters
# Simulate 10000 times using mvrtriangle (from the FLa4a package)
vb_tri <- mvrtriangle(10000, vb_gr, paramMargins=tri_pars)
class(vb_tri)
params(vb_tri)

# As before we can look at the maginal distributions of the  parameters
par(mfrow=c(3,1))
hist(c(params(vb_tri)["linf",]), main="linf", xlab="")
hist(c(params(vb_tri)["k",]), main="k", prob=TRUE, xlab="")
hist(c(params(vb_tri)["t0",]), main="t0", xlab="")

# And the spread of them
splom(data.frame(t(params(vb_tri)@.Data)), pch='.')

# And the resulting growth curves
boxplot(t(predict(vb_tri, t=0:50+0.5)))

# No weird results as the parameters have bounds

boxplot(t(predict(vb_tri, len=20)))


#-------------------------------------------------------
# Adding uncertainty with other copulas
#-------------------------------------------------------

# The mvrtriangle() function makes use of copulas
# Sklar's Theorem states that any multivariate joint distribution
# can be written in terms of univariate marginal distribution functions
# and a copula which describes the dependence structure between the variables. 
# http://en.wikipedia.org/wiki/Copula_(probability_theory)

# A more general approach to adding parameter uncertainty is to make use of whatever copula and marginal distribution you want.
# This is possible with mvrcop() the function (in the FLa4a package).
# This is essentially a wrapper to the rMvdc() method and the copula methods (e.g. archmCopula()) in the copula package.
# The example below keeps the same parameters and changes only the copula type and family but a lot more can be done.
# For more detauls see the package 'copula'.

vb_cop <- mvrcop(10000, vb_gr, copula="archmCopula", family="clayton", param=2, margins="triangle", paramMargins=tri_pars)

par(mfrow=c(3,1))
hist(c(params(vb_cop)["linf",]), main="linf", xlab="")
hist(c(params(vb_cop)["k",]), main="k", prob=TRUE, xlab="")
hist(c(params(vb_cop)["t0",]), main="t0", xlab="")

splom(data.frame(t(params(vb_cop)@.Data)), pch=".")
boxplot(t(predict(vb_cop, t=0:20+0.5)))

#-------------------------------------------------------
# Slicing with the l2a() methods
#-------------------------------------------------------

# Now we have an a4aGr growth model with parameter uncertainty.
# To convert the length-based data to age-based data we use the growth model and the l2a() method.

# When converting from length-based data to age-based data you think how the aggregation of length classes is performed.
# For example, individuals in length classes, 1-2, 2-3, and 3-4 cm may all be considered as being of age 1.
# How should the values in those length classes be combined?

# If the values are abundances then the values should be summed.
# But summing other types of values such as weights and rates (like fishing mortality) does not make sense.
# Instead these values shoud be averaged over the length classes (weighted by the abundance for weights).
# This is controlled using the 'stat' argument which can be either 'mean' or 'sum' (the default).

# We need some data so we load some that we prepared earlier
# It's Red Fish (actually output data from a Gadget model)
load("data/rfLen.rdata")
# This data includes an object of type FLStockLen
summary(rfLen.stk)

# Slicing an FLQuant
# We take the catch numbers from the FLStockLen object
# and slice them
dim(catch.n(rfLen.stk))
# Note that it has seasons
# show a bit of it
catch.n(rfLen.stk)[1:10,1:5,,1,]

# If you have a lot of iterations in your growth model this can take a very long time (sorry, we are working on improving this)
# For this example we will reduce the number of iterations
# Make the growth model with 100 iterations and the parameters we made
# earlier
vb_small_tri <- mvrtriangle(100, vb_gr, paramMargins=tri_pars)
# slice using stat='sum'
cage <- l2a(catch.n(rfLen.stk), vb_small_tri, stat="sum")
# What happened
dim(catch.n(rfLen.stk))
dim(cage)
# We have as many iterations as there were in the growth model
# and the first dimension is now age
cage[1:10,1:5,,1,]
dimnames(cage)
# We have contiguous ages but do we really need so many ages?
# Many of the later ages have comparatively small abundances
# (the age range has to be the same for all dimensions, so if 1 iteration has a very long age structure, all iterations do)
cage[,1,,1,]


# Slicing an FLStockLen
# An FLStockLen has a range of different slot types: *.n, *.wt, mat etc
# When slicing an FLStockLen, the *.n slots are summed over lengths, the *.wt are meaned, weighted by the corresponding *.n slot
# The other slots are meaned
# We call l2a() as before. Now there is a plusgroup argument.
sage <- l2a(rfLen.stk, vb_small_tri, plusgroup=20)
summary(sage)
plot(sage)

# Same for FLIndex
iage <- l2a(rfTrawl.idx, vb_small_tri)
summary(iage)

# Running l2a() with a growth model which has uncertainty parameters
# may throw up some interesting results...
# For example, the age of your minimum length class may be less
# than 0.
# This is probably to do with your t0 values.






