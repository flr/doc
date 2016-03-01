# Tutorial: Estimating natural mortality with the a4a framework
# Copyright 2014 FLR Core Development Team
# Distributed under the GPL 2 or later

#-------------------------------------------------------
# Preliminaries
#-------------------------------------------------------

# Load the FLa4a library
library(FLa4a)

#-------------------------------------------------------
# The a4aM class - a simple example
#-------------------------------------------------------

showClass("a4aM")

# 3 models:
# shape - age effect
# level - 'average' level
# trend - time trend
# These models are of type FLModelSim
# We need to supply model (formula) and parameters (FLPar)

# Simple example 0.2
# Make a level model
level1 <- FLModelSim(model = ~a,
                     params = FLPar(a = 0.2))
level1
# Note that we have a slot for 'vcov', we'll use this later

# Make the a4aM object passing in only the level model
# We don't use a shape or trend model (yet...)
m1 <- a4aM(level=level1)

# What do we have?
class(m1)
m1
# The level model has been set
# The shape and trend models have the default ~1
# Remember these models are multiplicative
level(m1)
shape(m1)
trend(m1)

# We use the m() method to create an FLQuant of m
# The dimensions of the FLQuant are set in the range slot
# At the moment we no have dimensions specified
range(m1)
# Note the use of minmbar - we'll come to this

# If we call m() on this object we get an FLQuant with no ages or years
m(m1)
dimnames(m(m1))

# We can set ages and years using the rngage() and rngyear() methods
# (Or you can set the range slot directly)
# First some ages
rngage(m1) <- c(0,5)
range(m1)
m(m1)
# Then some years
rngyear(m1) <- c(2000,2005)
range(m1)
m(m1)

# The minmbar and maxmbar values are the age (or length) range
# over which the mean m is calculated.
# This is similar to the fbar range
# The value of mean m is set by the level model
# In the previous example level was a single model so the mbar range
# was not important.

#-------------------------------------------------------
# A more interesting m model
#-------------------------------------------------------

# Here we use Jensen's second estimator (see Kenshington, 2013)
# m = 1.5 k
# for the level model
level2 <- FLModelSim(model=~1.5*k, params=FLPar(k=0.4))
level2

# And we use an exponential decay model for the shape model
# i.e. m is high on young ages
shape2 <- FLModelSim(model=~exp(-age-0.5))
shape2
# Note that the model is a function of age
# but that age does not feature in the params
# age (and len) are 'reserved' names
# If they are used in the model, the values are taken from the
# FLQuant dimensions to calculate values

# Make the new growth model
m2 <- a4aM(shape=shape2, level=level2)
m2

# Set some age and year ranges
rngage(m2) <- c(0,5)
rngyear(m2) <- c(2000,2005)
m(m2)

# Shape looks OK. But why is the value 0.6 in age 0
# The 0.6 comes from 1.5 * k, where k = 0.4
# Look at mbar (similar to fbar - the age range over which the mean m is calculated)
range(m2)
# It's 0 to 0, so the mbar range is just age 0
# The value of m over the mbar range is given by the level model - hence 0.6 at 0.6
# change it to the whole age range
rngmbar(m2) <- c(0,5)
range(m2)
m(m2)
# Take the mean over the mbar range
apply(m(m2),2,mean)
# It's 0.6
# The value of m over the mbar range in a year is given by the level model 

#-------------------------------------------------------
# Including a time trend
#-------------------------------------------------------

# Here we use NAO to include a time trend in m
# First get the data:
nao.orig <- read.table("http://www.cdc.noaa.gov/data/correlation/nao.data", skip=1, nrow=62, na.strings="-99.90")
# Set the dimnames from which to make an FLQuant
dnms <- list(quant="nao", year=1948:2009, unit="unique", season=1:12, area="unique")
# Build an FLQuant from the NAO data
nao.flq <- FLQuant(unlist(nao.orig[,-1]), dimnames=dnms, units="nao")
dim(nao.flq)
# It has 12 seasons
# Interested in impact of NAO on the quarter before spawning
# Build covariate by calculating mean over the first 3 months
nao <- seasonMeans(nao.flq[,,,1:3]) 
dim(nao)
# We cudely turn this into a positive / negative or 1 / 0 FLQuant
nao <- (nao>0) * 1

# Make we make a trend model
# Note that we include nao as an argument, but is not included in the parameters
# We pass it into m() later on
# nao is really just 1 or 0, so the trend will be 1.5 in nao 'on' years and 1 in nao 'off' years
trend3 <- FLModelSim(model=~1+b*nao, params=FLPar(b=0.5))

# Use the shape and level from the above example
m3 <- a4aM(shape=shape2, level=level2, trend=trend3)
rngage(m3) <- c(0,5)
rngyear(m3) <- c(2000,2005)
# Pass in the nao with the required year range
m(m3, nao = window(nao,2000,2005))
# Years where the nao is 'on' (2000 and 2002) are affected

#-------------------------------------------------------
# Including multivariate normal uncertainty
#-------------------------------------------------------

# We can include uncertainty on the parameters in each of the three models
# We make use of the FLModelSim 'mvr' methods.

# We use the same shape as before
# But for level we'll use Jensen's third estimator (Kenshington, 2013).
# Has two parameters, k and t
# We also include a variance-covariance matrix for the parameter interaction
level4 <- FLModelSim(model=~k^0.66*t^0.57,
                     params = FLPar(k=0.4, t=10),
                     vcov=matrix(c(0.002, 0.01,0.01, 1),
                                 ncol=2))
# Make the m model
m4 <- a4aM(shape=shape2, level=level4)

m4
# Check the number of iterations in the parameters for the level and trend models
params(level(m4))

# Use mvrnorm to sample from the models
m4 <- mvrnorm(1000, m4)
m4
params(level(m4))
# Now we have iterations

rngage(m4) <- c(0,5)
rngyear(m4) <- c(2000,2005)
# Pass in the nao with the required year range
m4flq <- m(m4)
dim(m4flq)

plot(m4flq)


