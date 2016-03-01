# FLS.R - DESC
# FLS.R

# Copyright 2015 Iago Mosqueira. Distributed under the GPL 2.
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

library(FLCore)

# We have the result of an stock assesment (using VPA) in ple4
data(ple4)

# and now want to fit an stock-recruitment relationship

rec(ple4)

ssb(ple4)

# We can convert and FLStock into an FLSR

p4sr <- as.FLSR(ple4)

summary(p4sr)

# As recruitts are of age=1, the lag between ssb and rec is also 1

rec(p4sr)

ssb(p4sr)

plot(ssb(p4sr), rec(p4sr))

# To fit a model, we need to select a functional form

model(p4sr) <- ricker()

# so p4sr is all set up

# model formula
model(p4sr)

# log-likelihood function
logl(p4sr)

# initial values function
initial(p4sr)

# and ouputs: params
params(p4sr)

# Fitting thorugh MLE

p4sr <- fmle(p4sr)

# gives us some results, not too good

summary(p4sr)

plot(p4sr)

# default method is Nelder-Mead

p4sr <- fmle(p4sr, method='L-BFGS-B')


# The nhser dataset is perfect for FLSR

data(nsher)

plot(ssb(nsher), rec(nsher))


# There are also other SR models to choose from
bevholt()

shepherd()

cushing()

geomean()

segreg()

rickerSV()

bevholtSV()

shepherdSV()

bevholtAR1()

# Let's try ricker

model(nsher) <- 'ricker'

nhri <- fmle(nsher)

# and then bevholt

model(nsher) <- 'bevholt'

nhbh <- fmle(nsher)

# and compare the fits

plot(nhri)

plot(nhbh)

# ... using AIC

AIC(nhri)

AIC(nhbh)


# or Schwarz's Bayesian Information Criterion

BIC(nhri)

BIC(nhbh)


# Predict recruitments using predict()

predict(nhri, ssb=FLQuant(seq(185, 500, length=10)))


# Profile the likelihood to check the fit

profile(nhri)

profile(p4sr)


# Fix some parameters, e.g. steepness

model(p4sr) <- bevholtSV

p4sr <- fmle(p4sr, fixed = list(s = 0.8))

plot(p4sr)

params(p4sr)


# Other outputs include the covariance matrix
vcov(nhri)

# from which we can derive a correlation matrix
cov2cor(vcov(nhri)[,,1])


# Bootstrapping

niter <- 10

model(p4sr) <- bevholt

p4sr <- fmle(p4sr)

res.boot <- sample(c(residuals(p4sr)), niter * dims(p4sr)$year, replace=T)

p4sb <- propagate(p4sr, niter)

rec(p4sb) <- rec(p4sb) * exp(res.boot)

p4sb <- fmle(p4sb)

params(p4sb)

plot(rec(p4sb))

plot(fitted(p4sb))
