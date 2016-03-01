################################################################################
# FLSR examples ################################################################
################################################################################

## FLSR is found in FLCore
library(FLCore)

data(ple4)

# So 
##  we have read in our data
##  we have fitted a stock recruitment model

# Now we have an FLStock object that has estimates of stock.n

# stock numbers
stock.n(ple4)

# SSB and recruitment
ssb(ple4)

rec(ple4)

#### Creation from an FLStock

pleSR <-as.FLSR(ple4)

summary(pleSR)


### Note the shift in years, reflecting that recruitment is at age 1 in the ple4
ssb(pleSR)
rec(pleSR)



# what if we want to investigate the recruitment at age 2?
pleSR2 <-as.FLSR(ple4[-1])

### Note the shift in years, reflecting that recruitment is now at age 2
ssb(pleSR2)
rec(pleSR2)

plot(ssb(pleSR), rec(pleSR))



## fitting a SR relationship


model(pleSR) <- ricker()

#### Model and likelihood are functions of the FLSR class
model(pleSR)

#### initial values for the optimiser
initial(pleSR)

#### lower and upper limits for the parameters
lower(pleSR)
upper(pleSR)


################################################################################

# other stock recruitment functional forms exist
bevholt()
shepherd()
cushing()
geomean()
segreg()
rickerSV()
bevholtSV()
shepherdSV()
bevholtAR1()


# the fmle method then fits the SRR using logl and R's optim
pleSR <- fmle(pleSR)

plot(pleSR)


################################################################################

# Let's look something with a better relationship
data(nsher)

plot(ssb(nsher), rec(nsher))

summary(nsher)



###################
## DO Exercise 1
###################









# prediction

newssb <- FLQuant(seq(1, 500000, length = 100))
newrec <- predict(pleSR, ssb = newssb)

plot(ssb(pleSR), rec(pleSR))
lines(newssb, newrec)








###################
## DO Exercise 2
###################






# fixing parameters

model(pleSR) <- bevholt

pleSR <- fmle(pleSR, fixed = list(a = 100000))

plot(pleSR)






# some other stuff

# Profile the likelihood to check the fit
# for a 2-parameter model like Riker, profiles over a range of 2 values
# around MLE estimate
profile(nsher)


# methods exist for Akaike Information Criterion
AIC(nsher)
AIC(nsherFixed)
# and Schwarz's Bayesian Information Criterion

BIC(nsher)
BIC(nsherFixed)




#### Creation from an FLStock
data(ple4)

ple4SR<-as.FLSR(ple4)

### Note the shift in years, reflecting that recruitment is at age 1 in the ple4
ssb(ple4SR)
rec(ple4SR)

summary(ple4SR)
################################################################################


#### Specifying the stock recruitment relationship and error model
## after creating the new object
model(ple4SR)<-bevholt()

## when creating the new object
ple4SR<-as.FLSR(ple4,model="bevholt")
################################################################################


# parameter values can also be fixed
nsherRKFixed  <-fmle(nsherRK, fixed=list(a=63580373))

# Comparison of fits using AIC
AIC(nsherRK)
AIC(nsherRKFixed)
################################################################################

plot(nsherRK)
################################################################################

#### Profile both parameters
profile(nsherRK,which=c("a","b"))

#### Profile alpha
profile(nsherRK,which=c("a"))

#### Profile beta
profile(nsherRK,which=c("b"))

#### Covariance matrix
vcov(nsherRK)

#### Correlation matrix
cov2cor(vcov(nsherRK)[,,1])

## fit Beverton and Holt
nsherBH       <-nsher
model(nsherBH)<-bevholt()
nsherBH       <-fmle(nsherBH)

#### Problems with fitting
pleSegR<-as.FLSR(ple4)

## fit segmented regression
model(pleSegR)<-segreg()

## inspect default starting values
initial(pleSegR)(rec(pleSegR), ssb(pleSegR))






#### Bootstrapping ##################################################################
niter <- 10
model(pleSR) <- bevholt
pleSR <- fmle(pleSR)
res.boot <- sample(c(residuals(pleSR)), niter * dims(pleSR)$year, replace=T)

sr.bt       <- propagate(pleSR, niter)
rec(sr.bt) <- rec(sr.bt)[] * exp(res.boot)


## fits across all iters independently
model(sr.bt) <- bevholt
sr.bt  <-fmle(sr.bt)

plot(rec(sr.bt))

plot( t(params(sr.bt)[drop=TRUE]))

################################################################################

















myricker <- function () 
{
  logl <- function(a, b, rec, ssb) {
    if (a < 1e-10 || b < 1e-10) return(-1e100)
    z <- log(rec) - log(a) - log(ssb) + b * ssb
    n <- length(rec)
    #sigma2 <- var(z)
    sigma2 <- mean(z^2)
    0.5 * (log(1/(2 * pi)) - length(rec) * log(sigma2) - sum(z^2)/(2 * sigma2) ) 
  }
  initial <- structure(function(rec, ssb) {
        S <- c(ssb)
        logS <- log(S)
        logR <- log(c(rec))
        res <- coefficients(lm(logR ~ S, offset = logS))
        return(FLPar(a = exp(res[1]), b = -min(res[2],0)))
    }, lower = rep(-Inf, 2), upper = rep(Inf, 2))
    model <- rec ~ a * ssb * exp(-b * ssb)
    return(list(logl = logl, model = model, initial = initial))
}

