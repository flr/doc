## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("FLAssess","FLBRP","ggplotFL"), repos="http://flr-project.org/R")

## ---- eval=TRUE----------------------------------------------------------
# Load the required packages
library(FLAssess)
library(FLash)
library(ggplotFL)
library(FLBRP)
# Load the data
data(ple4)
summary(ple4)

## ---- eval=TRUE----------------------------------------------------------
maxyr_stk <- range(ple4)[["maxyear"]]
ple4_stf <- stf(ple4,nyears=3,wts.nyears=3, na.rm=TRUE)
maxyr_stf <- range(ple4_stf)[["maxyear"]]

## ---- eval=TRUE----------------------------------------------------------
range(ple4_stf)
stock.wt(ple4_stf)[,ac((maxyr_stf-5):maxyr_stf)]

## ---- eval=TRUE----------------------------------------------------------
mat(ple4_stf)[,ac((maxyr_stf-5):maxyr_stf)]

## ----figA----------------------------------------------------------------
ggplot(harvest(ple4_stf)[,ac((maxyr_stf-5):maxyr_stf)]) + geom_line(aes(x=age, y=data)) + facet_wrap(~year)

## ---- eval=TRUE----------------------------------------------------------
stock.n(ple4_stf)[,ac((maxyr_stf-5):maxyr_stf)]

## ---- eval=TRUE----------------------------------------------------------
discards.n(ple4_stf)[,ac((maxyr_stf-5):maxyr_stf)]
landings.n(ple4_stf)[,ac((maxyr_stf-5):maxyr_stf)]
# Compare above landings.n for the forecast years with
yearMeans((landings.n(ple4)/(landings.n(ple4)+discards.n(ple4)))[,ac((maxyr_stk-2):maxyr_stk)])
# Furthermore, landings and discards proportions sum to 1
landings.n(ple4_stf)[,ac((maxyr_stf-2):maxyr_stf)] + discards.n(ple4_stf)[,ac((maxyr_stf-2):maxyr_stf)]

## ---- eval=TRUE----------------------------------------------------------
mean_rec <- exp(mean(log(rec(ple4))))
ple4_sr <- as.FLSR(ple4, model="geomean")
params(ple4_sr)['a',] <- mean_rec
params(ple4_sr)

## ----figB----------------------------------------------------------------
round(fbar(ple4),3)
ggplot(fbar(ple4), aes(x=year,y=data)) + geom_line()

## ---- eval=TRUE----------------------------------------------------------
fbar_SQ <- mean(fbar(ple4)[,as.character(maxyr_stk)])

## ---- eval=TRUE----------------------------------------------------------
# Set the control object - year, quantity and value for the moment
ctrl_target <- data.frame(year = 2009:2011, quantity = "f", val = fbar_SQ)
ctrl_f <- fwdControl(ctrl_target)
ctrl_f

## ---- eval=TRUE----------------------------------------------------------
ple4_sq <- fwd(ple4_stf, ctrl = ctrl_f, sr = ple4_sr)

## ---- eval=TRUE----------------------------------------------------------
mean_rec
rec(ple4_sq)[,ac((maxyr_stf-5):maxyr_stf)]

## ---- eval=TRUE----------------------------------------------------------
round(fbar_SQ,3)
round(fbar(ple4_sq)[,ac((maxyr_stf-5):maxyr_stf)],3)

## ---- eval=TRUE----------------------------------------------------------
stock.n(ple4_sq)[,ac((maxyr_stf-5):maxyr_stf)]

## ---- eval=TRUE----------------------------------------------------------
round(harvest(ple4_stf)[,ac((maxyr_stf-2):maxyr_stf)],3)
round(harvest(ple4_sq)[,ac((maxyr_stf-2):maxyr_stf)],3)
harvest(ple4_stf)[,ac((maxyr_stf-2):maxyr_stf)] / harvest(ple4_sq)[,ac((maxyr_stf-2):maxyr_stf)]

## ---- eval=TRUE----------------------------------------------------------
landings.n(ple4_sq)[,ac((maxyr_stf-5):maxyr_stf)]
discards.n(ple4_sq)[,ac((maxyr_stf-5):maxyr_stf)]

## ---- eval=TRUE----------------------------------------------------------
plot(ple4_sq) + geom_vline(lty=2,xintercept=an(ISOdate(maxyr_stk+1,1,1)))

## ---- eval=TRUE----------------------------------------------------------
fbar_multiplier <- seq(from = 0, to = 2, by = 0.2)

## ---- eval=TRUE----------------------------------------------------------
fbar_scenarios <- cbind(rep(fbar_SQ,length(fbar_multiplier)),
                        fbar_multiplier*fbar_SQ,
                        fbar_multiplier*fbar_SQ)

## ---- eval=TRUE----------------------------------------------------------
f01 <- c(refpts(brp(FLBRP(ple4)))["f0.1","harvest"])
# Add the F0.1 scenario as a final scenario 
fbar_scenarios <- rbind(fbar_scenarios, c(fbar_SQ,f01,f01))

## ---- eval=TRUE----------------------------------------------------------
# Add some names
colnames(fbar_scenarios) <- c("2009","2010","2011")
rownames(fbar_scenarios) <- c(fbar_multiplier, "f01")
fbar_scenarios

## ---- eval=TRUE----------------------------------------------------------
stf_results <- matrix(NA,nrow = nrow(fbar_scenarios),ncol = 8)
# Set some column names
colnames(stf_results) <- c('Fbar',
    paste0('Catch',maxyr_stk+1), 
    paste0('Catch',maxyr_stk+2),
    paste0('Catch',maxyr_stk+3),
    paste0('SSB',maxyr_stk+2),
    paste0('SSB',maxyr_stk+3),
    paste0('SSB_change_',maxyr_stk+2,'-',maxyr_stk+3,'(%)'),
    paste0('Catch_change_',maxyr_stk,'-',maxyr_stk+2,'(%)'))

## ---- eval=TRUE, results="hide"------------------------------------------
# Set up an FLStocks object to store the resulting FLStock each time
stk_stf <- FLStocks()
# Loop over the scenarios (each row in the fbar_scenarios table)
for (scenario in 1:nrow(fbar_scenarios)) {
    cat("Scenario: ", scenario, "\n")
    flush.console()
    # Make a target object with F values for that scenario
    # Set the control object - year, quantity and value for the moment
    ctrl_target <- data.frame(year = (maxyr_stf-2):maxyr_stf,
                              quantity = "f",
                              val = fbar_scenarios[scenario,])
    # ctrl_target
    ctrl_f <- fwdControl(ctrl_target)
    # Run the forward projection. We could include an additional argument, maxF.
    # By default the value of maxF is 2.0. It could be increased to 10.0, say,
    # so that F is less limited, and the bound is not hit (not a problem here).
    ple4_fwd <- fwd(ple4_stf, ctrl = ctrl_f, sr = ple4_sr)#, maxF = 10.0)
    ## Check it has worked - uncomment out to check scenario by scenario
    # plot(ple4_fwd[,ac(2001:2011)])
    # Store the result - if you want to, comment out if unnecessary
    stk_stf[[as.character(scenario)]] <- ple4_fwd

    # Fill results table
    stf_results[scenario,1] <- round(fbar(ple4_fwd)[,ac(2011)],3) # final stf year
    stf_results[scenario,2] <- catch(ple4_fwd)[,ac(maxyr_stk+1)] # 1st stf year
    stf_results[scenario,3] <- catch(ple4_fwd)[,ac(maxyr_stk+2)] # 2nd stf year
    stf_results[scenario,4] <- catch(ple4_fwd)[,ac(maxyr_stk+3)] # final stf year
    stf_results[scenario,5] <- ssb(ple4_fwd)[,ac(maxyr_stk+2)] # 2nd stf year
    stf_results[scenario,6] <- ssb(ple4_fwd)[,ac(maxyr_stk+3)] # final stf year
    
    # change in ssb in last two stf years
    stf_results[scenario,7] <- round((ssb(ple4_fwd)[,ac(maxyr_stk+3)]-ssb(ple4_fwd)[,ac(maxyr_stk+2)])/
                                      ssb(ple4_fwd)[,ac(maxyr_stk+2)]*100,1) 
    
    # change in catch from true year, to 2nd to last stf year
    stf_results[scenario,8] <- round((catch(ple4_fwd)[,ac(maxyr_stk+2)]-catch(ple4_fwd)[,ac(maxyr_stk)])/
                                      catch(ple4_fwd)[,ac(maxyr_stk)]*100,1) 
}
# Give the FLStocks object some names
names(stk_stf) <- rownames(fbar_scenarios)

# Plotting
plot(stk_stf)

## ---- eval=TRUE----------------------------------------------------------
stf_results

