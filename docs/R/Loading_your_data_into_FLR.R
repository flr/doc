## ---- ini, echo=FALSE, results='hide', message=FALSE, warnings=FALSE, cache=FALSE----
library(knitr)
source("R/ini.R")

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("ggplot2"))
## install.packages(c("FLcore", "ggplotFL"), repos="http://flr-project.org/R")

## ---- pkgs---------------------------------------------------------------
# This chunk loads all necessary packages, trims pkg messages
library(FLFleet)
library(ggplotFL)

## ---- getfiles, message=FALSE--------------------------------------------
dir <- tempdir()
download.file("http://flr-project.org/doc/src/loading_data.zip", file.path(dir, "loading_data.zip"))
unzip(file.path(dir, "loading_data.zip"), exdir=dir)

## ------------------------------------------------------------------------
catch.n <- read.csv(file.path(dir,"catch_numbers.csv"), row=1)

## ------------------------------------------------------------------------
class(catch.n)

## ------------------------------------------------------------------------
catch.n.matrix <- as.matrix(catch.n)
catch.n.matrix[,1:8]

## ------------------------------------------------------------------------
catch.n.flq <- FLQuant(catch.n.matrix, dimnames=list(age=1:7, year = 1957:2011))
catch.n.flq[,1:7]


## ---- readVPA------------------------------------------------------------
# Read from a VPA text file
catch.n <- readVPAFile(file.path(dir, "her-irlw","canum.txt"))
class(catch.n)

## ---- readFLStock--------------------------------------------------------
# Read a collection of VPA files, pointing to the Index file:
her <- readFLStock(file.path(dir,'her-irlw','index.txt'))
class(her)

## ---- readFLStock2-------------------------------------------------------
summary(her)

## ---- AddMissingAssessmentData-------------------------------------------
her@stock.n <- readVPAFile(file.path(dir, "her-irlw", "n.txt"))
print(her@stock.n[,ac(2007:2011)]) # only print 2007:2011

her@harvest <- readVPAFile(file.path(dir,"her-irlw", "f.txt"))


## ---- CheckConsistency---------------------------------------------------
# The sum of products (SOP)
apply(her@landings.n * her@landings.wt, 2, sum)[,ac(2007:2011)]
# and the value read in from the VPA file
her@landings[,ac(2007:2011)]

## They are not the same!!  We correct the landings to be the same as the SOP - there is a handy function for this purpose
her@landings <- computeLandings(her)

# In addition, there is no discard information
her@discards.wt[,ac(2005:2011)]
her@discards.n[,ac(2005:2011)]
# Set up the discards and catches
her@discards.wt   <- her@landings.wt
her@discards.n[]  <- 0
her@discards      <- computeDiscards(her)
her@catch         <- her@landings
her@catch.wt      <- her@landings.wt
her@catch.n       <- her@landings.n


## ---- Descriptions-------------------------------------------------------
summary(her)

#name and descriptions
her@name # ok
her@desc # ok

# Set the Fbar range for the stock 
her@range[c('minfbar','maxfbar')] # ok, but can be filled with  <- c(min,max)

# set the plus group
her@range['plusgroup']  <- 7 # final year is a plusgroup

## Units
units(her@catch)    <- units(her@discards)    <- units(her@landings)    <- units(her@stock)    <- 'tonnes'
units(her@catch.n)  <- units(her@discards.n)  <- units(her@landings.n)  <- units(her@stock.n)  <- '1000'
units(her@catch.wt) <- units(her@discards.wt) <- units(her@landings.wt) <- units(her@stock.wt) <- 'kg'
units(her@harvest) <- 'f'


## ---- Plot---------------------------------------------------------------
summary(her)
plot(her) + theme_bw() # using the simple black and white theme

## ------------------------------------------------------------------------
indices <- readFLIndices(file.path(dir, "ple4_ISIS.txt"))

## ------------------------------------------------------------------------
indices <- read.table(file.path(dir, "ple4Index1.txt"))

## ------------------------------------------------------------------------
indices <- FLQuant(as.matrix(indices), dimnames=list(age=1:8, year = 1985:2008))

## ------------------------------------------------------------------------
indices <- FLIndex(index = indices)

## ------------------------------------------------------------------------
indices <- FLIndices(indices)

plot(indices[[1]])


## ------------------------------------------------------------------------
indices[[1]]@range[c('startf', 'endf')] <- c(0.66,0.75)

## ---- level table, echo = F----------------------------------------------
kable(data.frame(Level = c(1,2,3),
		 Class = c('FLFleet','FLMetier(s)','FLCatch(es)'),
		 Contains = c('variables relating to vessel level activity',
			     'variables relating to fishing level activity',
			     'variables relating to stock catches')))

## ---- FLFleet slots------------------------------------------------------
# FLFleet level
summary(FLFleet())
# FLMetier level
summary(FLMetier())
# FLCatch level
summary(FLCatch())

## ---- Fleetdata, echo = F------------------------------------------------
kable(data.frame(Fleet = c('Fleet1', 'Fleet2'),
		 Metier = c('Metier1', 'Metier1'),
		 Stock = c('Stock1', 'Stock2'),
		 type = c('landings.n', 'landings.wt'),
		 age = c(1,1),
		 year = c(2011,2011),
		 unit = c(1,1),
		 season = c('all', 'all'),
		 area = c('unique', 'unique'),
		 iter = c(1,1),
		 data = c(254,0.3)))

## ---- Generating FLFleets, eval = F--------------------------------------
## # Example of generating fleets
## fl.nam <- unique(data$Fleet) # each of the fleets
## 
## yr.range <- 2005:2011 # year range of the data - must be same, even if filled with NAs or 0s
## 
## # empty FLQuant for filling with right dimensions
## fq  <- FLQuant(dimnames = list(year = yr.range), quant = 'age')
## 
## ### Fleet level slots ###
## fleets <- FLFleet(lapply(fl.nam, function(Fl) {
## 
## # blank quants with the same dims
## eff <- cap <- crw <- cos.fl <- fq
## 
## # fleet effort
## eff[,ac(yr.range)] <- data$data[data$Fleet == Fl & data$type == 'effort']
## units(eff) <- '000 kw days'
## 
## ## Repeat for each fleet level variables (not shown) ##
## 
## ### Metier level slots ###
## met.nam  <- unique(data$Metier[data$Fleet == Fl]) # metiers for fleet
## met.nam  <- met.nam[!is.na(met.nam)] # exclude the fleet level data
## 
## metiers  <- FLMetiers(lapply(met.nam, function(met) {
## 
## # blank quants
## effmet <- cos.met <- fq
## 
## # effort share for metier
## effmet[,ac(yr.range)] <- data$data[data$Fleet == Fl & data$Metier & data$type == 'effshare']
## units(effmet)  <- NA
## 
## ## Repeat for each metier level variables (not shown) ##
## 
## 
## sp.nam <- unique(data$stock[data$Fleet == Fl & data$Metier == met]) # stocks caught by metier
## sp.nam <- sp.nam[!is.na(sp.nam)] # exclude fleet and metier level data
## 
## catch <- FLCatches(lapply(sp.nam, function(S){
## print(S)
## 
## # Quant dims may be specific per stock
## la.age <- FLQuant(dimnames = list(age = 1:7, year = yr.range, quant = 'age'))
## la.age[,ac(yr.range)] <- data$data[data$Fleet == Fl & data$Metier == met & data$Stock == S & data$type == 'landings.n']
## units(la.age) <- '1000'
## 
## ## Repeat for all stock level variables (not shown) ##
## 
## # Build F
## res <- FLCatch(range = yr.range, name = S, landings.n = la.age,...)
## 
## ## Compute any missing slots, e.g.
## res@landings <- computeLandings(res)
## 
## return(res) # return filled FLCatch
## 
## })) # End of FLCatches
## 
## # Fill an FLMetier with all the stock catches
## m <- FLMetier(catches = catch, name = met)
## m@effshare  <- effmet
## m@vcost <- vcost
## 
## 		 })) # end of FLMetiers
## 
## fl <- FLFleet(metiers = metiers, name = Fl, effort = ef,...) # fill with all variables
## return(fl)
## 
## 		 }))
## 
## names(fleets) <- fl.nam
## 

