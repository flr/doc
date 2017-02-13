---
title: Reading data into FLR 
date: "13 February, 2017"
output:
  github_document
tags:
license: Creative Commons CC-BY SA
---
 


This tutorial details methods for reading various formats of data into R for generating the FLStock() object class.

## Required packages


Youen !!!

To follow this tutorial you should have installed the following packages:

- CRAN: [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html)
- FLR: [FLCore](http://www.flr-project.org/FLCore/), [ggplotFL](http://www.flr-project.org/ggplotFL/)

You can do so as follows,


```r
install.packages(c("ggplot2"))
install.packages(c("ggplotFL"), repos="http://flr-project.org/R")
```


```r
# This chunk loads all necessary packages, trims pkg messages
library(FLCore)
library(ggplotFL)
```

# SECTION

PD

## SubSECTION : Reading files (csv, dat, ...) - YV

## Reading common fisheries data formats 

FLCore contains functions for reading in fish stock data in common pre-defined formats. To read a single variable (e.g. numbers-at-age, maturity-at-age) from the **Lowestoft VPA** format you use the `readVPA` function. The following example reads the catch numbers-at-age for herring:


```r
catch.n <- readVPAFile(file.path('src','Data','her-irlw',"canum.txt"))
class(catch.n)
```

```
## [1] "FLQuant"
## attr(,"package")
## [1] "FLCore"
```
This can be repeated for each of the data files. 

Alternatively, if you have the full information for a stock in the **Lowestoft VPA**, **Adapt**, **CSA** or **ICA** format you can read in together using the `readFLStock` function. Here, you point the function to the index file, with all other files in the same directory:


```r
her <- readFLStock(file.path('src','Data','her-irlw','index.txt'))
class(her)
```

```
## [1] "FLStock"
## attr(,"package")
## [1] "FLCore"
```
Which we can see correctly formats the data as an `FLStock` object.


```r
summary(her)
```

```
## An object of class "FLStock"
## 
## Name: Herring VIa(S) VIIbc  
## Description: Imported from a VPA file. ( src/Data/her-irl [...] 
## Quant: age 
## Dims:  age 	year	unit	season	area	iter
## 	7	55	1	1	1	1	
## 
## Range:  min	max	pgroup	minyear	maxyear	minfbar	maxfbar 
## 	1	7	NA	1957	2011	1	7	
## 
## catch         : [ 1 55 1 1 1 1 ], units =  NA 
## catch.n       : [ 7 55 1 1 1 1 ], units =  NA 
## catch.wt      : [ 7 55 1 1 1 1 ], units =  NA 
## discards      : [ 1 55 1 1 1 1 ], units =  NA 
## discards.n    : [ 7 55 1 1 1 1 ], units =  NA 
## discards.wt   : [ 7 55 1 1 1 1 ], units =  NA 
## landings      : [ 1 55 1 1 1 1 ], units =  NA 
## landings.n    : [ 7 55 1 1 1 1 ], units =  NA 
## landings.wt   : [ 7 55 1 1 1 1 ], units =  NA 
## stock         : [ 1 55 1 1 1 1 ], units =  NA 
## stock.n       : [ 7 55 1 1 1 1 ], units =  NA 
## stock.wt      : [ 7 55 1 1 1 1 ], units =  NA 
## m             : [ 7 55 1 1 1 1 ], units =  NA 
## mat           : [ 7 55 1 1 1 1 ], units =  NA 
## harvest       : [ 7 55 1 1 1 1 ], units =  f 
## harvest.spwn  : [ 7 55 1 1 1 1 ], units =  NA 
## m.spwn        : [ 7 55 1 1 1 1 ], units =  NA
```

However, this object only contains the input data for the stock assessment, not any estimated values (e.g. harvest rates, stock abundances). You can add these to the object as follows:


```r
her@stock.n <- readVPAFile(file.path('src','Data','her-irlw',"n.txt"))
```
Note that the units for the harvest slot have not been set. We will deal with this in the section below.



## SUbsection : Reshaping data as a matrix - YV

## Subection :  Making an FLQuant object - PD

## Subsection : Description, units, ranges etc.. - PD

# References


# More information

* You can submit bug reports, questions or suggestions on this tutorial at <https://github.com/flr/doc/issues>.
* Or send a pull request to <https://github.com/flr/doc/>
* For more information on the FLR Project for Quantitative Fisheries Science in R, visit the FLR webpage, <http://flr-project.org>.

## Software Versions

* R version 3.3.1 (2016-06-21)
* FLCore: 2.6.0.20170130
* ggplotFL: 2.5.9.9000
* ggplot2: 2.1.0
* **Compiled**: Mon Feb 13 15:25:11 2017

## License

This document is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International](https://creativecommons.org/licenses/by-sa/4.0) license.

## Author information

**Iago MOSQUEIRA**. European Commission Joint Research Centre (JRC), Institute for the Protection and Security of the Citizen (IPSC), Maritime Affairs Unit, Via E. Fermi 2749, 21027 Ispra VA, Italy. <https://ec.europa.eu/jrc/>
