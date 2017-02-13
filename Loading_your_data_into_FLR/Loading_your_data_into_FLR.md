---
title: Reading data into FLR 
date: "13 February, 2017"
output:
  github_document
tags:
license: Creative Commons CC-BY SA
---
 


This tutorial details methods for reading various formats of data into R for generating the `FLStock` object class.

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

# FLStock objects 

PD

## SubSECTION : Reading files (csv, dat, ...) - YV

Fisheries data are generally stored in different format (cvs, excel, SAS...). R provides tools to read and import data from simple text files to more advanced SAS files or databases.
https://www.datacamp.com/community/tutorials/importing-data-r-part-two#gs.kNzBd5k is a nice tutorial to quickly import data into R.

Your data are stored in a folder in your computer or a server. You have to tell R what is the path to the data.
You can check the working directory already active in your R session using the command getwd(). 
To set the working directory use setwd("directory name"). Case is important, use // or \ for separating folders and directories in Windows.


This tutorial will give some exemples but regardless the format, the different steps are:
- Finding the right function to import data into R
- Reshaping the data as a matix 
- creating an FLQuant objetc

### importing CSV files

There is many ways of reading csv files. 
read.table with 'header', 'sep', 'dec' and 'row.names' options will allow you reading all .csv and .txt files

The 


```r
# Read this in using read.table() with default options

catch.n <- read.csv("src/Data/catch_numbers.csv",row=1)

# We have read in the data as a data.frame
class(catch.n)
```

```
## [1] "data.frame"
```

```r
# There is an FLQuant contructor that uses a data.frame, but here our data.frame is not set up the right way
# Instead we can convert the object to a matrix
catch.n.matrix <- as.matrix(catch.n)
catch.n.matrix
```

```
##   X1957 X1958 X1959 X1960 X1961 X1962 X1963 X1964
## 1     0   100  1060   516  1768   259   132    88
## 2  7709  3349  7251 18221  7129  7170  6446  7030
## 3  9965  9410  3585  7373 14342  5535  5929  5903
## 4  1394  6130  8642  3551  6598 10427  2032  4048
## 5  6235  4065  3222  2284  2481  5235  3192  2195
## 6  2062  5584  1757   770  2392  3322  3541  3972
## 7  1720  6666  3699  1924  1659  7289  5889  9168
##   X1965 X1966 X1967 X1968 X1969 X1970 X1971 X1972
## 1   234     0     0   574  1495   135   883  1001
## 2  3847 16809  1232 10192 15038 35114  6177 28786
## 3 10135 11894 55013  4702 13013 26007  7038 20534
## 4  9008 10319 12681 78638  4410 13243 10856  6191
## 5  2426  7392  9071  5316 54809  3895  8826 11145
## 6  2019  3356  6348  4534  4918 40181  3938 10057
## 7 13362 16208 16482  6068  8324  6560 44999 55730
##   X1973 X1974 X1975 X1976 X1977 X1978 X1979 X1980
## 1  6423  3374  7360 16613  4485 10170  5919  2856
## 2 40390 29406 41308 29011 44512 40320 50071 40058
## 3 47389 41116 25117 37512 13396 27079 19161 64946
## 4 16863 44579 29192 26544 17176 13308 19969 25140
## 5  7432 17857 23718 25317 12209 10685  9349 22126
## 6 12383  8882 10703 15000  9924  5356  8422  7748
## 7 62140 51722 47316 24507 11044 11232 13956 16624
##   X1981 X1982 X1983 X1984 X1985 X1986 X1987 X1988
## 1  1620   748  1517  2794  9606   918 12149     0
## 2 22265 18136 43688 81481 15143 27110 44160 29135
## 3 41794 17004 49534 28660 67355 27818 80213 46300
## 4 31460 28220 25316 17854 12756 66383 41504 41008
## 5 12812 18280 31782  7190 11241 14644 99222 23381
## 6 12746  8121 18320 12836  7638  7988 15226 45692
## 7 11416 10213 14275 12002 18940 13245 28908 11392
##   X1989  X1990  X1991 X1992 X1993 X1994 X1995
## 1  2241    878    675  2592   191 11709   284
## 2  6919  24977  34437 15519 20562 56156 34471
## 3 78842  19500  27810 42532 22666 31225 35414
## 4 26149 151978  12420 26839 41967 16877 18617
## 5 21481  24362 100444 12565 23379 21772 19133
## 6 15008  20164  17921 73307 13547 13644 16081
## 7 32166  25628  33836 23024 80949 50419 28549
##   X1996 X1997 X1998 X1999 X2000 X2001 X2002 X2003
## 1  4776  7458  7437  2392  4101  2316  4058  1731
## 2 24424 56329 72777 51254 34564 21717 32640 32819
## 3 69307 25946 80612 61329 38925 21780 37749 28714
## 4 31128 38742 38326 34901 30706 17533 18882 24189
## 5  9842 14583 30165 10092 13345 18450 11623  9432
## 6 15314  5977  9138  5887  2735  9953 10215  5176
## 7 27093 16033 11658  3915  3756  3276  4996  3751
##   X2004 X2005 X2006 X2007 X2008 X2009 X2010 X2011
## 1  1401   209   598    76   483   202  1271   121
## 2 15122 28123 22036 24577 12265 12574 13507 14207
## 3 32992 30896 36700 43958 19661 12077 20127  9315
## 4 19720 26887 30581 23399 28483 12096  6541  9114
## 5  9006 10774 21956 13738 11110 12574  7588  3386
## 6  4924  5452  9080  5474  5989  5239  6780  3780
## 7  2845  2449  3619  2187  3750  2910  3413  3946
```

```r
# We need to specify the dimnames
catch.n.flq <- FLQuant(catch.n.matrix, dimnames=list(age=1:7, year = 1957:2011))
catch.n.flq
```

```
## An object of class "FLQuant"
## , , unit = unique, season = all, area = unique
## 
##    year
## age 1957   1958   1959   1960   1961   1962  
##   1      0    100   1060    516   1768    259
##   2   7709   3349   7251  18221   7129   7170
##   3   9965   9410   3585   7373  14342   5535
##   4   1394   6130   8642   3551   6598  10427
##   5   6235   4065   3222   2284   2481   5235
##   6   2062   5584   1757    770   2392   3322
##   7   1720   6666   3699   1924   1659   7289
##    year
## age 1963   1964   1965   1966   1967   1968  
##   1    132     88    234      0      0    574
##   2   6446   7030   3847  16809   1232  10192
##   3   5929   5903  10135  11894  55013   4702
##   4   2032   4048   9008  10319  12681  78638
##   5   3192   2195   2426   7392   9071   5316
##   6   3541   3972   2019   3356   6348   4534
##   7   5889   9168  13362  16208  16482   6068
##    year
## age 1969   1970   1971   1972   1973   1974  
##   1   1495    135    883   1001   6423   3374
##   2  15038  35114   6177  28786  40390  29406
##   3  13013  26007   7038  20534  47389  41116
##   4   4410  13243  10856   6191  16863  44579
##   5  54809   3895   8826  11145   7432  17857
##   6   4918  40181   3938  10057  12383   8882
##   7   8324   6560  44999  55730  62140  51722
##    year
## age 1975   1976   1977   1978   1979   1980  
##   1   7360  16613   4485  10170   5919   2856
##   2  41308  29011  44512  40320  50071  40058
##   3  25117  37512  13396  27079  19161  64946
##   4  29192  26544  17176  13308  19969  25140
##   5  23718  25317  12209  10685   9349  22126
##   6  10703  15000   9924   5356   8422   7748
##   7  47316  24507  11044  11232  13956  16624
##    year
## age 1981   1982   1983   1984   1985   1986  
##   1   1620    748   1517   2794   9606    918
##   2  22265  18136  43688  81481  15143  27110
##   3  41794  17004  49534  28660  67355  27818
##   4  31460  28220  25316  17854  12756  66383
##   5  12812  18280  31782   7190  11241  14644
##   6  12746   8121  18320  12836   7638   7988
##   7  11416  10213  14275  12002  18940  13245
##    year
## age 1987   1988   1989   1990   1991   1992  
##   1  12149      0   2241    878    675   2592
##   2  44160  29135   6919  24977  34437  15519
##   3  80213  46300  78842  19500  27810  42532
##   4  41504  41008  26149 151978  12420  26839
##   5  99222  23381  21481  24362 100444  12565
##   6  15226  45692  15008  20164  17921  73307
##   7  28908  11392  32166  25628  33836  23024
##    year
## age 1993   1994   1995   1996   1997   1998  
##   1    191  11709    284   4776   7458   7437
##   2  20562  56156  34471  24424  56329  72777
##   3  22666  31225  35414  69307  25946  80612
##   4  41967  16877  18617  31128  38742  38326
##   5  23379  21772  19133   9842  14583  30165
##   6  13547  13644  16081  15314   5977   9138
##   7  80949  50419  28549  27093  16033  11658
##    year
## age 1999   2000   2001   2002   2003   2004  
##   1   2392   4101   2316   4058   1731   1401
##   2  51254  34564  21717  32640  32819  15122
##   3  61329  38925  21780  37749  28714  32992
##   4  34901  30706  17533  18882  24189  19720
##   5  10092  13345  18450  11623   9432   9006
##   6   5887   2735   9953  10215   5176   4924
##   7   3915   3756   3276   4996   3751   2845
##    year
## age 2005   2006   2007   2008   2009   2010  
##   1    209    598     76    483    202   1271
##   2  28123  22036  24577  12265  12574  13507
##   3  30896  36700  43958  19661  12077  20127
##   4  26887  30581  23399  28483  12096   6541
##   5  10774  21956  13738  11110  12574   7588
##   6   5452   9080   5474   5989   5239   6780
##   7   2449   3619   2187   3750   2910   3413
##    year
## age 2011  
##   1    121
##   2  14207
##   3   9315
##   4   9114
##   5   3386
##   6   3780
##   7   3946
## 
## units:  NA
```
## SUbsection : Reshaping data as a matrix - YV

## Subection :  Making an FLQuant object - PD



## Reading common fisheries data formats 

FLCore contains functions for reading in fish stock data in commonly used formats. To read a single variable (e.g. numbers-at-age, maturity-at-age) from the **Lowestoft VPA** format you use the `readVPA` function. The following example reads the catch numbers-at-age for herring:


```r
# Read from a VPA text file
catch.n <- readVPAFile(file.path('src','Data','her-irlw',"canum.txt"))
class(catch.n)
```

```
## [1] "FLQuant"
## attr(,"package")
## [1] "FLCore"
```
This can be repeated for each of the data files. In addition, functions are available for Multifan-CL format  `readMFCL` and ADMB `readADMB`.

Alternatively, if you have the full information for a stock in the **Lowestoft VPA**, **Adapt**, **CSA** or **ICA** format you can read in together using the `readFLStock` function. Here, you point the function to the index file, with all other files in the same directory:


```r
# Read a collection of VPA files, pointing to the Index file:
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
Note: the units for the slots have not been set. We will deal with this in the next section.

In addition, this object only contains the input data for the stock assessment, not any estimated values (e.g. harvest rates, stock abundances). You can add these to the object as follows:


```r
her@stock.n <- readVPAFile(file.path('src','Data','her-irlw',"n.txt"))
print(her@stock.n[,ac(2005:2011)]) # only print 2005:2011
```

```
## An object of class "FLQuant"
## , , unit = unique, season = all, area = unique
## 
##    year
## age 2005     2006     2007     2008     2009    
##   1 516917.8 339465.2 174571.1 282187.1 256537.9
##   2 179953.1 190041.8 124606.8  64089.7 103602.4
##   3 115639.1 109316.3 113657.7  75691.6  39075.8
##   4  68903.3  66928.1  55794.7  60037.5  40312.1
##   5  34519.1  36892.1  33210.4  28921.5  31447.1
##   6  15211.7  21023.5  17193.0  16241.9  14308.2
##   7   6833.0   8379.3   5355.8   9315.2   8255.6
##    year
## age 2010     2011    
##   1 500771.9 473853.8
##   2  94215.4 183911.3
##   3  65137.7  59210.2
##   4  22271.7  37090.3
##   5  23016.5  12700.7
##   6  17112.1  12507.7
##   7   9662.4  16579.1
## 
## units:  NA
```
Now we have a fully filled `FLStock` object. But let's check the data are consistent.


```r
# The sum of products (SOP)
apply(her@landings.n * her@landings.wt, 2, sum)[,ac(2005:2011)]
```

```
## An object of class "FLQuant"
## , , unit = unique, season = all, area = unique
## 
##      year
## age   2005    2006    2007    2008    2009   
##   all 16252.0 19172.0 17790.6 13340.9 10482.3
##      year
## age   2010    2011   
##   all 10232.6  6921.2
## 
## units:  NA
```

```r
# and the value read in from the VPA file
her@landings[,ac(2005:2011)]
```

```
## An object of class "FLQuant"
## , , unit = unique, season = all, area = unique
## 
##      year
## age   2005  2006  2007  2008  2009  2010  2011 
##   all 16231 19193 17791 13340 10468 10241  6919
## 
## units:  NA
```

```r
## They are not the same!!  We correct the landings to be the same as the SOP - there is a handy function for this purpose
her@landings <- computeLandings(her)

# In addition, there is no discard information
her@discards.wt[,ac(2005:2011)]
```

```
## An object of class "FLQuant"
## , , unit = unique, season = all, area = unique
## 
##    year
## age 2005 2006 2007 2008 2009 2010 2011
##   1 NA   NA   NA   NA   NA   NA   NA  
##   2 NA   NA   NA   NA   NA   NA   NA  
##   3 NA   NA   NA   NA   NA   NA   NA  
##   4 NA   NA   NA   NA   NA   NA   NA  
##   5 NA   NA   NA   NA   NA   NA   NA  
##   6 NA   NA   NA   NA   NA   NA   NA  
##   7 NA   NA   NA   NA   NA   NA   NA  
## 
## units:  NA
```

```r
her@discards.n[,ac(2005:2011)]
```

```
## An object of class "FLQuant"
## , , unit = unique, season = all, area = unique
## 
##    year
## age 2005 2006 2007 2008 2009 2010 2011
##   1 NA   NA   NA   NA   NA   NA   NA  
##   2 NA   NA   NA   NA   NA   NA   NA  
##   3 NA   NA   NA   NA   NA   NA   NA  
##   4 NA   NA   NA   NA   NA   NA   NA  
##   5 NA   NA   NA   NA   NA   NA   NA  
##   6 NA   NA   NA   NA   NA   NA   NA  
##   7 NA   NA   NA   NA   NA   NA   NA  
## 
## units:  NA
```

```r
# Set up the discards and catches
her@discards.wt   <- her@landings.wt
her@discards.n[]  <- 0
her@discards      <- computeDiscards(her)
her@catch         <- her@landings
her@catch.wt      <- her@landings.wt
her@catch.n       <- her@landings.n
```
Functions are available to `computeLandings`, `computeDiscards`, `computeCatch` and `computeStock`. These functions take the argument `slot = 'catch'`, `slot = 'wt'` and `slot = 'n'` to compute the total weight, individual weight and numbers respectively, in addition to `slot = 'all'`. 





## Subsection : Description, units, ranges etc.. - PD

Before we are finished, we want to ensure the units and range references are correct. This is important 

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
* **Compiled**: Mon Feb 13 16:16:35 2017

## License

This document is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International](https://creativecommons.org/licenses/by-sa/4.0) license.

## Author information

**Iago MOSQUEIRA**. European Commission Joint Research Centre (JRC), Institute for the Protection and Security of the Citizen (IPSC), Maritime Affairs Unit, Via E. Fermi 2749, 21027 Ispra VA, Italy. <https://ec.europa.eu/jrc/>
