## ----knitr_init, echo=FALSE, results="hide"------------------------------
library(knitr)
## Global options
opts_chunk$set(cache     =TRUE,
               cache.path="../cache/FLife/",
               echo      =FALSE,
               eval      =TRUE,
               prompt    =FALSE,
               comment   =NA,
               message   =FALSE,
               warning   =FALSE,
               tidy      =TRUE,
               fig.height=6,
               fig.width =8,
               fig.path  ="../tex/FLife/")

options(digits=3)


iFig=0

## ---- pkgs1, echo=TRUE---------------------------------------------------
library(ggplot2)
library(GGally)

## ---- pkgs2, echo=TRUE---------------------------------------------------
library(reshape)
library(plyr)

## ---- pkgs3, echo=TRUE---------------------------------------------------
library(FLCore)
library(ggplotFL)

library(FLBRP)
library(FLasher)

#library(FLAssess)

## ---- pkgs4, echo=TRUE---------------------------------------------------
library(popbio)

## ----install,echo=TRUE,eval=FALSE----------------------------------------
## install.packages("FLife", repos = "http://flr-project.org/R")

## ----lib,echo=TRUE-------------------------------------------------------
library(FLife)

## ----data,echo=TRUE------------------------------------------------------
data(teleost)

## ----data-2,echo=TRUE----------------------------------------------------
is(teleost)

## ----data-3,echo=TRUE----------------------------------------------------
vonB(1:10,teleost[,"Hucho hucho"])

## ---- length, echo=TRUE, fig.cap="Von Bertalanffy growth curves."--------
age=FLQuant(1:20,dimnames=list(age=1:20))
len=vonB(age,teleost)

ggplot(as.data.frame(len))+
  geom_line(aes(age,data,col=iter))+
  theme(legend.position="none")

## ---- pair-plot, fig.height=8,fig.cap="Relationship between life history parameters in the teleost dataset."----
habitat=ifelse(attributes(teleost)$habitat=="demersal","Demersal","Other")

my_smooth <- function(data,mapping,...){
  ggplot(data=data,mapping=mapping)+
  geom_point(...,size=.5)+
  geom_smooth(...,method="lm",se=FALSE)}

my_density <- function(data,mapping,...){
  ggplot(data=data,mapping=mapping)+
  geom_density(...,lwd=1,alpha=0.5)}

ggpairs(cbind(transform(model.frame(teleost)[,-c(7)],linf=log(linf),k=log(k),l50=log(l50)),
                  "habitat"=habitat),
  mapping = ggplot2::aes(color=habitat),
  lower = list(continuous = wrap(my_smooth)),
  diag=list(continuous=wrap(my_density,alpha=0.2)),
  upper = list(continuous = wrap("cor", size = 3, hjust=0.8)),
  title = "")+
  theme(legend.position ="none",
  panel.grid.major =element_blank(),
  axis.ticks       =element_blank(),
  axis.text.x      =element_blank(),
  axis.text.y      =element_blank(),
  panel.border     =element_rect(linetype = 1, colour="black", fill=NA))

## ----growth--------------------------------------------------------------
age=FLQuant(0:10,dimnames=list(age=0:10))

lenV=vonB(age,FLPar(linf=120,k=0.1,t0=-0.1))
lenR=richards(age,params=FLPar(linf=100,k=.4,b=.1,m=2))
lenG=gompertz(age,FLPar(linf=100,a=2,k=.4))

ggplot(as.data.frame(FLQuants(Gompertz=lenG,Richards=lenR,"Von Bertalanffy"=lenV)))+
  geom_line(aes(age,data,col=qname))

## ---- ogives, echo=TRUE--------------------------------------------------
dnormal( age,FLPar(a1=4,sl=2,sr=5000))
knife(   age,FLPar(a1=4))
logistic(age,FLPar(a50=4,ato95=1,asym=1.0))
sigmoid( age,FLPar(a50=4,ato95=1))

## ----ages----------------------------------------------------------------
data(ple4)
ages(m(ple4))

## ----wt2len--------------------------------------------------------------
wt2len(stock.wt(ple4),FLPar(a=0.0001,b=3))

## ---- echo=TRUE----------------------------------------------------------
par=lhPar(FLPar(linf=100))
par

## ------------------------------------------------------------------------
data(teleost)
teleost

## ----lh, eval=FALSE------------------------------------------------------
## library(glmnet)
## 
## t.=cbind(model.frame(teleost)[,-7],as.data.frame(attributes(teleost)[5:9]))
## 
## x=model.matrix(~order+genus+family+habitat,t.[,-1])
## y=log(as.matrix(t.[,"k"]))
## 
## fit=glmnet::cv.glmnet(x, y)
## plot(fit)
## 
## lmb=fit$lambda.1se
## coeffs=coef(fit, s="lambda.1se")
## 
## fit = glmnet(x, y)
## plot(fit, xvar = "lambda", label = TRUE)

## ----m-gislason----------------------------------------------------------
m=gislason(FLQuant(1:15,dimnames=list(age=1:15)),teleost)

ggplot(as.data.frame(m))+
    geom_line(aes(age,data,col=factor(iter)))+
    theme(legend.position="none")+
    scale_x_continuous(limits=c(0,15))

## ------------------------------------------------------------------------

## ------------------------------------------------------------------------

## ---- refs, echo=TRUE, eval=FALSE----------------------------------------
## data(ple4)
## rodFn=FLife:::rodFn
## refs(ple4)

## ---- eql, echo=TRUE, fig.cap="Age-vectors of growthm natural mortality, maturity and selection pattern"----
eql=lhEql(par)

ggplot(FLQuants(eql,"m","catch.sel","mat","catch.wt"))+
  geom_line(aes(age,data))+
  facet_wrap(~qname,scale="free")+
  scale_x_continuous(limits=c(0,15))

## ---- plot, fig.cap="Equilibrium curves and reference points."-----------
plot(eql)

## ------------------------------------------------------------------------
lhRef(par)

## ----fig2----------------------------------------------------------------
data(teleost)
#teleost=with(teleost,l50linf=l50/linf)
teleost=rbind(teleost,l50linf=teleost["l50"]/teleost["linf"])
dimnames(teleost)[[1]][7]="l50linf"

alb=FLPar(unlist(teleost[,"Thunnus alalunga",
        c("linf","k","t0","l50","a","b")]))
alb=lhPar(rbind(alb,FLPar(m1=0.15,m2=-0.288,s=0.75)))

## ----fig3,fig.cap="Stock recruitment relationships for a steepness of 0.75 and vigin biomass of 1000"----
srr=as(list("Beverton and Holt"     =lhEql(alb,sr="bevholt"),
            "Ricker"                =lhEql(alb,sr="ricker"),
            "Cushing"               =lhEql(alb,sr="cushing"),
            "Shepherd"              =lhEql(rbind(alb,FLPar(c=1.5)),sr="shepherd"),
            "Segmented \nRegression"=lhEql(alb,sr="segreg")),"FLBRPs")

srr=
  ldply(srr,function(x) {
  refpts(x)=refpts(x)["msy"]
  fbar(x)=FLQuant(seq(0,1,length.out=501))
  res=brp(x)
  subset(model.frame(FLQuants(res,"ssb","rec","catch"),drop=TRUE),ssb>=0)})

ggplot(melt(srr[,-5],id=c("year","ssb",".id")))+
  geom_vline(aes(xintercept=200))+
  geom_line(aes(ssb,value,col=.id))+
  theme_bw()+theme(legend.position="bottom")+
  scale_colour_manual("Stock Recruit \n Relationship",
                      values=c("red","green","yellow","blue","pink"))+
  xlab("Spawning Stock Biomass")+ylab("Recruits")

## ----fig4,fig.cap="Production curves, Yield v SSB, for a steepness of 0.75 and vigin biomass of 1000."----
ggplot(melt(srr[,-4],id=c("year","ssb",".id")))+
  geom_path(aes(ssb,value,col=.id))+
  theme_bw()+theme(legend.position="bottom")+
  scale_colour_manual("Stock Recruit \n Relationship",
                      values=c("red","green","yellow","blue","pink"))+
  xlab("Spawning Stock Biomass")+ylab("Yield")

## ----fig5,fig.height=3,fig.width=6,eval=FALSE----------------------------
## par=lhPar(teleost[c("linf","k","t0","l50","a","b")])
## 
## mGislason=function(length,params)
##    0.55*(length^-1.66)%*%(params["linf"]^1.44)%*%params["k"]
## ref=mdply(seq(dim(par)[2]),function(i,par) lhRef(par[,i],m=mGislason),par=par)
## 
## dat=ref[dimnames(ref[!is.na(ref[,"rc"]),])[[1]],]
## 
## pc=princomp(dat[-122,c("r","rc","lopt","sk")],
##             cor=TRUE,use="pairwise.complete.obs")
## gg=ggbiplot(pc, obs.scale=1, var.scale=1,
##                 ellipse=TRUE, ellipse.prob=.5, circle=FALSE,
##          groups=factor(family[-122,"what"]) )+
##   kobe:::theme_ms(12,legend.position="bottom")
## 
## gg=gg+geom_point(aes(xvar,yvar),data=gg$data[130,],size=3)
## 
## #gg$layers[[2]]=NULL
## #gg$layers[[2]]$mapping$colour=NULL
## #gg$layers[[3]]$mapping$colour=NULL
## gg+theme(legend.position="none")

## ------------------------------------------------------------------------
#alb["t0"]=-alb["t0"]
## Beverton and Holt recruitment
bh      =lhEql(alb,m=lorenzen)
refpts(bh)=refpts(bh)["msy",]
p=plot(bh)+
   theme_bw()+
   theme(legend.position="bottom")+
   scale_colour_manual("",values="red",label="MSY")

## ---- echo=TRUE, m-density-dependence,fig.cap="Density Dependence in M"----
data(teleost)
par=teleost[,"Hucho hucho"]
par=lhPar(par)
hutchen=lhEql(par)
 
scale=stock.n(hutchen)[,25]%*%stock.wt(hutchen)
scale=(stock.n(hutchen)%*%stock.wt(hutchen)%-%scale)%/%scale
 
m=mdd(stock.wt(hutchen),par=FLPar(m1=.2,m2=-0.288),scale,k=.5)   

ggplot(as.data.frame(m))+
   geom_line(aes(age,data,col=factor(year)))+
   theme(legend.position="none")+
    scale_x_continuous(limits=c(0,15))

## ---- echo=TRUE, Maturity-density-dependence,fig.cap="Density Dependence in M"----
scale=stock.n(hutchen)[,25]%*%stock.wt(hutchen)
scale=(stock.n(hutchen)%*%stock.wt(hutchen)%-%scale)%/%scale

mat=matdd(ages(scale),par,scale,k=.5)   
 
ggplot(as.data.frame(mat))+
    geom_line(aes(age,data,col=factor(year)))+
    theme(legend.position="none")+
    scale_x_continuous(limits=c(0,15))

## ----DD, eval=FALSE------------------------------------------------------
## NA

## ----fig6,fig.height=5,fig.width=6,eval=FALSE----------------------------
## NA

## ----fig9----------------------------------------------------------------
data(ple4)
res=rnoise(4,m(ple4)[1:8,ac(1980:2008)],burn=10,b=0.9,what="age")
ggplot()+
  geom_point(aes(year,age,size= data),
             data=subset(as.data.frame(res),data>0))+
  geom_point(aes(year,age,size=-data),
             data=subset(as.data.frame(res),data<=0),colour="red")+
  scale_size_area(max_size=4, guide="none")+
  facet_wrap(~iter)+theme_bw()

## ----fig10---------------------------------------------------------------
res=rnoise(4,m(ple4)[1:8,ac(1980:2008)],burn=10,b=0.9,what="cohort")
ggplot()+
  geom_point(aes(year,age,size= data),
             data=subset(as.data.frame(res),
                         data>0))+
  geom_point(aes(year,age,size=-data),
             data=subset(as.data.frame(res),
                         data<=0),colour="red")+
  scale_size_area(max_size=4,  guide="none")+
  facet_wrap(~iter)+theme_bw()  

## ----noise, fig.height=6,fig.width=10------------------------------------
alb  =teleost[c("linf","k","t0","l50","a","b"),"Thunnus alalunga"]
#alb  =lhPar(rbind(alb,FLPar(m1=0.3,m2=-0.288)))
#eq      =lhEql(alb,m=lorenzen,sr="bevholt")
eq      =lhEql(lhPar(alb))

fbar(eq)=FLQuant(rep(1,1001)*c(refpts(eq)["msy","harvest"]))
fbar(eq)=propagate(fbar(eq),3)
fbar(eq)[,,,,,1]=0.00001
fbar(eq)[,,,,,3]=fbar(eq)[,,,,,2]*2

stk =as(eq,"FLStock")

#scale =rnoise(1,iter(stock.wt(stk),1),sd=0.3,b=0.0,what="cohort")

# SRR
srDev =rlnorm(1,iter(rec(stk),1)*0,.3)
stkr=fwd(stk,fbar=fbar(stk)[,-1],sr=eq,residuals=srDev)

# M
#cv(ssb(stkf))/cv(ssb(stkm))

stkm     =stk
#m(stkm)  =mdd(stock.wt(stkm),alb,scale) 
m(stkm)  =rlnoise(1,log(iter(m(stk),1)),sd=0.3*0.95,b=0.9,what="year")
stkm     =fwd(stkm,fbar=fbar(stk)[,-1],sr=eq)

# Fecundity
stkf     =stk
#mat(stkf)=matdd(ages(iter(stock.wt(stkf),1)),alb,scale) 
mat(stkf)=rlnoise(1,log(iter(mat(stk),1)),sd=0.3,b=0.9,what="year")
stkf     =fwd(stkf,fbar=fbar(stk)[,-1],sr=eq)

## ----ssbNoise, fig.height=6,fig.width=10---------------------------------
dat =as.data.frame(FLQuants("SRR"       =ssb(stkr),
                             "M"        =ssb(stkm),
                             "Fecundity"=ssb(stkf)))
#dat=ddply(subset(dat,year>50),.(iter,qname), transform, val=data/mean(data))

dat$F=factor(c("0","Fmsy","2Fmsy")[dat$iter],levels=c("0","Fmsy","2Fmsy"))
names(dat)[8]="Process"

## ----ARnoise, fig.height=6,fig.width=10----------------------------------
data(teleost)
alb  =teleost[c("linf","k","t0","l50","a","b"),"Thunnus alalunga"]
#alb  =lhPar(rbind(alb,FLPar(m1=0.3,m2=-0.288)))
#eq      =lhEql(alb,m=lorenzen,sr="bevholt")
eq      =lhEql(lhPar(alb))

fbar(eq)=FLQuant(rep(1,1001)*c(refpts(eq)["msy","harvest"]))
fbar(eq)=propagate(fbar(eq),3)
fbar(eq)[,,,,,1]=0.00001
fbar(eq)[,,,,,3]=fbar(eq)[,,,,,2]*2

stk =as(eq,"FLStock")

#scale =rnoise(1,iter(stock.wt(stk),1),sd=0.3,b=0.0,what="cohort")

# SRR
srDev =rlnoise(1,iter(rec(stk),1)*0,.3,b=0.9)
stkr=fwd(stk,fbar=fbar(stk)[,-1],sr=eq,residuals=srDev)

# M
#cv(ssb(stkf))/cv(ssb(stkm))

stkm     =stk
#m(stkm)  =mdd(stock.wt(stkm),alb,scale) 
m(stkm)  =rlnoise(1,log(iter(m(stk),1)),sd=0.3*0.95,b=0.9,what="cohort")
stkm     =fwd(stkm,fbar=fbar(stk)[,-1],sr=eq)

# Fecundity
stkf     =stk
#mat(stkf)=matdd(ages(iter(stock.wt(stkf),1)),alb,scale) 
mat(stkf)=rlnoise(1,log(iter(mat(stk),1)),sd=0.3,b=0.9,what="cohort")
stkf     =fwd(stkf,fbar=fbar(stk)[,-1],sr=eq)

## ----ssbAR, fig.height=6,fig.width=10------------------------------------
dat2=as.data.frame(FLQuants("SRR"       =ssb(stkr),
                             "M"        =ssb(stkm),
                             "Fecundity"=ssb(stkf)))
#dat2=ddply(subset(dat2,year>50),.(iter,qname), transform, val=data/mean(data))

dat2$F=factor(c("0","Fmsy","2Fmsy")[dat2$iter],levels=c("0","Fmsy","2Fmsy"))
names(dat2)[8]="Process"

dat=rbind(cbind("AR"="0.0",dat),
          cbind("AR"="0.9",dat2))

ggplot(dat)+
  geom_line(aes(year,data,col=AR))+
  theme_bw()+
  theme(legend.position="bottom")+
  facet_grid(F~Process,scale="free_y")+
  scale_x_continuous(limits=c(500,700))+
  xlab("Time (year)")+ylab("Relative SSB")

## ----spectAR, fig.width=8,fig.height=6-----------------------------------
dat=ddply(dat,.(AR,Process,F), with, 
          as.data.frame(spectrum(data, log = "dB", ci = 0.8,plot=FALSE)[c("freq","spec")]))

dat=ddply(subset(dat,freq>0.05),.(AR,Process,F),transform,val=spec/max(spec))
ggplot(dat,aes(freq,val,col=AR))+
  geom_smooth(se=FALSE)+
  facet_grid(F~Process,scale="free_y")+
  theme_bw()+
  theme(legend.position="bottom")

## ----fig13, fig.width=10,fig.height=8,fig.cap="Cohort Effects"-----------
library(FLAssess)

stk   =iter(stkm[,101:250],1)
m(stk)=iter(m(stkr),1)[,101:250]
vpa    =stk+VPA(stk)

p=plot(FLStocks("Actual"=stk,"Model Estimate"=vpa))
p$data=subset(p$data,qname%in%c("SSB","Rec"))
p$data=ddply(p$data,.(qname), transform, val=`50%`/mean(`50%`,na.rm=TRUE))
ggplot(subset(p$data,year>50))+
  geom_line(aes(year,val,col=qname))+
  facet_grid(qname~stock,scale="free")+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Year")+ylab("")

## ------------------------------------------------------------------------
hcrSBT1=function(cpue,tac,k1=1.5,k2=3,gamma=1,nyrs=5,lag=1,interval=3){
  
     dat=as.data.frame(cpue[,ac(-((nyrs-1):0)+dims(cpue)$maxyear)])
     lambda=as.FLQuant(ddply(dat, .(iter), with,  data.frame(data=coefficients(lm(data~year))[2])))
     
     flag  =lambda<0
     lambda=abs(lambda)
  
     res=1+ifelse(flag,-k1,k2)*lambda^ifelse(flag,gamma,1)
     res=res%*%tac
     
     dmns=dimnames(tac)
     dmns$year=as.integer(dmns$year)+lag+seq(interval)-1
     dmns$iter=dimnames(cpue)$iter
     
     res=FLQuant(rep(c(res),each=interval),dimnames=dmns)
     
     return(res)}

## ----fig14, fig.width=6,fig.height=8,fig.cap="MSE using empirical HCR"----
par     =teleost[c("linf","k","t0","l50","a","b"),"Thunnus alalunga"]
par     =lhPar(rbind(par,FLPar(m1=0.3,m2=-0.288)))
eql     =lhEql(par,m=lorenzen)
fbar(eql)=FLQuant(seq(101)/50.5*c(refpts(eql)["msy","harvest"]))

om=as(eql,"FLStock")
om=fwd(om,fbar=fbar(eql)[,-1],sr=eql)
om=fwdWindow(om,end=131,eql)

set.seed(1235)  
srDev=rlnorm(100,FLQuant(rep(0,131)),0.3)
uDev =rlnorm(100,FLQuant(rep(0,131)),0.3)

#mseSBT1<-function(om,eql,srDev,uDev,
                  
                  ##hcr params
                  k1=1.5;k2=3.0;gamma=1;nyrs=5;lag=1
                  
                  #years over which to run MSE
                  start=range(om)["maxyear"]-30;interval=3;end=range(om)["maxyear"]-interval
                  
                  monitor=TRUE
 #                 ){
  
  ## Get number of iterations in OM
  #if (dims(uDev)$iter!=dims(srDev)$iter) 
  #  stop("Iters in srDev and uDev must match")
  #nits=c(om=dims(om)$iter, sr=dims(params(eql))$iter, rsdl=dims(srDev)$iter)
  
  #if (length(unique(nits))>=2 & !(1 %in% nits)) 
  #  ("Stop, iters not '1 or n' in OM")
  #nits=max(nits)
  nits=100
  
  stock(om)=propagate(computeStock(om),nits)
  
  #### Observation Error (OEM) setup #######################
  ## Random variation for CPUE
  cpue=window(computeStock(om),end=start)%*%window(uDev,end=start)

  mn=apply(cpue,6,mean,na.rm=TRUE)
  sd=apply(cpue,6,sd,  na.rm=TRUE)
  
  ## capacity can not be twice what was seen historically
  effort_max=max(fbar(window(om,end=start)))*2
  
  ## Loop round years
  tac=catch(om)[,ac(start-1)]
  for (iYr in seq(start,end,interval)){
    #if (monitor) cat('iYr, ')
    
    cpue=window(cpue,end=iYr)
    cpue[,ac(iYr-(interval:1)+1)]=stock(om)[,ac(iYr-(interval:1)+1)]*uDev[,ac(iYr-(interval:1)+1)]
        
    tac=hcrSBT1((cpue%/%mn),tac,k1,k2,gamma,interval=interval)
    
    om <-fwd(om,catch=tac,sr=eql,residuals=srDev,effort_max=effort_max)
    tac=tac[,interval]}
  
  #return(window(om,end=end))}


ggplot()+
    geom_line(aes(year,data,col=iter),
              data=as.data.frame(FLQuants(iter(om[,ac(95:120)],5:7),c("Rec"=rec,"SSB"=ssb,
                                                      "Catch"=catch,"Harvest"=fbar)),drop=T))+
    facet_grid(qname~.,scale="free")+
    theme_bw()+xlab("")+ylab("")+
  theme(legend.position="bottom")

## ------------------------------------------------------------------------
data(ple4)
ctc=as.data.frame(catch.n(ple4))
dat=cc(age=ctc$age,n=ctc$data)
head(dat)

## ------------------------------------------------------------------------
data(cas)
pw=ddply(subset(cas), .(year), 
   function(cas) powh(cas$len,cas$n)$data)
   
   pw=transform(pw, lustrum=(year%/%5)*5,
         yr    =year-(year%/%5)*5,
         weight=ifelse(len>=100&len<=200,1,0))
         
ggplot(pw)+
   geom_line(aes(len,diff,colour=factor(yr),group=year))+
   scale_x_continuous(limits=c(0,300)) +
   xlab("Length (cm)")+
   ylab("Difference between Length and Mean Size")+
   geom_smooth(aes(len,diff,weight=weight),
   method="lm",col="red",size=1.25,alpha=.1)+
   theme_bw()+theme(legend.position="none")

## ---- echo=TRUE----------------------------------------------------------
data(ple4)
ctc=as.data.frame(catch.n(ple4))
ctc=ddply(ctc,.(year), with, cc(age=age,n=data))
ctc=ddply(transform(ctc,decade=factor(10*(year%/%10))),.(decade,age),with,data.frame(sel=mean(sel)))
ggplot(ctc)+
  geom_line(aes(age,sel,colour=decade))

## ---- devtools, echo=TRUE, eval=FALSE------------------------------------
## 	library(devtools)
## 	install_github("lauriekell/FLife")

