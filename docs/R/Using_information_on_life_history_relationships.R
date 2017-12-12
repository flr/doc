## ----knitr_init, echo=FALSE, results="hide"------------------------------
library(knitr)
## Global options
opts_chunk$set(cache     =TRUE,
               echo      =!TRUE,
               eval      =TRUE,
               prompt    =FALSE,
               comment   =NA,
               message   =FALSE,
               warning   =FALSE,
               tidy      =FALSE,
               fig.height=6,
               fig.width =8)

iFig=0

## ---- pkgs, echo=FALSE, message=FALSE------------------------------------
library(ggplot2)
library(FLife)
library(plyr)

theme_set(theme_bw())
options(digits=3)

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

## ---- echo=TRUE, fig.cap="Von Bertalanffy growth curves."----------------
age=FLQuant(1:20,dimnames=list(age=1:20))
len=vonB(age,teleost)

ggplot(as.data.frame(len))+
  geom_line(aes(age,data,col=iter))+
  theme(legend.position="none")

## ---- fig.height=8,fig.cap="Relationship between life history parameters in the teleost dataset."----
library(GGally)

habitat=ifelse(attributes(teleost)$habitat=="demersal","Demersal","Other")

my_smooth <- function(data,mapping,...){
  ggplot(data=data,mapping=mapping)+
  geom_point(...,size=.5)+
  geom_smooth(...,method="lm",se=FALSE)}

my_density <- function(data,mapping,...){
  ggplot(data=data,mapping=mapping)+
  geom_density(...,lwd=1)}

ggpairs(cbind(transform(model.frame(teleost)[,-c(7)],linf=log(linf),k=log(k),l50=log(l50)),
                  "habitat"=habitat),
  mapping = ggplot2::aes(color=habitat),
  lower = list(continuous = wrap(my_smooth)),
  diag=list(continuous=wrap(my_density,alpha=0.2)),
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

## ---- echo=TRUE,eval=FALSE-----------------------------------------------
## dnormal( age,FLPar(a1=4,sl=2,sr=5000))
## knife(   age,FLPar(a1=4))
## logistic(age,FLPar(a50=4,ato95=1,asym=1.0))
## sigmoid( age,FLPar(a50=4,ato95=1))

## ----ages,eval=FALSE-----------------------------------------------------
## data(ple4)
## ages(m(ple4))

## ----wt2len,eval=FALSE---------------------------------------------------
## wt2len(stock.wt(ple4),FLPar(a=0.0001,b=3))

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
## y=log(as.matrix(t.[,"k"])
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

## ---- echo=TRUE----------------------------------------------------------
library(FLBRP)
data(ple4)
refs(ple4)

## ---- echo=TRUE, fig.cap="Age-vectors of growthm natural mortality, maturity and selection pattern"----
library(FLBRP)
eql=lhEql(par)

ggplot(FLQuants(eql,"m","catch.sel","mat","catch.wt"))+
  geom_line(aes(age,data))+
  facet_wrap(~qname,scale="free")+
  scale_x_continuous(limits=c(0,15))

## ---- fig.cap="Equilibrium curves and reference points."-----------------
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
library(reshape)

srr=FLBRPs("Beverton and Holt"     =lhEql(alb,sr="bevholt"),
           "Ricker"                =lhEql(alb,sr="ricker"),
           "Cushing"               =lhEql(alb,sr="cushing"),
           "Shepherd"              =lhEql(rbind(alb,FLPar(c=1.5)),sr="shepherd"),
           "Segmented \nRegression"=lhEql(alb,sr="segreg"))

srr=
  ldply(srr,function(x) {
  refpts(x)=refpts(x)["msy"]
  fbar(x)=seq(0,1,length.out=501)
  res=brp(x)
  subset(model.frame(FLQuants(res,"ssb","rec","catch"),drop=TRUE),ssb>=0)})

ggplot(melt(srr[,-5],id=c("year","ssb",".id")))+
  geom_vline(aes(xintercept=200))+
  geom_line(aes(ssb,value,col=.id))+
  theme_bw()+theme(legend.position="bottom")+
  scale_colour_manual("Stock Recruit \n Relationship",
                      values=c("red","green","yellow","blue","pink"))+
  xlab("Spawning Stock Biomass")+ylab("Recruits")
i=0

## ----fig4,fig.cap="Production curves, Yield v SSB, for a steepness of 0.75 and vigin biomass of 1000."----
ggplot(melt(srr[,-4],id=c("year","ssb",".id")))+
  geom_path(aes(ssb,value,col=.id))+
  theme_bw()+theme(legend.position="bottom")+
  scale_colour_manual("Stock Recruit \n Relationship",
                      values=c("red","green","yellow","blue","pink"))+
  xlab("Spawning Stock Biomass")+ylab("Yield")

## ----fig5,eval=!TRUE,fig.height=3,fig.width=6----------------------------
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

## ----eval=FALSE----------------------------------------------------------
## #alb["t0"]=-alb["t0"]
## ## Beverton and Holt recruitment
## bh      =lhEql(alb,m=lorenzen)
## refpts(bh)=refpts(bh)["msy",]
## p=plot(bh)+theme_bw()+theme(legend.position="bottom")+
##   scale_colour_manual("",values="red",label="MSY")

## ---- echo=TRUE, m-density-dependence,fig.cap="Density Dependence in M"----
library(FLBRP)
library(FLife)

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

## ----fig11, fig.height=6,fig.width=10,eval=FALSE-------------------------
## k   =1
## sr  ="cushing"
## s   =0.7
## alb  =teleost[c("linf","k","t0","l50","a","b"),"Thunnus alalunga"]
## alb  =lhPar(rbind(alb,FLPar(m1=0.3,m2=-0.288,s=s)))
## eq      =lhEql(alb,m=lorenzen,sr=sr)
## fbar(eq)=FLQuant(rep(1,1001)*refpts(eq)["msy","harvest"])
## f       =fbar(eq)[,-1]
## f       =propagate(fbar(eq)[,-1],3)
## f[,,,,,2]=0
## f[,,,,,3]=f[,,,,,1]*4
## 
## stkr=as(eq,"FLStock")
## stkr=fwd(stkr,f=f,sr=eq)
## ref =stock.n(eq)[,1]
## 
## scale=rnoise(1,stock.wt(stkr),sd=0.3,b=0.9,what="cohort")
## stkm=stkr
## 
## m(  stkm)=mdd(iter(stock.wt(stkm),1),alb,scale,k)
## stkm=fwd(stkm,f=f,sr=eq)
## 
## scale=rnoise(1,stock.wt(stkm),sd=0.3,b=0.9,what="cohort")
## stkf=stkr
## mat(stkf)=matdd(ages(stock.wt(stkf)),alb,scale,k,TRUE)
## stkf=fwd(stkf,f=f,sr=eq)
## stkr=fwd(stkr,f=f,sr=eq,sr.residuals=rlnorm(1,iter(f,1)*0,.3))
## dat =as.data.frame(FLQuants("SRR"=ssb(stkr),
##                               "M"=ssb(stkm),
##                               "Fecundity"=ssb(stkf)))
## fishMat=ddply(subset(dat,year>50),.(iter,qname), transform, val=data/mean(data))
## 
## k   =1
## sr  ="cushing"
## s   =0.7
## alb  =FLPar(unlist(teleost[teleost$species=="Thunnus alalunga",
##              c("linf","k","t0","l50","a","b")]))
## alb  =lhPar(rbind(alb,FLPar(m1=0.3,m2=-0.288,s=s)))
## alb[c("a1","sr")]=c(0,5000)
## eq      =lhEql(alb,m=lorenzen,sr=sr)
## fbar(eq)=FLQuant(rep(1,1001)*refpts(eq)["msy","harvest"])
## f       =fbar(eq)[,-1]
## f       =propagate(fbar(eq)[,-1],3)
## f[,,,,,2]=0
## f[,,,,,3]=f[,,,,,1]*4
## 
## stkr=as(eq,"FLStock")
## stkr=fwd(stkr,f=f,sr=eq)
## ref =stock.n(eq)[,1]
## 
## scale=rnoise(1,stock.wt(stkr),sd=0.3,b=0.9,what="cohort")
## 
## stkm=stkr
## m(  stkm)=mdd(iter(stock.wt(stkm),1),alb,scale,k)
## stkm=fwd(stkm,f=f,sr=eq)
## scale=rnoise(1,stock.wt(stkm),sd=0.3,b=0.9,what="cohort")
## stkf=stkr
## mat(stkf)=matdd(ages(stock.wt(stkf)),alb,scale,k,TRUE)
## stkf=fwd(stkf,f=f,sr=eq)
## stkr=fwd(stkr,f=f,sr=eq,sr.residuals=rlnorm(1,iter(f,1)*0,.3))
## dat =as.data.frame(FLQuants("SRR"=ssb(stkr),
##                             "M"=ssb(stkm),
##                             "Fecundity"=ssb(stkf)))
## fishJuv=ddply(subset(dat,year>50),.(iter,qname), transform, val=data/mean(data))
## 
## dat=rbind(cbind("Selection"="Juvenile",fishJuv),
##           cbind("Selection"="Mature",  fishMat))
## dat=transform(dat,F=factor(paste("F times",c(0,1,3))[iter]))
## 
## ggplot(dat)+
##   geom_line(aes(year,val,col=qname))+
##   theme_bw()+
##   theme(legend.position="bottom")+
##   facet_grid(F~Selection,scale="free_y")+
##   scale_x_continuous(limits=c(500,700))+
##   xlab("Time (year)")+ylab("")

## ----fig12,fig.width=6,fig.height=6,eval=FALSE,fig.cap="Year effects"----
## dat=rbind(cbind("Selection"="Juvenile",fishJuv),
##           cbind("Selection"="Mature",  fishMat))
## dat=transform(dat,F=factor(paste("F times",c(0,1,3))[iter]))
## 
## dat=ddply(dat,.(Selection,qname,F), with,
## #          as.data.frame(spectrum(data, spans = c(7,13), log = "dB", ci = 0.8,plot=FALSE)[c("freq","spec")]))
##           as.data.frame(spectrum(data, log = "dB", ci = 0.8,plot=FALSE)[c("freq","spec")]))
## 
## dat=ddply(subset(dat,freq>0.05),.(F,Selection,qname),transform,val=spec/max(spec))
## ggplot(dat,aes(freq,val,col=qname))+
##   geom_smooth(se=FALSE)+
##   facet_grid(F~Selection,scale="free_y")+
##   theme_bw()+
##   theme(legend.position="bottom")

## ----fig13, fig.width=10,fig.height=4,eval=FALSE,fig.cap="Cohort Effects"----
## library(FLAssess)
## 
## stk   =iter(stkm[,101:250],1)
## m(stk)=iter(m(stkr),1)[,101:250]
## vpa    =stk+VPA(stk)
## 
## p=plot(FLStocks("Actual"=stk,"Model Estimate"=vpa))
## p$data=subset(p$data,qname%in%c("SSB","Rec"))
## names(p$data)[8]="data"
## p$data=ddply(p$data,.(qname), transform, val=data/mean(data))
## ggplot(subset(p$data,year>50))+
##   geom_line(aes(year,val,col=qname))+
##   facet_grid(qname~stock,scale="free")+
##   theme_bw()+
##   theme(legend.position="none")+
##   xlab("Year")+ylab("")

## ----fig14, fig.width=6,fig.height=8,fig.cap="MSE using empirical HCR"----
library(FLife)
library(FLash)
library(FLBRP)
  
par=lhPar(FLPar(linf=100))  
eql=lhEql(par)
mou=as(eql,"FLStock")
mou=FLash::fwd(mou,f=fbar(eql)[,-1]/4,sr=eql)
  
srDev=rlnorm(100,FLQuant(rep(0,136)),0.3)

om=FLife:::mseSBT1(mou,eql,srDev,
             start=dims(mou)$maxyear,end=dims(mou)$maxyear+20,interval=3,
             k1=1.5,k2=3.0,gamma=1,nyrs=5,   
             uDev =0.2) 
  
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
library(plyr)
library(dplyr)

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
## 	install_github("flr/FLife")

