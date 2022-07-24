haupt<-function(lfd,pars,lc=FLife::vonB(pars["sel1"],pars)*.9,lmax="missing"){
  
  dat=lfd[as.numeric(dimnames(lfd)$len)>c(lc)]
  if (!missing(lmax))
    dat=dat[as.numeric(dimnames(dat)$len)<c(lmax)]
  
  l  =dat
  l[]=as.numeric(dimnames(l)[[1]])

  l1  =dat[-dim(dat)[1]]
  l1[]=as.numeric(dimnames(l1)[[1]])

  l2  =dat[-1]
  l2[]=as.numeric(dimnames(l2)[[1]])

  dt=-log(1-l2%/%pars["linf"])/pars["k"]+log(1-l1/pars["linf"])/pars["k"]
  t =pars["t0"]*log(1-(l/pars["linf"]))/pars["k"]  
  
  dat=model.frame(FLQuants("y"=log(dat[-dim(t)[1]]%/%dt),
                           "x"=      t[-dim(t)[1]]))
  dat=subset(dat,is.finite(y))
  #z  =ddply(dat,.(year,iter), with, data.frame(data=-lmRob(y~x)$coefficients["x"]))
  z  =ddply(dat,.(year,iter), with, data.frame(data=-lm(y~x)$coefficients["x"]))
  z  =transform(z,year=factor(year),iter=factor(iter))
  as.FLQuant(z)
  }

gislasonM<-function(params,age=params["sel1"]) {
  
  length=vonB(age,params)
  mean(exp(params["m1"]%+%(params["m2"]%*%log(length))%+%(params["m3"]%*%log(params["linf"]))%+%log(params["k"])))
  }

if (FALSE){
library(ggplotFL)
library(ggpubr)

load("/home/laurence-kell/Desktop/papers/COM3/R/runs/om/om.18.RData")
load("/home/laurence-kell/Desktop/papers/COM3/R/runs/indicators/lfd.18.RData")
load("/home/laurence-kell/Desktop/papers/COM3/data/lhs.RData")

z=haupt(lfdc,lhs[[3]])
  
plot(z)
}
