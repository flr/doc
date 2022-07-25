utils::globalVariables(c("adply","dnorm","lh","llply","pnorm","rmultinom"))

#' invAlk
#' 
#' @title invAlk
#' 
#' @description Create an Age Length key, i.e. probability of a fish of a given age being in 
#' a length bin, can be used to sample length frequency distributions
#' 
#' @author Laurence Kell, Sea++
#' 
#' @name invAlk
#' @param object \code{FLPar} with `linf` or  \code{numeric}
#' @param ... any additional arguments
# #' @param age \code{numeric} 0:40 by default
# #' @param cv  \code{numeric} 0.1 by default
# #' @param lmax  \code{numeric} maximum size relative to 'linf' 1.25 by default
#'  
#' @aliases invAlk invAlk-method invAlk,FLPar-method
#' 
#' @docType methods
#' @export invAlk
#'  
#' @rdname invAlk
#' @seealso lenSample
#' 
#' @examples
#' \dontrun{
#' ak=invAlk(FLPar(linf=50))
#' }
setGeneric('invAlk', function(object,...) standardGeneric('invAlk'))

setMethod('invAlk', signature(object="FLPar"),
          function(object,age=0:40,cv=0.1,lmax=1.25,bin=1,...)
            
            setInvAlkFn(object,age,cv,lmax,bin))

setInvAlkFn<-function(par,age=0:40,cv=rep(0.1,length(age)),lmax=1.2,bin=1){
  
  invAlk=mdply(data.frame(age=age,cv=cv), function(age,cv,par,lmax){
    res=adply(par,2,function(x,age,cv,lmax){
        bins =seq(0,ceiling(x["linf"]*lmax),bin)
        len=vonB(age,x)
        sd=len*cv
        p  =c(pnorm(1,len,sd),
              dnorm(bins[-c(1,length(bins))],len,sd),
              pnorm(bins[length(bins)],len,sd,lower.tail=FALSE))
  
        data.frame(len=bins,p=p/sum(p))},
      age=age,cv=cv,lmax=lmax)},par=par,lmax=lmax)
  
  invAlk=cast(invAlk,age~len~iter,fun=sum,value="p")
  invAlk=FLPar(array(invAlk,dim=unlist(llply(dimnames(invAlk),length)),dimnames=dimnames(invAlk)))
  
  invAlk}

#' lenSample
#' 
#' @title lenSample 
#' 
#' @description 
#' @author Laurence Kell, Sea++  
#' 
#' @name lenSample
#' 
#' @param object \code{FLStock} blah,blah,blah,...
#' @param invAlk \code{FLPar} with invAlk
#' @param nsample \code{numeric} sample size, default is 500
#' @param ... any additional arguments
#' 
#' @docType methods
#' 
#' @export lenSample
#' @rdname lenSample
#' @seealso setInvAlk
#' 
#' @aliases lenSample  
#'          lenSample-method  
#'          lenSample,FLQuant,FLPar,missing-method
#'          lenSample,FLQuant,FLPar,numeric-method
#' 
#' @examples
#' \dontrun{
#' lfd=lenSample(catch.n(ple4)[,ac(2000:2005)],invAlk,nsample=100)
#' }
setGeneric('lenSample', function(object,invAlk,nsample,...) standardGeneric('lenSample'))

setMethod('lenSample', signature(object="FLQuant",invAlk="FLPar",nsample="missing"),
          function(object,invAlk,nsample=500,...)
            
            lenSampleFn(object,invAlk,nsample))

setMethod('lenSample', signature(object="FLQuant",invAlk="FLPar",nsample="numeric"),
          function(object,invAlk,nsample,...)
            
            lenSampleFn(object,invAlk,nsample))

lenSampleFn<-function(object,invAlk,nsample){
  
  res=mdply(expand.grid(iter=seq(dim(object)[6]),year=seq(dim(object)[2])),
         function(iter,year){ 
            lfd=object[,year,,,,iter]%*%invAlk[,,min(iter,dim(invAlk)[3]),drop=T]
            data.frame(len=dimnames(lfd)[[2]],data=apply(rmultinom(nsample,1,prob=lfd),1,sum))})

  res=as.FLQuant(res)
  dimnames(res)[2:6]=dimnames(object)[2:6]
  res}

lenSampleFn2<-function(object,invAlk,nsample){
  
  res=mdply(expand.grid(iter=seq(dim(object)[6]),year=seq(dim(object)[2])),
            function(iter,year){ 
              lfd=object[,year,,,,iter]%*%invAlk[,,min(iter,dim(invAlk)[3]),drop=T]
              lfd2=lfd
              lfd2[]=1/sum(dim(lfd)[2])
              res=data.frame(len=dimnames(lfd)[[2]],data=apply(rmultinom(nsample,1,prob=lfd2),1,sum))
              mutate(cbind(res,p=t(lfd)),len=len,data=data*p)[,-3]})
  
  res=as.FLQuant(res)
  dimnames(res)[2:6]=dimnames(object)[2:6]
  res}

if (FALSE){
  library(FLCore)
  library(FLife)
  library(plyr)
  library(dplyr)

  load("/home/laurence/Desktop/sea++/mydas/tasks/task4/data/turbot.RData")
  invAlk=invAlk(FLPar(lh[,1]))
  lfd=lenSample(stock.n(om)[,95:100,,,,1:2],invAlk,nsample=5000)
    
  ggplot(melt(lfd))+
    geom_histogram(aes(Var.3,weight=value),binwidth=1)+
    facet_grid(year~iter)+
    xlab("Length (cm)")+ylab("Frequency")+
    scale_x_continuous(limits=c(0,45))  
}

#' mode
#' 
#' @description 
#'  For a vector with labels corresponding to intervals i.e. \code{"(0,10]"}
#'  returns a data.frame with left and right boundaries and mid point.
#'      
#' @param x; a vector of with intervals as names 
#' @return a \code{data.frame} with left and right boundaries and mid points.
#' @export
#' @docType functions
#' @rdname lk-funcs
#' 
#' @examples
#' x=summary(cut(runif(100),seq(0,1,.1)))
#' unbin(x)
unbin=function(x){
  left =as.numeric(substr(x,2,unlist(gregexpr(",",x))-1))
  right=as.numeric(substr(x,  unlist(gregexpr(",",x))+1,nchar(x)-1))
  mid  =(left+right)/2
  
  data.frame(left=left,right=right,mid=mid,n=x)}

lmodeFn<-function(len,n,bin=25) {
  
  dat=data.frame(bin=cut(len,breaks=bin),n=n)
  res=ddply(dat,.(bin), with, data.frame(freq=sum(n)))
  res=as.character(subset(res,freq==max(freq))[1,"bin"])
  unbin(res)$mid}

lenInd<-function(len,n,wt="missing",lopt=NA){
  
  res=ddply(data.frame(len=len,n=n), .(len), with, data.frame(n=sum(n)))
  n  =res$n
  len=res$len
  
  if (wt[1]!="missing")
   wt=ddply(data.frame(len=len,n=n,wt=wt), .(len), with, data.frame(wt=sum(n*wt)/sum(wt)))[,2]
  
  lopt=lopt[1]
  
  # L95 95th percentile
  l95=weighted.quantile(len,n,probs=0.95)
  
  # L25 25th percentile of length distribution
  l25=weighted.quantile(len,n,probs=0.25)
  
  # Lmax5 mean length of largest 5%
  lmax5=weighted.mean(len,n*(len>=l95))
  
  # Lmega 
  pmega=NA
  if (!is.na(lopt)){
    lmega=lopt*1.1
  
    # Pmega Proportion of indiv$iduals above $L_{opt} + 10\%$
    pmega=sum(n*(len>=lmega))/sum(n)
    }
  
  
  # Lc Length at 50\% of modal abundance
  lc=lmodeFn(len,n)*0.5
  
  lmean=weighted.mean(len,n*(len>=lc))

  lbar=weighted.mean(len,n)
  
  lmaxy=NA
  if (!missing(wt))
    lmaxy=lmodeFn(len,n*wt)
  
  res=c(l95=l95,l25=l25,lmega=lmega,lmax5=lmax5,pmega=pmega,lmean=lmean,lbar=lbar,lmaxy=lmaxy,lc=lc)
  names(res)=c("l95","l25","lmega","lmax5","pmega","lmean","lbar","lmaxy","lc")
  
  res}
