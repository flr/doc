#' @title Catch curve analysis
#'
#' @description 
#' Catch curve analysis
#' 
#' @param age age
#' @param n   frequency
#' @param ... any other arguments
#' 
#' @aliases cc cc-method cc,numeric,numeric-method cc,missing,FLQuant-method cc,FLQuant,missing-method
#' 
#' @return Returns an object of same class \code{age} 
#' 
#' @seealso \code{\link{powh}}
#' 
#' @export
#' @docType methods
#' @rdname cc
#' 
#' @seealso \code{\link{powh}}  
#' 
#' @examples
#' \dontrun{
#' data(ple4)
#' ctc=as.data.frame(catch.n(ple4))
#' dat=cc(age=ctc$age,n=ctc$data)
#' head(dat)
#' }
setGeneric('cc', function(age,n,...) standardGeneric('cc'))

setMethod("cc", signature(age="numeric",n="numeric"),
          function(age,n,...){  
            res=ccFn(age,n)
            res})

setMethod("cc", signature(age="missing",n="FLQuant"),
          function(age,n){   
            dat=data.frame(n)
            res=with(dat,ccFn(age,data))
            res@units=""
            res})

setMethod("cc", signature(age="FLQuant",n="missing"),
          function(age,n){   
            dat=data.frame(age)
            res=with(dat,ccFn(age,data))
            res@units=""
            res})
ccFn=function(age,n){
  lm  =lm(log(n)~age)
  hat =exp(predict(lm))
  sel =(n/hat)/max(n/hat)
  data.frame(age=age,obs=n,hat=hat,sel=sel)}

data(ple4)
x   =catch.n(ple4)[ac(2:9)]
smpl=as.FLQuant(adply(x, c(2,6), function(x) data.frame(age=names(x),data=apply(rmultinom(100,1,x),1,sum))))
z   =ddply(subset(as.data.frame(smpl),data>0&!is.na(data)&is.finite(data)), .(year,iter), with, 
                     data.frame(data=-coefficients(lm(log(data)~age,na.rm=T))[2]))

zhat=as.FLQuant(z)


setGeneric("acc", function(object, ...)
		standardGeneric("acc"))

#' @examples
#' data(ple4)
#' acc(catch.n(ple4), ages=2:9)

setMethod("acc", signature(object="FLQuant"),
  function(object, ages=seq(dim(object)[1])) {

  res <- apply(object[ages,], c(2, 6), function(i) {
    -coefficients(lm(log(i[i > 0]) ~ as.numeric(dimnames(i)$age)))[2]
  })

  units(res) <- 'z'

  return(res)

})


#' @examples
#' acc(ple4)

setMethod("acc", signature(object="FLStock"),
  function(object, metric="catch.n", 
  ages=seq(range(x, 'minfbar'), range(x, 'plusgroup') - 1)) {

  inp <- do.call(metric, list(object))[ages]

  res <- acc(inp)

  units(res) <- 'z'

  return(res)
})

acc(ple4)
acc(propagate(ple4, 10))

setMethod("acc", signature(object="FLCatch"),
  function(object, metric="catch.n", 
  ages=seq(range(x, 'minfbar'), range(x, 'plusgroup') - 1)) {

  inp <- do.call(metric, list(object))[ages]

  res <- acc(inp)

  units(res) <- 'z'

  return(res)
})





plot(mcf(FLQuants("OM"=fbar(ple4)+m(ple4)[5],"MP"=zhat)))
plot(mcf(FLQuants("OM"=quantMeans(z(ple4)[3:6]),"MP"=zhat)))

rc <- roc(quantMeans(z(ple4)[3:6]) > 0.42, zhat)
rc <- roc(quantMeans(z(ple4)[3:6]) > 0.42, zhat / 0.42)
rc <- roc((fbar(ple4)+m(ple4)[5]) > 0.42, zhat)

ggplot(rc, aes(x=FPR, y=TPR)) +
  geom_line() +
  geom_abline(slope=1, intercept=0, colour="red", linetype=2)

with(rc, auc(TPR, FPR))
