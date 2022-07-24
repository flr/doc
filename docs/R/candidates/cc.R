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
