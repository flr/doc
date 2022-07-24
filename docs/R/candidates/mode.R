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

lmode<-function(len,n,bin=2.5) {

  res=ddply(len,.(bin), with, data.frame(freq=sum(n)))
  res=as.character(subset(res,freq==max(freq))[1,"bin"])
  unbin(res)$mid}


