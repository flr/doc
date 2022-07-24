#' @title Derives an \code{FLBRP} from life history parameters
#' 
#' @description 
#' Takes an \code{FLPar} object with life history and selectivity parameters
#' and generates a corresponding \code{FLBRP} object. Can use a range of functional forms.
#'
#' @param params an \code{FLPar} object with life history parameters
#' @param growth function that takes an \code{FLPar} object with parameters, by default \code{vonB}
#' @param m \code{character} takes the natural mortality model name, by default gislason 
#' @param mat function that takes an \code{FLPar} object with parameters, by default \code{logistic}
#' @param sel function that takes an \code{FLPar} object with parameters, by default \code{dnormal}
#' @param sr \code{character} value, "bevholt" by default
#' @param range \code{numeric} with age range by default from 0 to 40
#' @param spwn \code{numeric} give propotion of year when spawning occurs, by default is params["a50"]-floor(params["a50"])
#' @param fish \code{numeric} give propotion of year when fishing occurs, by default 0.5        
#' @param units \code{character} for vectors in \code{FLBRP} returned by method
#' @param midyear when growth measured, default 0.5
#' @param ... any other arguments 
#' 
#' @aliases lhEql lhEql-method lhEql,FLPar-method
#' 
#' @return \code{FLBRP} object
#'
#' @seealso \code{\link{lhPar}}, \code{\link{lhRef}}
#'
#' @export lhEql
#' @docType methods
#' @rdname lhEql
#'
#' @seealso  \code{\link{vonB}} \code{\link{lorenzen}} \code{\link{sigmoid}}
#' 
#' @examples
#' \dontrun{
#' load("lfd.1.RData")
#' load("design.RData")
#' par=lhs[["Pollachius pollachius"]]
#' par=rbind(FLPar("lc"=FLife:::vonB(par["sel1"],par)),par)
#' z=bhz(lfd,par)
#' plot(z)
#' }
#' 
#' 
#' 


bhz<-function(lfd,par){
  lfd=lfd[dimnames(lfd)[[1]]>=c(par["lc"]),]
  len=FLQuant(dimnames(lfd)[[1]],dimnames=dimnames(lfd))
  lmean=quantSums(lfd*len)%/%quantSums(lfd)
  
  par["k"]%*%(par["linf"]%-%lmean)%/%(lmean%-%par["lc"])}




