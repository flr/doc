# ini.R - DESC
# /ini.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(knitr)

opts_chunk$set(fig.align='center', message=FALSE, warning=FALSE, echo=TRUE, cache=TRUE)

set.seed(1423)

library(captioner)

# Figures cross-referencing {{{

fig_nums <- captioner(prefix = "Figure")

fign <- function(name) {
  fig_nums(name=name, display="cite")
}

knit_hooks$set(fig.cap = function(before, options, envir) {
  if(before) {
    invisible(fig_nums(name=options$label, caption=options$fig.cap))
  }
  options$fig.cap <- paste0("Figure", fig_nums(name=options$label), ".")

  return(options)
}) # }}}

library(printr)

# knit_print.FLQuant{{{
knit_print.FLQuant <- function(object, options, cols=5) {

    # dims
    do <- dim(object)

    # More year than cols * 2
    if(do[2] > (cols * 2)) {
  
      cat("An object of class \"", as.character(class(object)), "\"\n", sep="")
      
      scols <- 1:cols
      ecols <- dimnames(object)[[2]][(do[2]-cols+1):do[2]]

      x1 <- object[,scols]
      x2 <- object[,ecols]

      if(dim(object)[6] != 1)
        cat("iters: ", dim(object)[6],"\n\n")

      if(dim(object)[6] > 1) {
        x1v1 <- apply(x1@.Data, 1:5, median, na.rm=TRUE)
        x1v2 <- apply(x1@.Data, 1:5, mad, na.rm=TRUE)   
        x1v3 <- paste(format(x1v1,digits=5),"(", format(x1v2, digits=3), ")", sep="")
        x2v1 <- apply(x1@.Data, 1:5, median, na.rm=TRUE)
        x2v2 <- apply(x1@.Data, 1:5, mad, na.rm=TRUE)   
        x2v3 <- paste(format(x1v1,digits=5),"(", format(x1v2, digits=3), ")", sep="")
      } else {
        x1v3 <- format(x1,digits=5)
        x2v3 <- format(x2,digits=5)
      }
    
      print(array(x1v3, dim=dim(x1)[1:5], dimnames=dimnames(x1)[1:5]), quote=FALSE)
      cat("      [ ... ", do[2] - cols*2,"years]\n\n")
      print(array(x2v3, dim=dim(x2)[1:2], dimnames=dimnames(x2)[1:2]), quote=FALSE)
    } else {
      print(object)
    }
} # }}}
