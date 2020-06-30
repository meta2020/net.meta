
#######################################################
##
##  SUCRA
##
######################################################

## load pkgs
library(gemtc)

SUCRA <- function(bmt, digits=3)
  {
  rank.prob <- gemtc::rank.probability(bmt$mtc.run,preferredDirection=-1)
  sucra <- NULL
  all.trt <- length(bmt$treatment$id)
  for (i in 1:all.trt){
    sucra[i] <- cumsum(cumsum(rank.prob[i,]))[all.trt-1]/(all.trt-1)
  }
  df <- data.frame(
    id = rownames(rank.prob),
    treatment=bmt$treatment$description,
    sucra =round(sucra, digits)
  )
  return(df)
}
