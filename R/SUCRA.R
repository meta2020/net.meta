#' To calculate SUCRA from bayesian model.
#'
#' @title SUCRA from bayesian model
#'
#' @return SUCRA result
#'
#' @importFrom gemtc rank.probability
#'
#' @param bmt result from model_gemtc
#' @param digits digits of the result
#'
#' @export
#' @examples
#' LDT1 <- read.csv(system.file("extdata", "HR_SH_D.csv", package = "net.meta"))
#' trt1 <- read.table(system.file("extdata", "HR_SH_D.txt", package = "net.meta"),
#'     header=TRUE,quote = '"', stringsAsFactors=FALSE)
#'
#' trt1$description <- factor(trt1$description, trt1$description)
#' LDT1$study <- factor(LDT1$study, unique(LDT1$study))
#'
#' bmt1 <- model_gemtc(
#' long.data=LDT1,
#' id.treatments=trt1,
#' reference="A",
#' outcome="HR",
#' mtc.n.adapt = 5000, mtc.n.iter = 10000, mtc.thin = 20)
#'
#' # View(bmt1)
#'
#' SUCRA(bmt1)
#'

SUCRA <- function(bmt, digits=3)
  {
  rank.prob <- rank.probability(bmt$mtc.run,preferredDirection=-1)
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
