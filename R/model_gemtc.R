#' This function is the warpper of mtc.network, mtc.model, mtc.run, and relative.effect from gemtc package. \cr
#' R packgae gemtc and rjags are required.
#'
#' @title Bayesian net-meta model for MD, HR, and RR
#'
#' @return Summary list of the results
#'
#' @importFrom gemtc mtc.model mtc.network mtc.run relative.effect
#'
#' @param long.data data.frame to be analyzed should be formatted in long format
#' @param id.treatments data.frame to specify the id and treatments
#' @param reference the referential id in the net-meta
#' @param outcome the outcome should be MD-mean difference, HR-hazard ratio, and RR-risk ratio
#' @param mtc.n.adapt the number of adaptation (or tuning, burn-in) iterations, default is 5000; means to discard 1-5000 of the interations
#' @param mtc.n.iter the number of simulation iteration, default is 10000; means to perform 10000 simulations
#' @param mtc.thin default is 20, means to extract 20th value; details in mtc.run from gemtc package
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
#
model_gemtc <- function(
  long.data,
  id.treatments,
  reference,
  outcome=c("MD", "HR", "RR"),
  mtc.n.adapt,
  mtc.n.iter,
  mtc.thin){

  outcome <- match.arg(outcome)

net <- mtc.network(data = long.data, treatments=id.treatments, description="")

# MD
if(outcome=="MD"){
  model <- mtc.model(
    net,
    type="consistency",
    likelihood = "normal",
    link = "identity",
    linearModel = "random")
}

# RR
if(outcome=="RR"){
  model <- mtc.model(
    net,
    type="consistency",
    likelihood = "binom",
    link = "log",
    linearModel = "random")
}
# HR
if(outcome=="HR"){
  model <- mtc.model(
    net,
    type="consistency",
    likelihood = "poisson",
    link = "log",
    linearModel = "random",
    dic=TRUE)
}

  mcmc <- mtc.run(model, n.adapt=mtc.n.adapt,n.iter=mtc.n.iter,thin=mtc.thin)

  RE  <- relative.effect(mcmc, reference,  preserve.extra = FALSE)
  RET <- summary(RE)

  return(list(
  mtc.net=net,
  mtc.model=model,
  mtc.run=mcmc,
  relative.effect=RET,
  outcome = outcome,
  treatment = id.treatments,
  ref.treatment = id.treatments[id.treatments$id==reference,]))
}

