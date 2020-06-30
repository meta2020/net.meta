#' This function is the warpper of netmeta and pairwise functions from netmeta package. \cr
#' R packgae netmeta is required.
#'
#' @title Frequentist net-meta model for MD, HR, and RR
#'
#' @return Summary list of the results
#'
#' @importFrom netmeta pairwise netmeta
#' @import netmeta
#'
#' @param long.data data.frame to be analyzed should be formatted in long format
#' @param id.treatments data.frame to specify the id and treatments
#' @param treatment treatment names or labels
#' @param reference the referential id in the net-meta
#' @param outcome the outcome should be MD-mean difference, HR-hazard ratio, and RR-risk ratio
#'
#' @export
#' @examples
#' LDT1 <- read.csv(system.file("extdata", "mHR_SH_D.csv", package = "net.meta"))
#' trt1 <- read.table(system.file("extdata", "mHR_SH_D.txt", package = "net.meta"),
#'     header=TRUE,quote = '"', stringsAsFactors=FALSE)
#'
#' trt1$description <- factor(trt1$description, trt1$description)
#' LDT1$study <- factor(LDT1$study, unique(LDT1$study))
#'
#' trt1$label <- paste0(trt1$id,"-", trt1$description)
#' LDT1$label <- factor(LDT1$treatment, labels = trt1$label)
#'
#'
#' nmt.lab1 <- model_netmeta(
#'   long.data=LDT1[,-2],
#'   treatment=LDT1$label,
#'   id.treatments=trt1,
#'   reference = "A",
#'   outcome="HR")
#'
#' # View(nmt.lab1)
#
model_netmeta<- function(
  long.data,
  id.treatments,
  treatment,
  reference,
  outcome=c("MD", "HR", "RR")){

if(outcome=="MD"){
p <- pairwise(
  treat = treatment,
  n = sampleSize,
  mean = mean,
  sd = std.dev,
  #seTE = std.err,
  data = long.data,
  studlab = study,
  sm="MD"
)

nmt <- netmeta(
  p,
  sm="MD",
  prediction=TRUE,
  all.treatments=TRUE,
  comb.fixed = FALSE,
  comb.random = TRUE,
  warn = FALSE,
  #seq=label,
  reference.group=reference
)
}

## RR
if(outcome=="RR"){
p <- pairwise(
  treat = treatment,
  event = responders,
  n = sampleSize,
  data = long.data,
  studlab = study,
  sm="RR"
)

nmt <- netmeta(
  p,
  sm="RR",
  prediction=TRUE,
  all.treatments=TRUE,
  comb.fixed = FALSE,
  comb.random = TRUE,
  warn = FALSE,
  reference.group=reference
)
}
## HR
if(outcome=="HR"){
p <- pairwise(
  treat = treatment,
  event = responders,
  time = exposure,
  data = long.data,
  studlab = study,
  sm="IRR"
)

nmt <- netmeta(
  p,
  sm="HR",
  prediction=TRUE,
  all.treatments=TRUE,
  comb.fixed = FALSE,
  comb.random = TRUE,
  warn = FALSE,
  reference.group=reference
)
}


## labs and names
study.lab <- nmt$studlab
comp.all <- names(nmt$prop.direct.random)
comp.dir <- nmt$designs
comp.ind <- comp.all[!(comp.all %in% comp.dir)]
#comp.order <- c(comp.dir, comp.ind)
nmt[["paiwise"]] <- p
nmt[["comp.order"]] <- c(comp.dir, comp.ind)
nmt$outcome <- outcome
nmt$id.treatment <- id.treatments
nmt$ref.treatment <- id.treatments[id.treatments$id==reference,]

return(nmt)
}
