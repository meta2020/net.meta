
CI.matrix <- function(bmt, nmt){
  ## CrI matrix
  name <- as.character(bmt$treatment$description)
  id <- as.character(bmt$treatment$id)

  cr.low <- NULL
  cr.up <- NULL

  for (i in 1:length(id)){
    df <- summary(relative.effect(bmt$mtc.run, id[i], id,preserve.extra = FALSE))
    cr.low <- c(cr.low, df$summaries$quantiles[,1])
    cr.up <- c(cr.up, df$summaries$quantiles[,5])
  }
  cr.lowm <- matrix(cr.low, length(id),length(id), dimnames = list(id, id))
  cr.upm <- matrix(cr.up, length(id),length(id), dimnames = list(id, id))

  ## CI matrix

  ci.lowm <-nmt$lower.random
  ci.upm <- nmt$upper.random

  ## PI matrix
  pi.lowm <-nmt$lower.predict
  pi.upm <- nmt$upper.predict

  ## names
  dname <- NULL
  for(i in 1:(length(id)-1)){
    dname <- c(dname, paste0(id[i], ":", id[-(1:i)]))
  }


  MD <- data.frame(
    cr.low = as.vector(cr.lowm[upper.tri(cr.lowm)]),
    cr.up = as.vector(cr.upm[upper.tri(cr.upm)]),
    ci.low = as.vector(ci.lowm[upper.tri(ci.lowm)]),
    ci.up = as.vector(ci.upm[upper.tri(ci.upm)]),
    pi.low = as.vector(pi.lowm[upper.tri(pi.lowm)]),
    pi.up = as.vector(pi.upm[upper.tri(pi.upm)]),
    row.names = dname
  )
  return(MD)
}

### Imprecision
imp <- function(EF=c("diff", "ratio"),
                set,
                low, up){
  if(EF=="diff"){
    im.cond3 <-   (low < -set) + (set < up) == 2  # M
    im.cond2.1 <- (-set < low) + (low < 0) + (up > set) == 3 #S
    im.cond2.2 <- (low < -set) + (0 < up)  + (up < set) == 3 #S
  }
  if(EF=="ratio"){
    im.cond3 <-   (low < 1/set) + (set < up) == 2  # M
    im.cond2.1 <- (1/set < low)  + (low < 1) + (up > set) == 3 #S
    im.cond2.2 <- (low < 1/set) + (1 < up)  + (up < set) == 3 #S
  }

  imprecision <- ifelse(im.cond3, 3, ifelse(im.cond2.1 | im.cond2.2, 2, 1))
  return(imprecision)
}

#imp("diff", set=1, low=re.MD$m.low, re.MD$m.up)

### Heterogeneity

heter <- function(EF=c("diff", "ratio"),
                  set,
                  low1, up1,
                  low2, up2){

  i.ci <- imp(EF,set, low1, up1)
  i.pi <- imp(EF,set, low2, up2)

  h.cond1.1 <- (i.pi==1) + (i.ci ==1) == 2 ## N
  h.cond1.2 <- (i.pi==2) + (i.ci ==2) == 2 ## N
  h.cond1.3 <- (i.pi==3) + (i.ci ==3) == 2 ## N
  h.cond3   <- (i.pi==3) + (i.ci ==1) == 2 ## M
  heterog <- ifelse(h.cond1.1|h.cond1.2|h.cond1.3,1,ifelse(h.cond3,3,2))

  return(heterog)
}

#heter("diff", set=1, low=re.MD$m.low, re.MD$m.up, low2=re.MD$m.low2, re.MD$m.up2)

### GRADE function
###########################################################################################################3
#' To calculate the GRADE table.
#'
#' @title GRADE table
#'
#' @return GRADE table matrix
#'
#' @importFrom netmeta decomp.design netsplit
#' @importFrom stats complete.cases
#'
#' @param study.CM contribution matrix from sutdyCM_matrix
#' @param bmt result from bayesian net-meta model_gemtc
#' @param nmt result from frequentist net-meta model_netmmeta
#' @param rob risk of bias
#' @param ind indirectness
#' @param report.bias reported bias
#' @param effect.size two types: diff-difference; ratio-ratio
#' @param clinical.effect.size clinical effect size
#' @param inconsis inconsistence from mtc.nodesplit
#' @param model Bayes-bayesian net-meta model, Freq-frequentist net-meta model
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
#' nmt1 <- model_netmeta(long.data = LDT1,
#' treatment=LDT1$treatment,
#' id.treatments = trt1,
#' reference = "A",
#' outcome = "HR")
#'
#' name1 <- NULL
#' for(i in 1:(length(trt1$id)-1)){
#'   name1 <- c(name1, paste0(trt1$id[i], ":", trt1$id[-(1:i)]))
#' }
#'
#' study.CM1 <- studyCM_matrix(name1, nmt1)
#'
#' study.assess1 <- read.csv(system.file("extdata", "HR_SH_A.csv", package = "net.meta"))
#'
#' RB.comp1 <- rep(0, nrow(study.CM1)) #1 Yes, 0 no
#'
#' inconsist1<-gemtc::mtc.nodesplit(
#' bmt1$mtc.net,
#' likelihood = "poisson",
#' link = "log",
#' linearModel = "random",
#' dic=TRUE)
#'
#' RESULT.F1 <- GRADE_table(
#'   study.CM1,
#'   bmt=bmt1,
#'   nmt=nmt1,
#'   rob=study.assess1$ROB,
#'   ind=study.assess1$IND,
#'   report.bias=RB.comp1,
#'   effect.size = "ratio",
#'   clinical.effect.size=1.25,
#'   inconsis = inconsist1,
#'   model="Freq")
#'
#' RESULT.B1 <- GRADE_table(
#'   study.CM1,
#'   bmt1,
#'   nmt1,
#'   rob=study.assess1$ROB,  ## define Rob per study
#'   ind=study.assess1$IND,  ## define Indirectness per study
#'   report.bias=RB.comp1,
#'   effect.size = "ratio",
#'   clinical.effect.size=1.25,
#'   inconsis = inconsist1,
#'   model="Bayes")
#'

GRADE_table <- function(
  study.CM,
  bmt,
  nmt,
  rob,
  ind,
  report.bias,
  effect.size = c("diff", "ratio"),
  clinical.effect.size,
  inconsis,
  model = c("Bayes", "Freq")
  ){

## 1 and 2 from studyCM
within.bias <-  round(as.matrix(study.CM)%*% (rob)*0.01)
indirectness <- round(as.matrix(study.CM) %*% (ind)*0.01)

## 3 reporting bias

## 4 and 5 CrI, CI, PI
MD <- CI.matrix(bmt, nmt)
re.MD <- MD[rownames(study.CM),]

## 4.imprecision and 5.heterogeneity
if(model=="Freq"){
imprecision <- imp(effect.size, clinical.effect.size, low=re.MD$ci.low, up=re.MD$ci.up)

heterogeneity<- heter(effect.size, clinical.effect.size,
                      low1=re.MD$ci.low, up1=re.MD$ci.up,
                      low2=re.MD$pi.low, up2=re.MD$pi.up)
}

if(model=="Bayes"){
  imprecision <- imp(effect.size, clinical.effect.size, low=re.MD$cr.low, up=re.MD$cr.up)

  heterogeneity<- heter(effect.size, clinical.effect.size,
                        low1=re.MD$cr.low, up1=re.MD$cr.up,
                        low2=re.MD$pi.low, up2=re.MD$pi.up)
}

## 6. incoherence

## global test
dd <- decomp.design(nmt, tau.preset = nmt$tau.preset, warn = TRUE)
#inc.X <- dd$Q.decomp$pval[3]
inc.X <- dd[["Q.inc.random"]]$pval

ns <- netsplit(nmt, method = "SIDDE")
inc.P <- data.frame(inc.p=ns$compare.random, row.names = ns$compare.random[,1])

#inconsist.res <- ns

if(sum(complete.cases(inc.P$inc.p.p))>0){

if(model=="Freq"){

inc.P$inc.p.p <- ifelse(inc.P$inc.p.p>=0.1 & complete.cases(inc.P$inc.p.p), 1,inc.P$inc.p.p)
c.name <-rownames(inc.P[inc.P$inc.p.p<0.1 & complete.cases(inc.P),])

#if(length(c.name)>0){
if (typeof(inconsis)=="list"){

inc.D <- data.frame(inc.p=ns$direct.random, row.names = ns$compare.random[,1])[c.name,]
inc.I <- data.frame(inc.p=ns$indirect.random, row.names = ns$compare.random[,1])[c.name,]

i.D <- imp(effect.size,clinical.effect.size, inc.D$inc.p.lower, inc.D$inc.p.upper)
i.I <- imp(effect.size,clinical.effect.size, inc.I$inc.p.lower, inc.I$inc.p.upper)

inc.P[c.name,"inc.p.p"] <- ifelse((inc.D$inc.p.upper < inc.I$inc.p.lower) | (inc.D$inc.p.lower > inc.I$inc.p.upper),3,
              ifelse((i.D==3 | i.I==3), 3,
                     ifelse((i.D==2 | i.I==2),2,1)))

}

}

if(model=="Bayes"){

  if(bmt$outcome=="MD"){
    inconsist<-mtc.nodesplit(
        bmt$mtc.net,
        likelihood = "normal",
        link = "identity",
        linearModel = "random",
        dic=TRUE)
  }

  if(bmt$outcome=="RR"){
    inconsist<-mtc.nodesplit(
      bmt$mtc.net,
      likelihood = "binom",
      link = "log",
      linearModel = "random",
      dic=TRUE)
  }

  if(bmt$outcome=="HR"){
    inconsist<-mtc.nodesplit(
      bmt$mtc.net,
      likelihood = "poisson",
      link = "log",
      linearModel = "random",
      dic=TRUE)
  }

  inc <- summary(inconsis)
  inc.P[complete.cases(inc.P$inc.p.p),"inc.p.p"] <- inc$p.value$p

  inc.P$inc.p.p <- ifelse(inc.P$inc.p.p>=0.1 & complete.cases(inc.P$inc.p.p), 1,inc.P$inc.p.p)
  c.name <-rownames(inc.P[inc.P$inc.p.p<0.1 & complete.cases(inc.P),])

  #if(length(c.name)>0){
  if (typeof(inconsis)=="list"){
    #inc <- summary(inconsist)

    inc.D <- data.frame(inc.p=inc$dir.effect, row.names = paste0(inc$dir.effect$t1,":",inc$dir.effect$t2))[c.name,]
    inc.I <- data.frame(inc.p=inc$ind.effect, row.names = paste0(inc$ind.effect$t1,":",inc$ind.effect$t2))[c.name,]

    i.D <- imp(effect.size,clinical.effect.size, inc.D$inc.p.ci.l, inc.D$inc.p.ci.u)
    i.I <- imp(effect.size,clinical.effect.size, inc.I$inc.p.ci.l, inc.I$inc.p.ci.u)

    inc.P[c.name,"inc.p.p"] <- ifelse((inc.D$inc.p.ci.u < inc.I$inc.p.ci.l) | (inc.D$inc.p.ci.l > inc.I$inc.p.ci.u),3,
                                      ifelse((i.D==3 | i.I==3), 3,
                                             ifelse((i.D==2 | i.I==2),2,1)))
   # inc <- summary(inconsist)
    #inconsist.res <- inc
  }
  #inconsist.res <- inc
  }
}

inc.P[is.na(inc.P)] <- ifelse(inc.X<0.05, 3, ifelse(inc.X>0.1,1,2))
incoherence.p <- inc.P$inc.p.p


#RESULT$incoherence <- rep(3,10)

## report.bias


RESULT <- as.data.frame(matrix(NA, length(nmt$comp.order),7))
row.names(RESULT) <- rownames(study.CM)
RESULT[,1] <- ifelse(within.bias==1, "No concerns", ifelse(within.bias==2, "Some concerns", "Major concerns"))
RESULT[,2] <- ifelse(report.bias==0, "Undetected", "Suspected")
RESULT[,3] <- ifelse(indirectness==1, "No concerns", ifelse(indirectness==2, "Some concerns", "Major concerns"))
RESULT[,4] <- ifelse(imprecision==1, "No concerns", ifelse(imprecision==2, "Some concerns", "Major concerns"))
RESULT[,5] <- ifelse(heterogeneity==1, "No concerns", ifelse(heterogeneity==2, "Some concerns", "Major concerns"))
RESULT[,6] <- ifelse(incoherence.p==1, "No concerns", ifelse(incoherence.p==2, "Some concerns","Major concerns"))
incoherence.n <- ifelse(RESULT[,6]=="No concerns", 1,ifelse(RESULT[,6]=="Some concerns", 2, 3))
incoherence.n[is.na(incoherence.n)] <- 3
#RESULT[,7] <- rep("High", length(nmt$comp.order))
RESULT7.s <- rep(4, length(nmt$comp.order))
RESULT7 <- round(RESULT7.s-(within.bias-1)/2-(imprecision-1)/2-(heterogeneity-1)/4-(incoherence.n-1)/4)
RESULT[,7] <- ifelse(RESULT7==4, "High",ifelse(RESULT7==3, "Moderate",ifelse(RESULT7==2, "Low", "Very low")))
RESULT[is.na(RESULT)] <- "Not applicable"

colnames(RESULT) <- c("Within-study bias", "Reporting bias", "Indirectness","Imprecision","Heterogeneity","Incoherence", "Confidence rating")
return(RESULT)

}
