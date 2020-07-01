#' To plot GRADE result.
#'
#' @title GRADE plot
#'
#' @return ggplot object
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 geom_tile element_rect scale_fill_manual scale_x_discrete scale_y_discrete
#'
#' @param RESULT GRADE matrix result
#' @param font.size font size
#' @param lab.size label size
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
#' p<-GRADE_plot(RESULT.B1, font.size=4, lab.size=12)
#'

GRADE_plot <- function(
  RESULT,   ## result from grades.RData
  font.size,
  lab.size){

RESULT$group <- rownames(RESULT)
m.result <- melt(RESULT, id="group")
study.order <- rownames(RESULT)

group <- m.result[,1]
value <- m.result[,3]
variable <- m.result[,2]

## color
gyr <- c("No concerns"="#00CC00","Undetected"="#00CC00",
         "Some concerns"="#FFFF66",
         "Major concerns"="#CC0000", "Suspected"="#CC0000", "Not applicable"="#FFFFFF",
         "High"="#2ECC71","Moderate"="#3498DB","Low"="#F1C40F","Very low"="#E74C3C")


p<-ggplot(m.result,
    aes(x=variable, y=group, fill=value)) +
    geom_tile(colour="white", size=1,stat="identity",linejoin="round") + #rectangles for each correlation
    geom_text(aes(label = value), size=font.size,family = "Helvetica" ) +
    theme_tufte() +
    theme(axis.text.x = element_text(angle = 0, face = "bold", color = "black", size = lab.size),
          axis.text.y = element_text(angle = 0, color = "black", size = lab.size),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          #plot.margin = unit(c(3, 1, 0, 0), "mm"),
          legend.title=element_blank(),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill=NA,color="white", size=0, linetype="solid"),
          text = element_text(family = "Helvetica")
    ) +
    scale_fill_manual(values=gyr)+
    scale_x_discrete(position = "top")+
    scale_y_discrete(limits=rev(study.order))
}
