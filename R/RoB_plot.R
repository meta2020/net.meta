#' To plot risk of bias result.
#'
#' @title Risk of bias bar plot
#'
#' @return ggplot object
#'
#' @importFrom ggplot2 geom_bar coord_flip scale_y_continuous scale_colour_discrete
#'
#' @param study.CM contribution matrix
#' @param rob.ind risk of bias or indirectness
#' @param lab.size label size
#' @param font.family "Helvetica" or "Times New Roman"
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
#' p<-RoB_plot(study.CM1, study.assess1$ROB, lab.size=10)
#'
#' p<-RoB_plot(study.CM1, study.assess1$IND, lab.size=10)
#'

RoB_plot <- function(
  study.CM,
  rob.ind,
  lab.size,
  font.family = c("Helvetica", "Times New Roman")
  ){

t.CM <- as.data.frame(t(study.CM))
t.CM$study <- colnames(study.CM)
comp.order <- rownames(study.CM)

m.CM <- reshape2::melt(t.CM, id.vars=c("study"))
m.CM$rob.ind <- rep(factor(rob.ind, levels=c(max(rob.ind):1)), length(comp.order))

## color
gyr <- c("1"="#00CC00","2"="#FFFF66","3"="#CC0000")

#gyr <- c("1"="#CBD741","2"="#FFE169","3"="#FF4200")
brks <- c(0,0.2,0.4,0.6,0.8,1)
ggplot(m.CM, aes(x = variable, y = value, fill = rob.ind))+
  geom_bar(position="fill", stat="identity", colour="white", size=0.3) +
  coord_flip()+
  theme_tufte()+
  ylab(NULL)+ xlab(NULL)+
  scale_fill_manual(values=gyr)+
  #scale_fill_brewer(palette="Spectral")+ #Spectral
  scale_y_continuous(breaks = brks, labels = paste0(brks*100, "%"))+
  scale_x_discrete(limits=rev(comp.order))+
  scale_colour_discrete(guide=FALSE)+
  theme(
    text = element_text(family = font.family),
    legend.title=element_blank(),
    legend.position = "none",
    axis.text.y = element_text(angle = 0, color = "black", size = lab.size))
}

