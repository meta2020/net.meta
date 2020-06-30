#' This function plots the network graph with SUCRA
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
#' LDT1 <- read.csv(system.file("extdata", "mHR_SH_D.csv", package = "net.meta"))
#' trt1 <- read.table(system.file("extdata", "mHR_SH_D.txt", package = "net.meta"),
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
## load pkgs
library(cowplot)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(netmeta)

net_score_plot <- function(
  nmt,
  sucra,
  scl=1,
  ofs=0.05,
  lab=trt$description,
  font.size,
  pos = 0.5,
  labels){

op <- par(family = "sans")
netgraph(
  nmt,
  start = "circle", iterate = FALSE,
  labels = lab,
  scale=scl,
  offset=ofs,
  #col.multiarm = "white",
  col = "skyblue",
  plastic = FALSE,
  thickness = "number.of.studies",
  points = TRUE,
  col.points="cornflowerblue",
  cex.points = nmt$k.trts,
  number.of.studies=TRUE,
  pos.number.of.studies = pos,
  col.number.of.studies = "black",
  bg.number.of.studies = "grey",
  cex=font.size+0.5,
  family= "sans")

par(op)

p1 = recordPlot()

p2 <-ggplot(sucra, aes(y = sucra, x = 0, label = treatment)) +
  geom_vline(xintercept = 0)+
  geom_point(color = "red") +
  ylim(0,1) +
  ylab("")+
  xlab("")+
  theme_tufte()+
  theme(
    text = element_text(family = "Helvetica"),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
    plot.title   = element_text(hjust = 0, size=8)
  )+
  xlim(0,1)+
  ggtitle("SUCRA")+
  geom_text_repel(
    nudge_x      = 0.1,
    direction    = "y",
    hjust        = 0,
    segment.size = 0.2,
    family="Helvetica",
    size = font.size*10
  )

p<-plot_grid(p1, p2, labels = labels, rel_widths = c(2, 1))
p

}
