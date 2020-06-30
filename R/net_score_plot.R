#' To plot the network graph with SUCRA.
#'
#' @title Network and SUCRA plots
#'
#' @return a ggplot object
#'
#' @importFrom netmeta netgraph
#' @importFrom ggplot2 ggplot aes geom_vline geom_point ggtitle ylim xlim ylab xlab theme element_text element_blank
#' @importFrom ggthemes theme_tufte
#' @importFrom ggrepel geom_text_repel
#' @importFrom cowplot plot_grid
#'
#' @param nmt results from model_netmeta function
#' @param sucra sucra results from SUCRA function
#' @param scl additional space added outside of edges in the net graph
#' @param ofs distance between edges in the net graph
#' @param lab an optional vector with treatment labels in the net graph
#' @param font.size font size
#' @param pos a [0, 1] specifying the position of the number of studies on the lines connecting treatments (edges)
#' @param labels label of the whole plots
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
#'
#' sucra1 <-SUCRA(bmt1)
#'
#' trt1$label <- paste0(trt1$id,"-", trt1$description)
#' LDT1$label <- factor(LDT1$treatment, labels = trt1$label)
#'
#'
#' nmt1 <- model_netmeta(long.data = LDT1,
#'                       treatment=LDT1$treatment,
#'                       id.treatments = trt1,
#'                       reference = "A",
#'                       outcome = "HR")
#'
#' # View(nmt1)
#'
#' net_score_plot(
#' nmt1, sucra1,
#' lab=trt1$description,
#' scl=1.1,ofs =0,
#' font.size = 0.3, pos=0.5,
#' labels="")
#'

net_score_plot <- function(
  nmt,
  sucra,
  scl=1,
  ofs=0.05,
  lab,
  font.size,
  pos = 0.5,
  labels=""){

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
