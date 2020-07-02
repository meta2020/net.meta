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
#' @param title.size sucra title size
#' @param sucra.text.size sucra text size
#' @param pos a [0, 1] specifying the position of the number of studies on the lines connecting treatments (edges)
#' @param labels label of the whole plots
#' @param font.family "Helvetica" or "Times New Roman"
#' @param net.scale the scaled size of the net plot
#' @param plot.scale the scaled size of the sucra plot
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
#' p<- net_score_plot(
#' nmt1, sucra1,
#' lab=trt1$description,
#' scl=1.1,ofs =0,
#' font.size = 1,
#' pos=0.5,
#' title.size=10,
#' sucra.text.size=4,
#' font.family="Helvetica",
#' labels="")
#'

net_score_plot <- function(
  nmt,
  sucra,
  scl=1,
  ofs=0.05,
  lab,
  font.size,
  title.size,
  sucra.text.size,
  pos = 0.5,
  labels="",
  font.family = c("Helvetica", "Times New Roman"),
  net.scale=1.1,
  plot.scale=0.7){

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
  cex=font.size,
  family= "sans")

par(op)

p1 <- recordPlot()

p2 <-ggplot(sucra, aes(y = sucra, x = 0, label = treatment)) +
  geom_vline(xintercept = 0)+
  geom_point(color = "red") +
  ylim(0,1) +
  ylab("")+
  xlab("")+
  theme_tufte()+
  ggtitle("SUCRA")+
  theme(
    text = element_text(family = font.family),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0, size=title.size, family = font.family)
  )+
  xlim(0,1)+
  geom_text_repel(
    nudge_x      = 0.1,
    direction    = "y",
    hjust        = 0,
    segment.size = 0.2,
    family=font.family,
    size = sucra.text.size
  )

plot_grid(p1, p2, #layout_matrix=lay,
          labels = labels,
          #align="h",
          nrow = 1,
          rel_widths = c(1.5, 1),
          #rel_heights = 1,
          scale = c(net.scale,plot.scale),
          label_fontfamily = font.family)

}
