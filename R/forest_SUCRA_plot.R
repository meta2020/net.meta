#' To give the forest plot with SUCRA of bayesian results from model_gemtc
#'
#' @title Forest-SUCRA plot of net-meta model for MD, HR, and RR
#'
#' @return ggplot object
#'
#'
#' @param bmt bayesian net-meta result from model_gemtc
#' @param x.lab label in the x-axis
#' @param title.size title size
#' @param font.size font size
#' @param x1 the left xlim
#' @param x2 the right xlim
#' @param digits digits of the results
#' @param sucra SUCRA values
#' @param labels label of the whole plot
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
#' sucra<-SUCRA(bmt1)
#'
#' p <- forest_SUCRA_plot(
#' bmt=bmt1,
#' x.lab="X caption",
#' title.size =15,
#' font.size =5,
#' x1=0,
#' x2=40,
#' digits = 2,
#' sucra=sucra,
#' labels="Title")
#'

forest_SUCRA_plot <- function(
  bmt,
  x.lab,
  title.size =15,
  font.size =5,
  x1,
  x2,
  digits = 2,
  sucra,
  labels){

  if(bmt$outcome=="MD"){
    df <- data.frame(
      mean = bmt[["relative.effect"]][["summaries"]][["statistics"]][,1], #stat$Mean
      low  = bmt[["relative.effect"]][["summaries"]][["quantiles"]][,1], #quan$`2.5%`
      up   = bmt[["relative.effect"]][["summaries"]][["quantiles"]][,5]
    ) #quan$`97.5%`
    vline=0
    tab.title = "Mean Difference (95% CrI)"
  }

  if(bmt$outcome=="HR"){
    df <- data.frame(
      mean = exp(bmt[["relative.effect"]][["summaries"]][["statistics"]][,1]), #stat$Mean
      low  = exp(bmt[["relative.effect"]][["summaries"]][["quantiles"]][,1]), #quan$`2.5%`
      up   = exp(bmt[["relative.effect"]][["summaries"]][["quantiles"]][,5])
    )#quan$`97.5%`
    vline=1
    tab.title = "Hazard Ratio (95% CrI)"
  }

  if(bmt$outcome=="RR"){
    df <- data.frame(
      mean = exp(bmt[["relative.effect"]][["summaries"]][["statistics"]][,1]), #stat$Mean
      low  = exp(bmt[["relative.effect"]][["summaries"]][["quantiles"]][,1]), #quan$`2.5%`
      up   = exp(bmt[["relative.effect"]][["summaries"]][["quantiles"]][,5])
    )#quan$`97.5%`
    vline=1
    tab.title = "Risk Ratio (95% CrI)"
  }

  ## treatment label
  labs <- bmt$treatment$description[!bmt$treatment$description %in% bmt$ref.treatment$description]
  df$label <- reorder(labs, order(labs))
  df$point_est <- 1:(nrow(bmt$treatment)-1)

  ## plot
  plot <- ggplot(df,
                 aes(x=point_est, y = reorder(label, desc(label)))) +
    geom_vline(xintercept = vline, linetype="dashed",size=0.5, alpha=0.5)+
    geom_errorbarh(aes(xmin=low, xmax=up), height=.2) +
    geom_point(aes(x=mean),
               shape=23,size=5,
               fill="midnightblue", color="midnightblue") +
    ggtitle(paste0("Compared with ", bmt$ref.treatment$description)) + #"Compared with iNPH(bid)"
    ylab(NULL) +
    xlab(x.lab) +
    xlim(x1,x2)+
    theme_tufte()+
    theme(text = element_text(size = title.size,family = "Helvetica")) #20


  tab_base <- ggplot(df, aes(y = reorder(label, desc(label)))) +
    ylab(NULL) + xlab("  ") +
    theme(plot.title = element_text(hjust = 0.5, size=title.size, family = "Helvetica"), ## centering title on text
          axis.text.x=element_text(color="white"), ## need text to be printed so it stays aligned with figure but white so it's invisible
          axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          text = element_text(size = 14,family = "Helvetica"))

  tab <- tab_base +
    geom_text(aes(x=1,
                  label=paste0(round(mean,digits), " (",round(low, digits),", ",round(up,digits),")")),
              family="Helvetica", size=font.size) + #6
    ggtitle(tab.title) #"Mean Difference (95% CrI)"


  lay <-  matrix(c(1,1,2), nrow=1)
  p1<-grid.arrange(plot, tab, layout_matrix=lay)


  p2 <-ggplot(sucra, aes(y = sucra, x = 0, label = treatment)) +
    geom_vline(xintercept = 0)+
    geom_point(color = "red") +
    ylim(0,1) +
    ylab("")+
    xlab("")+
    theme_tufte()+
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.title.x = element_blank(),
      plot.title   = element_text(hjust = 0, size=8)
    )+
    xlim(0,1)+
    geom_text_repel(
      nudge_x      = 0.1,
      direction    = "y",
      hjust        = 0,
      segment.size = 0.2,
      family="Helvetica",
      size = font.size
    )+ ggtitle("SUCRA")

  plot_grid(p1, p2, labels = labels, rel_widths = c(2, 1))

}
