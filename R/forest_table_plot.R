#' To give the forest plot with table of bayesian results from model_gemtc or
#' combined forest plot of bayesian results, frequentist results, and prediction interval from model_gemtc and model_netmeta.
#'
#' @title Forest-table plot of net-meta model for MD, HR, and RR
#'
#' @return ggplot object
#'
#' @importFrom stats reorder
#' @importFrom ggplot2 geom_text geom_errorbarh
#' @importFrom gridExtra grid.arrange
#' @importFrom plyr desc
#'
#' @param type the type of plots, bayesian-bayesian result; all-bayesian, freq, and prediction interval
#' @param bmt bayesian net-meta result from model_gemtc
#' @param nmt frequentist net-meta result from model_nemeta
#' @param digits digits of the results
#' @param x1 the left xlim
#' @param x2 the right xlim
#' @param x.lab label in the x-axis
#' @param title.size title size
#' @param text.size font size
#' @param point.size point size
#' @param labels label of the whole plot
#' @param font.family "Helvetica" or "Times New Roman"
#' @param plot.scale the scaled size of the whole plot
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
#' forest_table_plot(
#' type="bayesian",
#' bmt=bmt1,
#' x.lab = "X caption",
#' x1=0,
#' x2=40,
#' title.size =12,
#' text.size =4,
#' digits = 2,
#' font.family="Helvetica")
#'
#' nmt1 <- model_netmeta(long.data = LDT1,
#'                       treatment=LDT1$treatment,
#'                       id.treatments = trt1,
#'                       reference = "A",
#'                       outcome = "HR")
#'
#' # View(nmt1)
#'
#' forest_table_plot(
#' type="all",
#' bmt=bmt1,
#' nmt=nmt1,
#' x.lab = "X caption",
#' x1=0,
#' x2=50,
#' title.size =12,
#' text.size =4,
#' digits = 2)
#'

forest_table_plot <- function(
  type=c("bayesian", "all"),
  bmt,
  nmt=NULL,
  digits = 2,
  x1,
  x2,
  x.lab,
  title.size,
  text.size,
  point.size=4,
  labels=NULL,
  plot.scale=0.9,
  font.family = "Helvetica"
  ) {

  type <- match.arg(type)
  if (type=="bayesian"){

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
    shape=23,size=point.size,
    fill="midnightblue", color="midnightblue") +
  ggtitle(paste0("Compared with ", bmt$ref.treatment$description)) + #"Compared with iNPH(bid)"
  ylab(NULL) +
  xlab(x.lab) +
  xlim(x1,x2)+
  theme_tufte()+
  theme(text = element_text(size=title.size, family = font.family),
        plot.title = element_text(hjust = 0, size=title.size, family = font.family)) #20


tab_base <- ggplot(df, aes(y = reorder(label, desc(label)))) +
  ylab(NULL) + xlab("  ") +
  theme(plot.title = element_text(hjust = 0.5, size=title.size, family = font.family), ## centering title on text
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
        text = element_text(size = 14,family = font.family))

tab <- tab_base +
  geom_text(aes(x=1,
    label=paste0(round(mean,digits), " (",round(low, digits),", ",round(up,digits),")")),
    family=font.family, size=text.size) + #6
  ggtitle(tab.title) #"Mean Difference (95% CrI)"



plot_grid(plot, tab,
             labels = labels,
             align="h",
             nrow = 1,
             rel_widths = c(2, 1),
             #rel_heights = 1,
             scale = c(plot.scale,plot.scale),
             label_fontfamily = font.family)

  }

  else{

    rm.id <- which(nmt$ref.treatment$id==bmt$treatment$id)

    if(bmt$outcome=="MD"){
      df <- data.frame(
        mean = bmt[["relative.effect"]][["summaries"]][["statistics"]][,1], #stat$Mean
        low  = bmt[["relative.effect"]][["summaries"]][["quantiles"]][,1],  #quan$`2.5%`
        up   = bmt[["relative.effect"]][["summaries"]][["quantiles"]][,5],

        mean2 <- nmt[["TE.random"]][-rm.id,rm.id],
        low2  <- nmt[["lower.random"]][-rm.id,rm.id],
        up2   <- nmt[["upper.random"]][-rm.id,rm.id],

        low3  <- nmt[["lower.predict"]][-rm.id,rm.id],
        up3   <- nmt[["upper.predict"]][-rm.id,rm.id]
      )
      vline=0
      tab.title1 = "B.MD"
      tab.title2 = "F.MD"
    }

    if(bmt$outcome=="HR"){
      df <- data.frame(
        mean = bmt[["relative.effect"]][["summaries"]][["statistics"]][,1], #stat$Mean
        low  = bmt[["relative.effect"]][["summaries"]][["quantiles"]][,1], #quan$`2.5%`
        up   = bmt[["relative.effect"]][["summaries"]][["quantiles"]][,5],

        mean2 = nmt[["TE.random"]][-rm.id,rm.id],
        low2  = nmt[["lower.random"]][-rm.id,rm.id],
        up2   = nmt[["upper.random"]][-rm.id,rm.id],

        low3  = nmt[["lower.predict"]][-rm.id,rm.id],
        up3   = nmt[["upper.predict"]][-rm.id,rm.id]
      )
      df <- exp(df)
      vline=1
      tab.title1 = "B.HR"
      tab.title2 = "F.HR"
    }

    if(bmt$outcome=="RR"){
      df <- data.frame(
        mean = bmt[["relative.effect"]][["summaries"]][["statistics"]][,1], #stat$Mean
        low  = bmt[["relative.effect"]][["summaries"]][["quantiles"]][,1], #quan$`2.5%`
        up   = bmt[["relative.effect"]][["summaries"]][["quantiles"]][,5],

        mean2 = nmt[["TE.random"]][-rm.id,rm.id],
        low2  = nmt[["lower.random"]][-rm.id,rm.id],
        up2   = nmt[["upper.random"]][-rm.id,rm.id],

        low3  = nmt[["lower.predict"]][-rm.id,rm.id],
        up3   = nmt[["upper.predict"]][-rm.id,rm.id]
      )
      df <- exp(df)
      vline=1
      tab.title1 = "B.RR"
      tab.title2 = "F.RR"
    }

    ## treatment label
    labs <- bmt$treatment$description[!bmt$treatment$description %in% bmt$ref.treatment$description]
    df$label <- reorder(labs, desc(labs))
    df$point_est <- 1:(nrow(bmt$treatment)-1)

    ## plot
    ## forest plot
    plot <- ggplot(df, aes(x=point_est, y=label)) +
      geom_vline(xintercept = vline, linetype="dashed",size=0.5, alpha=0.5)+
      geom_errorbarh(aes(xmin=low, xmax=up), height=.2, na.rm = TRUE) +
      geom_point(aes(x=mean),shape=24,size=point.size, fill="midnightblue",color="midnightblue", alpha=0.5) +
      ggtitle(paste0("Compared with ", bmt$ref.treatment$description))+
      ylab(NULL) +
      xlab(x.lab) +
      xlim(x1,x2)+
      theme_tufte()+
      theme(text = element_text(size = title.size,family = font.family),
            plot.title = element_text(hjust = 0, size=title.size, family = font.family))+
      geom_errorbarh(aes(xmin=low3, xmax=up3), height=.2, color="blue") +
      geom_errorbarh(aes(xmin=low2, xmax=up2), height=.2, color="red") +
      geom_point(aes(x=mean2),shape=25,size=point.size, fill="red",color="red", alpha=0.5)

    ## table
    tab_base <- ggplot(df, aes(y=label)) +
      ylab(NULL) + xlab("  ") +
      theme(plot.title = element_text(hjust = 0.5, size=title.size, family = font.family), ## centering title on text
            axis.text.x=element_text(color="white"), ## need text to be printed so it stays aligned with figure but white so it's invisible
            axis.line=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())

    tab <- tab_base +
      geom_text(aes(x=1,
                    label=paste0(round(mean,digits), " (",round(low, digits),",",round(up,digits),")")),
                family=font.family,
                size=text.size) +
      ggtitle(paste0(tab.title1, " (95% CrI)"))

    tab2 <- tab_base +
      geom_text(aes(x=1,
                    label=paste0(round(mean2,digits), " (",round(low2, digits),",",round(up2,digits),")")),
                family=font.family,
                size=text.size, color="red") +
      ggtitle(paste0(tab.title2, " (95% CI)"))

    tab3 <- tab_base +
      geom_text(aes(x=1,
                    label=paste0(" (",round(low3, digits),",",round(up3,digits),")")),
                family=font.family, size=text.size, color="blue") +
      ggtitle("(95% PI)")

    #lay <-  matrix(c(1,1,1,2,3,4), nrow=1)
    #p<-grid.arrange(plot, tab,tab2,tab3, layout_matrix=lay)
    plot_grid(plot, tab, tab2, tab3,
              labels = labels,
              align="h",
              nrow = 1,
              rel_widths = c(2, 1,1,1),
              #rel_heights = 1,
              scale = rep(plot.scale,4),
              label_fontfamily = font.family)


  }

}



