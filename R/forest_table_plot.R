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
#' @param bmt bayesian net-meta result from model_gemtc
#' @param nmt frequentist net-meta result from model_nemeta
#' @param x.lab label in the x-axis
#' @param title.size title size
#' @param font.size font size
#' @param x1 the left xlim
#' @param x2 the right xlim
#' @param digits digits of the results
#' @param type the type of plots, bayesian-bayesian result; all-bayesian, freq, and prediction interval
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
#' p<-forest_table_plot(
#' bmt=bmt1,
#' x.lab = "X",
#' x1=0,
#' x2=40,
#' title.size =15,
#' font.size =5,
#' digits = 2,
#' type="bayesian")
#'
#' nmt1 <- model_netmeta(long.data = LDT1,
#'                       treatment=LDT1$treatment,
#'                       id.treatments = trt1,
#'                       reference = "A",
#'                       outcome = "HR")
#'
#' # View(nmt1)
#'
#' p<-forest_table_plot(
#' bmt=bmt1,
#' nmt=nmt1,
#' x.lab = "X",
#' x1=0,
#' x2=50,
#' title.size =15,
#' font.size =5,
#' digits = 2,
#' type="all")
#'

forest_table_plot <- function(
  bmt,
  nmt=NULL,
  x.lab,
  title.size =15,
  font.size =5,
  x1,
  x2,
  digits = 2,
  type=c("bayesian", "all")
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
p<-grid.arrange(plot, tab, layout_matrix=lay)

p
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
    plot <- ggplot(df, aes(x=df$point_est, y=df$label)) +
      geom_vline(xintercept = vline, linetype="dashed",size=0.5, alpha=0.5)+
      geom_errorbarh(aes(xmin=low, xmax=up), height=.2) +
      geom_point(aes(x=mean),shape=24,size=5, fill="midnightblue",color="midnightblue", alpha=0.5) +
      ggtitle(paste0("Compared with ", bmt$ref.treatment$description))+
      ylab(NULL) +
      xlab(x.lab) +
      xlim(x1,x2)+
      theme_tufte()+
      theme(text = element_text(size = title.size,family = "Helvetica"))+
      geom_errorbarh(aes(xmin=low3, xmax=up3), height=.2, color="blue") +
      geom_errorbarh(aes(xmin=low2, xmax=up2), height=.2, color="red") +
      geom_point(aes(x=mean2),shape=25,size=5, fill="red",color="red", alpha=0.5)

    ## table
    tab_base <- ggplot(df, aes(y=label)) +
      ylab(NULL) + xlab("  ") +
      theme(plot.title = element_text(hjust = 0.5, size=title.size, family = "Helvetica"), ## centering title on text
            axis.text.x=element_text(color="white"), ## need text to be printed so it stays aligned with figure but white so it's invisible
            axis.line=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())

    tab <- tab_base +
      geom_text(aes(x=1,
                    label=paste0(round(mean,digits), " (",round(low, digits),",",round(up,digits),")")),
                family="Helvetica",
                size=font.size) +
      ggtitle(paste0(tab.title1, " (95% CrI)"))

    tab2 <- tab_base +
      geom_text(aes(x=1,
                    label=paste0(round(mean2,digits), " (",round(low2, digits),",",round(up2,digits),")")),
                family="Helvetica", size=font.size, color="red") +
      ggtitle(paste0(tab.title2, " (95% CI)"))

    tab3 <- tab_base +
      geom_text(aes(x=1,
                    label=paste0(" (",round(low3, digits),",",round(up3,digits),")")),
                family="Helvetica", size=font.size, color="blue") +
      ggtitle("(95% PI)")

    lay <-  matrix(c(1,1,1,2,3,4), nrow=1)
    p<-grid.arrange(plot, tab,tab2,tab3, layout_matrix=lay)

    p
  }

}



