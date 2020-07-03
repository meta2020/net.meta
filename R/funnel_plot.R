#' To plot the funnel plot from frequentist net-meta
#'
#' @title Funnel plot from frequentist net-meta
#'
#' @return plot and ggplot objects
#'
#' @importFrom meta metabias metagen
#' @importFrom netmeta funnel.netmeta
#' @importFrom stats qnorm
#' @importFrom ggplot2 geom_polygon geom_segment scale_shape_manual labs annotate
#'
#' @param nmt results from model_netmeta function
#' @param font.size font size
#' @param text.size text size
#' @param text.x.position text position
#' @param cap caption
#' @param x1 left xlim
#' @param x2 right xlim
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
#' trt1$label <- paste0(trt1$id,"-", trt1$description)
#' LDT1$label <- factor(LDT1$treatment, labels = trt1$label)
#'
#' trt1$label <- paste0(trt1$id,"-", trt1$description)
#' LDT1$label <- factor(LDT1$treatment, labels = trt1$label)
#'
#' nmt.lab1 <- model_netmeta(
#'   long.data=LDT1[,-2],
#'   treatment=LDT1$label,
#'   id.treatments=trt1,
#'   reference = trt1$label[1],
#'   outcome="HR")
#'
#' p<-funnel_plot(
#'   nmt.lab1,
#'   font.size=10,
#'   text.size=4,
#'   text.x.position=0,
#'   x1=-1,
#'   x2=3,
#'   cap="HR",
#'   font.family = "Times New Roman"
#'   )
#'

funnel_plot <- function(
  nmt,                    ## freq model
  font.size,
  text.size,
  text.x.position,
  cap,
  x1,                     ## x-aix left
  x2,
  font.family = c("Helvetica", "Times New Roman")
  ){

  ## funnel plot data
  netmeta.f <- funnel.netmeta(nmt,
                      order = nmt$trts, legend = TRUE,
                      pos.legend = "bottomright", pos.tests = "topleft",
                      linreg = TRUE, rank = FALSE, mm = FALSE, digits.pval = 2)

  ## change treatment legend
  trt <- nmt$id.treatment
  if(sum(trt$label %in% netmeta.f$treat1)>0){
    treat1 <- factor(netmeta.f$treat1, labels = trt$description[trt$label %in% netmeta.f$treat1])
    treat2 <- factor(netmeta.f$treat2, labels = trt$description[trt$label %in% netmeta.f$treat2])
    netmeta.f$comparison <- factor(paste0(treat1,":",treat2), levels = unique(paste0(treat1,":",treat2)))
  }
  else{
    treat1 <- factor(netmeta.f$treat1)
    treat2 <- factor(netmeta.f$treat2)
    netmeta.f$comparison <- factor(paste0(treat1,":",treat2), levels = unique(paste0(treat1,":",treat2)))

  }

  ## p-value
  bias<-metabias(metagen(TE.adj, seTE, data = netmeta.f), k.min=9)

  ## set mean scale
  if (nmt$outcome=="MD"){mean=0}
  else{
    mean=1
    netmeta.f$TE.adj <- exp(netmeta.f$TE.adj)}

  ## triangle area
  ## 95% CI
  tri1 <- data.frame(x=c(mean-qnorm(0.025)*max(netmeta.f$seTE), mean, mean+qnorm(0.025)*max(netmeta.f$seTE)),
                     y=c(max(netmeta.f$seTE), 0, max(netmeta.f$seTE)))
  ## 99% CI
  tri2 <- data.frame(x=c(mean-qnorm(0.005)*max(netmeta.f$seTE), mean, mean+qnorm(0.005)*max(netmeta.f$seTE)),
                     y=c(max(netmeta.f$seTE), 0, max(netmeta.f$seTE)))

## plot
p<-ggplot(data=netmeta.f)+
  ylim(max(netmeta.f$seTE),0)+
  geom_polygon(data=tri2, mapping=aes(x=x, y=y), fill="#8c8c8c", alpha=0.2)+
  geom_polygon(data=tri1, mapping=aes(x=x, y=y), fill="#cccccc", alpha=0.5)+
  geom_segment(aes(x=mean,y=0,xend=mean,yend=max(netmeta.f$seTE)), linetype=2, size=0.1, alpha=0.2)+
  geom_segment(aes(x=mean,y=0,xend=mean+qnorm(0.025)*max(netmeta.f$seTE),yend=max(netmeta.f$seTE)), linetype=4, size=0.1,alpha=0.2)+
  geom_segment(aes(x=mean,y=0,xend=mean-qnorm(0.025)*max(netmeta.f$seTE),yend=max(netmeta.f$seTE)), linetype=4, size=0.1,alpha=0.2)+
  geom_segment(aes(x=mean,y=0,xend=mean+qnorm(0.005)*max(netmeta.f$seTE),yend=max(netmeta.f$seTE)), linetype=6, size=0.1, alpha=0.1)+
  geom_segment(aes(x=mean,y=0,xend=mean-qnorm(0.005)*max(netmeta.f$seTE),yend=max(netmeta.f$seTE)), linetype=6, size=0.1,alpha=0.1)+
  geom_point(aes(x=TE.adj, y=seTE, color=comparison, shape=comparison), size=3)+
  scale_shape_manual(values=1:nlevels(netmeta.f$comparison))+
  xlim(x1,x2)+
  labs(x=paste0(cap, " centered at comparison-specific effect"),
       y="Standard Error")+
  theme_tufte()+
  theme(text = element_text(size = font.size,family = font.family))+
  theme(legend.title=element_blank())+
  annotate("text", x = text.x.position, y = 0, label = paste0("P = ", round(bias$p.value,3), " (Egger's test)"),
           family=font.family, size=text.size)

p
}

