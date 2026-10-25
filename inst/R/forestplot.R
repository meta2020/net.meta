forestcinema<-function(nmaresults,civ,domain,sm){
  
  require(ggplot2)
  require(grid)

  #####################set values#############################
  
  if(sm=="OR"| sm=="RR" | sm=="HR"){
    NetwTE=exp(nmaresults[,"NMA treatment effect"])
    NetworkLCI=exp(nmaresults[,"lower CI"])
    NetworkUCI=exp(nmaresults[,"upper CI"])
    NetworkLRCI=exp(nmaresults[,"lower PrI"])
    NetworkURCI=exp(nmaresults[,"upper PrI"])
    comparisons=rownames(nmaresults)
    steps=length(NetwTE):1
    steps0=steps-0.2
    Effects=data.frame(NetwTE,steps,steps0,comparisons)
    
    NetwCI=c(NetworkLCI,NetworkUCI)
    NetwRCI=c(NetworkLRCI,NetworkURCI)
    NetwEffect=c(NetwTE,NetwTE)
    StepsForPlot=c(steps,steps)
    StepsForPlot0=c(steps0,steps0)
    ForReapPlot=data.frame(NetwCI,NetwRCI,NetwEffect,StepsForPlot,StepsForPlot0)
    
    noeffect=1
    
    if(civ<1){
      lciv=civ
      uciv=1/civ
    }
    if(civ>1){
      lciv=1/civ
      uciv=civ
    }
    if(civ==1){lciv=uciv=1}
  }

  if (sm=="MD" | sm=="SMD" | sm=="RD"){
    NetwTE=nmaresults[,"NMA treatment effect"]
    NetworkLCI=nmaresults[,"lower CI"]
    NetworkUCI=nmaresults[,"upper CI"]
    NetworkLRCI=nmaresults[,"lower PrI"]
    NetworkURCI=nmaresults[,"upper PrI"]
    comparisons=rownames(nmaresults)
    steps=length(NetwTE):1
    steps0=steps-0.2
    Effects=data.frame(NetwTE,steps,steps0,comparisons)
    
    NetwCI=c(NetworkLCI,NetworkUCI)
    NetwRCI=c(NetworkLRCI,NetworkURCI)
    NetwEffect=c(NetwTE,NetwTE)
    StepsForPlot=c(steps,steps)
    StepsForPlot0=c(steps0,steps0)
    ForReapPlot=data.frame(NetwCI,NetwRCI,NetwEffect,StepsForPlot,StepsForPlot0)
    
    noeffect=0
    
    if(civ<0){
      lciv=civ
      uciv=-civ
    }
    if(civ>0){
      lciv=-civ
      uciv=civ
    }
    if(civ==0){lciv=uciv=0}

  }
  
  ############################forest plots################################
  
  if(domain=="imprecision"){
    p=ggplot(Effects)+
      geom_point(aes(Effects$NetwTE,Effects$steps),na.rm = T) +
      theme(plot.margin = unit(c(1,14,1,1), "lines")) +
    geom_line(data=ForReapPlot,aes(NetwCI,StepsForPlot,group=StepsForPlot),na.rm = T) +
    geom_vline(xintercept = noeffect) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank())

    m1=ggplot_build(p)$layout$panel_ranges[[1]]$x.range[2]+0.1*ggplot_build(p)$layout$panel_ranges[[1]]$x.range[2]

    p = p + annotate("rect", xmin = lciv, xmax = uciv, ymin = min(steps)-1, ymax = max(steps)+1, alpha = .2)


    for (i in 1:length(Effects$comparisons))  {
      p <- p + annotation_custom(
        grob = textGrob(label = Effects$comparisons[i], just="left"),
        ymin = Effects$steps[i],      
        ymax = Effects$steps[i],
        xmin = m1,
        xmax = m1
        )
    }    
  
  }
  
  if(domain=="heterogeneity"){
    p=ggplot(Effects)+
      geom_point(aes(Effects$NetwTE,Effects$steps),na.rm = T)+            
      theme(plot.margin = unit(c(1,14,1,1), "lines")) 
  
    p=p+geom_line(data=ForReapPlot,aes(NetwRCI,StepsForPlot,group=StepsForPlot),colour="red",na.rm = T)
    p=p+geom_line(data=ForReapPlot,aes(NetwCI,StepsForPlot,group=StepsForPlot),na.rm = T)
    p=p+geom_vline(xintercept = noeffect)
    
    p=p + theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.title.x=element_blank())
    
    m1=ggplot_build(p)$layout$panel_ranges[[1]]$x.range[2]+0.1*ggplot_build(p)$layout$panel_ranges[[1]]$x.range[2]
    
    p= p + annotate("rect", xmin = lciv, xmax = uciv, ymin = min(steps)-1, ymax = max(steps)+1,
                    alpha = .2)
    
    for (i in 1:length(Effects$comparisons))  {
      p <- p + annotation_custom(
        grob = textGrob(label = Effects$comparisons[i], just="left"),
        ymin = Effects$steps[i],      
        ymax = Effects$steps[i],
        xmin = m1,         
        xmax = m1)
    }   
    
  }

  p
  # Code to override clipping
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
}
