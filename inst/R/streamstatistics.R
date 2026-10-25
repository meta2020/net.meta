#### Example:
## read data
#wideData = read.csv2("griselda_wide.csv",header=TRUE,sep=",")
## convert to long because getHatMatrix only accepts long
#indata = wide2long(wideData,"binary")
#C = getHatMatrix(indata,type="long_binary",model="random",sm="OR")
## get stream statistics
#cl = streamStatistics(C)
#plot(cl$contributionperlength)
#plot(cl$cummulativeContributionPerStream)
#streamHistogram(cl$lengthfrequency)

getdof = function(hatmatrix){
  library(rlist)
  library(igraph)

  directs <- hatmatrix$colNames

  hatMatrix <- hatmatrix$H
  
  rownames(hatMatrix) <- hatmatrix$rowNames

  split <- function (dir) {strsplit(dir,":")}

  dims <- dim(hatMatrix)

#rows of comparison matrix 
  comparisons <- unlist(lapply(rownames(hatMatrix),unlist))

  comparisonToEdge <- function (comp) unlist (split(comp))

  directlist <- unlist(lapply(lapply(directs,split),unlist))
  # print(c("dir",directs))

  edgeList <- matrix( directlist, nc = 2, byrow = TRUE)
  # print(c("Edgelist"))
  # print(edgeList)

  g <- graph_from_edgelist(edgeList , directed=FALSE)
  g <- set.vertex.attribute(g,'label',value = V(g))
  g <- set.edge.attribute(g,'label',value = E(g))
   #print(V(g)$label)
   #print(E(g)$label)

   dof = length(E(g))-length(V(g))+1
  return (dof)
}



comparisonStreamlengths = function(hatmatrix, comparison){

  library(rlist)
  library(igraph)

  directs <- hatmatrix$colNames

  hatMatrix <- hatmatrix$H
  
  rownames(hatMatrix) <- hatmatrix$rowNames

  split <- function (dir) {strsplit(dir,":")}

  dims <- dim(hatMatrix)

#rows of comparison matrix 
  comparisons <- unlist(lapply(rownames(hatMatrix),unlist))

  comparisonToEdge <- function (comp) unlist (split(comp))

  directlist <- unlist(lapply(lapply(directs,split),unlist))
  # print(c("dir",directs))

  edgeList <- matrix( directlist, nc = 2, byrow = TRUE)
  # print(c("Edgelist"))
  # print(edgeList)

  g <- graph_from_edgelist(edgeList , directed=FALSE)
  g <- set.vertex.attribute(g,'label',value = V(g))
  g <- set.edge.attribute(g,'label',value = E(g))
   #print(V(g)$label)
   #print(E(g)$label)

  setWeights <- function (g,comparison,conMat) {
    set.edge.attribute(g,"weight",value=rep(0,dims[2]))
  }


  getFlow <- function(g,edge) {return(E(g)[edge]$flow)}

  sv <- function (comparison) {split(comparison)[[1]][1][1]}

  tv <- function (comparison) {split(comparison)[[1]][2][1]}

  initRowGraph <- function(comparison) {
    dedgeList <- lapply(1:length(directs),function(comp) {
       if(hatMatrix[comparison,comp]>0){
         # print(c("not switched",directs[comp],hatMatrix[comparison,comp]))
         return (c(sv(directs[comp]),tv(directs[comp])))
       }else{
         # print(c("switched",directs[comp],hatMatrix[comparison,comp]))
         return (c(tv(directs[comp]),sv(directs[comp])))
       }
    })
    dedgeList <- matrix( unlist(dedgeList), nc = 2, byrow = TRUE)
    # gg <- setFlow(g,comparison)
    # E(gg)$weight <- rep(0,dims[2])
    # return(gg)
    flows<-abs(hatMatrix[comparison,])
    dg <- graph_from_edgelist(dedgeList , directed = TRUE)
    E(dg)[]$weight <- rep(0,dims[2])
    E(dg)[]$flow <- abs(hatMatrix[comparison,])
    V(dg)[]$label <- V(dg)[]$name
    # E(dg)[]$label <- E(dg)[]$flow
    dg <- set.edge.attribute(dg,'label',value = E(dg))
    # print(c("isdirected",is.directed(dg)))
    return(dg)
  }
  # print(dedgeList)
#test flow conservation
  # dg <- initRowGraph(comparison)
  # plot(dg)


  # print(c("comparison:",comparison))
#test1 outbound contribution from source should add up to 1
# test1 <- Reduce(function(w, e) {w+getFlow(dg,e[])},incident(dg,sv(comparison),"out"),0)
# print(test1)
# test2 <- Reduce(function(w, e) {w+getFlow(dg,e[])},incident(dg,tv(comparison),"in"),0)
# print(test2)
# inflows <- lapply(V(dg),function(vx){
#   Reduce(function(w, e) {
#     fl <- getFlow(dg,e[])
#     return (w + fl)
#   },incident(dg,vx,mode="in"),0)})
# 
# outflows <- lapply(V(dg),function(vx){
#   Reduce(function(w, e) {
#     fl <- getFlow(dg,e[])
#     return (w + fl)
#   },incident(dg,vx,mode="out"),0)})
# 
# test3 <- unlist(inflows)-unlist(outflows)
# print(test3)
# print(test3==0)

# test5 <- lapply(V(dg),function(vx){
#   ins <- incident(dg,vx,"in")
#   outs <-incident(dg,vx,"out")
#   print(c('vertex',vx))
#   print(c("ins",ins,"outs",outs))
# })


  contribution = rep(0,dims[2])
  streams = list()
  names(contribution) <- c(1:dims[2])

  reducePath <- function (g,comparison,spl) {
    pl <- length(spl[[1]])
    splE <- lapply(spl[[1]], function(e){
       return (E(g)[e[]])
    })
    flow <- min(unlist(lapply(splE, function(e){
      return(e$flow[])
    })))
    streams <<- list.append(streams,data.frame(comparison=comparison,stream=floor(length(splE)),flow=flow))
    # print(c("to shortest path einai :",spl))
    gg <- Reduce(function(g, e){
      elabel <- e$label
      # print(c("pame plevra:",e,"dld",e$label))
      pfl <- e$flow[]
      g <- set.edge.attribute(g,"flow",e, pfl-flow)
      # print(c("h e",e,"einai pragmatika h ",elabel))
      cw <-  e$weight[] + (flow[1]/pl) 
      # print(c("flow",flow,"eweight",e$weight[]))
      contribution[elabel] <<- cw
      return(set.edge.attribute(g,"weight",e, cw))
    },splE, g)
    # print(c("graph before deleting edges", E(gg)$label))
    emptyEdges <- Reduce(function(removedEdges, e){
      e <- E(gg)[e[]]
      if(e$flow[[1]][[1]]==0){
        removedEdges <- c(removedEdges, e)
      }
      return(removedEdges)
    },splE, c())
    # print(c("edges to be removed",emptyEdges))
   return(delete_edges(gg, emptyEdges))
    # print(c("graph after deleting edges", E(gg)$label))
  }

 reduceGraph <- function (g,comparison) {
    getshortest <- function (g,compariston) {
      floweights = lapply(edge_attr(g,"flow",E(g)), function(f){return(abs(2-f))})
      spths = suppressWarnings(
                get.shortest.paths(g,sv(comparison),tv(comparison),mode="out",output="epath",weights=floweights)
                              )
      return(spths$epath)
    }
    # while(edge_connectivity(g,sv(comparison),tv(comparison))>0){
    spath <- getshortest(g,comparison)
    while(length(unlist(spath))>0){
      g <- reducePath(g,comparison,spath)
      spath <- getshortest(g,comparison)
    }
    # print("teleiwse")
    return(g)
  }

  # ptm <- proc.time()
  # gg <- reduceGraph (initRowGraph(comparison), comparison)
  reduceGraph (initRowGraph(comparison), comparison)
  # executionTime <- proc.time() - ptm
  # print(c("execution time",executionTime))

  names(contribution) <- directs
  contribution <- 100 * contribution
  
  return(streams)
}

streamStatistics = function (hatmatrix){
  library(dplyr)

  rownames = hatmatrix$rowNames

  out = mapply(function(comp){
                res = comparisonStreamlengths(hatmatrix,comp)
                return(streams=res)
              },rownames)
  print(out)
  dof = getdof(hatmatrix)

  getStreamNumber = function(comp){
             return(length(comp))
  }

  getStream = function(comp){
    lapply(comp, function(str){
             return(str$stream)
    })
  }

  getStream2 = function(comp){
    lapply(comp, function(str){
             return(data.frame(length=str$stream,comparison=str$comparison))
    })
  }

  getStreamFlow = function(comp){
    lapply(comp, function(str){
             return(str)
    })
  }

  histstr = unlist(lapply(out, getStream))
  histstr2 = lapply(out, getStream2)

  numOfStreams = unlist(lapply(out, getStreamNumber))

  stfl = unlist(lapply(out, function(s){getStreamFlow(s)}), recursive=F)

  groupByStream = function (lst){
    Reduce(function(acc,str){
             i = str[[2]]
             f = str[[3]]
             newacc = acc
             if (is.na(acc[i])) {
               newacc[i]=f
             }else{
               newacc[i]=acc[i]+f
             }
             return (newacc)
    }, lst, array(), right = FALSE, accumulate = F)
  }

                           
  flowperlength = groupByStream(stfl)
  totalFlow = sum(flowperlength)

  contributionperlength = mapply(function(cont){
                                   if(is.na(cont)){
                                           return(0)
                                        }else{
                                           return(cont/totalFlow)
                                        }
                                },flowperlength)


  return(list( contributionperlength = contributionperlength
             , cummulativeContributionPerStream = cumsum(contributionperlength)
             , lengthfrequency = histstr
             , lengthfrequency2 = histstr2
             , dof = dof
             , numOfStreams = numOfStreams
             ))
}

streamHistogram = function(lengthfrequency){
  dataset = lengthfrequency
  hist(dataset, breaks=seq(min(dataset)-0.5, max(dataset)+0.5, by=1)  )
}
