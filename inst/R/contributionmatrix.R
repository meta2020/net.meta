#' Contribution matrix in network meta-analysis
#'
#' This function takes a csv file and computes the contribution matrix of a network meta-analysis
#' @param data a csv file that can be of type "netwide_binary" with required variables (id t1 t2 r1 r2 n1 n2),
#' "netwide_continuous" with required variables (id t1 t1 y1 y2 sd1 sd1 n1 n2) or
#' "iv" (standing for inverse variance) with required variables (id t1 t2 effect se)
#' @param type The type of the data which can be either "netwide_binary", "netwide_continuous" or "iv" (see data)
#' @param model Specified whether the fixed or the random effects model is going to be applied (fixed is the default.)
#' If random effects model is chosen, DerSimonian and Laird is used to estimate heterogeneity.
#' @param tau A user specified value for heterogeneity standard deviation (not implemented yet).
#' @param sm The effect size to be used. For binary outcomes, it can be "OR", "RR", "RD", or "ASD".
#' For continuous outcomes, it can be "MD" or "SMD". For iv, it can be either of the above.
#' @return The percentages contributions of each direct to each network estimate and to the entire network.
#' @return percentageContr The percentages contributions of each direct to each network estimate
#' @return impD The percentages contributions of each direct to the entire network.
#' @export


getContributionMatrix <- function(indata,type,model="fixed",tau=NA, sm){

  library(igraph)

  c1 <- getHatMatrix (indata,type,model,tau, sm)
  hatMatrix <- c1$H

  split <- function (dir) {strsplit(dir,":")}

  directs <- c1$colNames

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
  # print(V(g)$label)
  # print(E(g)$label)

  setFlow <- function (g,comparison,conMat) {
    flows<-abs(conMat[comparison,])
    set.edge.attribute(g,"flow",value=flows)
  }

  setWeights <- function (g,comparison,conMat) {
    set.edge.attribute(g,"weight",value=rep(0,dims[2]))
  }

  # initRowGraph <- function(comparison) {
  #   gg <- setFlow(g,comparison,hatMatrix)
  #   E(gg)$weight <- rep(0,dims[2])
  #   gg
  # }

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

  getFlow <- function(g,edge) {
    fl<-E(g)[edge]$flow
    # print(c("edge",edge,"flow",fl))
    fl
  }

  sv <- function (comparison) {
    split(comparison)[[1]][1][1]
  }

  tv <- function (comparison) {
    split(comparison)[[1]][2][1]
  }


  # spath <- function(g,comparison) {
    # shortest_paths(g, sv(comparison), tv(comparison) ,output="epath",weights=NA)$epath
  # }

#test1 outbound contribution from source should add up to 1
# test1 <- Reduce(function(w, e) {w+getFlow(rowgraph,e[])},incident(rowgraph,sv(row)),0)

#contribution Matrix dimensions

  weights <- matrix (
                     rep(0,dims[2]*dims[1]),
                     nrow=dims[1],
                     ncol=dims[2],
                     byrow=TRUE
                 )
  rownames(weights) <- rownames(hatMatrix)
  colnames(weights) <- c(1:dims[2])

  reducePath <- function (g,comparison,spl) {
    pl <- length(spl[[1]])
    splE <- lapply(spl[[1]], function(e){
       return (E(g)[e[]])
    })
    flow <- min(unlist(lapply(splE, function(e){
      return(e$flow[])
    })))
    # print(c("to shortest path einai :",spl))
    gg <- Reduce(function(g, e){
      elabel <- e$label
      # print(c("pame plevra:",e,"dld",E(g)[e[]]$label))
      pfl <- e$flow[]
      g <- set.edge.attribute(g,"flow",e, pfl-flow)
      # print(c("h e",e,"einai pragmatika h ",g))
      cw <-  e$weight[] + (flow[1]/pl) 
      weights[comparison,elabel] <<- cw
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
      return(get.shortest.paths(g,sv(comparison),tv(comparison),output="epath",weights=NA)$epath)
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

  ptm <- proc.time()
  lapply (comparisons, function (comp) {
    reduceGraph (initRowGraph(comp), comp)
  })
  executionTime <- proc.time() - ptm
  # print(c("execution time",executionTime))

  colnames(weights) <- directs
  weights <- 100 * weights
  totalSums <-colSums(weights)
  totalTotal <- sum(totalSums)
  totalWeights <- unlist(lapply(totalSums,function(comp){
                           100 * comp/ totalTotal
  }))
  return(list(hatMatrix=c1,contributionMatrix=weights,totalWeights=totalWeights))
}

