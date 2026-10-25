
getRandomComparisonContribution <- function(c1, comparison){

  library(igraph)
  library(rlist)
  tol = 1e-6

  # c1 <- getHatMatrix (indata,type,model,tau, sm)
  directs <- c1$colNames

  hatMatrix <- c1$H
  
  rownames(hatMatrix) <- c1$rowNames

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
  # print(V(g)$label)
  # print(E(g)$label)

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

  contribution = rep(0,dims[2])
  names(contribution) <- c(1:dims[2])
  streams = list()

  reducePath <- function (g,comparison,spl) {
    pl <- length(spl)
    splE <- lapply(spl, function(e){
       return (E(g)[e[]])
    })
    flow <- min(unlist(lapply(splE, function(e){
      return(e$flow[])
    })))
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
      tpath = toString(names(unlist(lapply(spl,function(e){c(head_of(g,e),tail_of(g,e))}))))
      if(length(streams)>0){
	    if(tail(streams,1)[[1]]$stream!=tpath){
	      streams <<- list.append(streams,data.frame(comp=comparison,length=floor(length(splE)),stream=tpath,flow=flow))
	    }
      }else{
	streams <<- list.append(streams,data.frame(comp=comparison,length=floor(length(splE)),stream=tpath,flow=flow))
      }
      return(set.edge.attribute(g,"weight",e, cw))
    },splE, g)
    # print(c("graph before deleting edges", E(gg)$label))
    emptyEdges <- Reduce(function(removedEdges, e){
      e <- E(gg)[e[]]
      if(abs(e$flow[[1]][[1]])<tol){
        removedEdges <- c(removedEdges, e)
      }
      return(removedEdges)
    },splE, c())
    # print(c("edges to be removed",emptyEdges))
   return(delete_edges(gg, emptyEdges))
    # print(c("graph after deleting edges", E(gg)$label))
  }
  getOutEdges = function (g,v) {
    insds = incident_edges(g,v,mode="out")
    out = if (length(insds[[1]])>0){
      Filter(function(e){
       abs(E(g)[e[[1]]])$flow>tol
        },
       insds[[1]])
    }else{
      list()
    }
    return(out)
  }
  randomStream = function(g,s,t) {
    neis = getOutEdges(g,s)
    if(length(neis)>0){
      firstEdge = if(length(neis)==1){
          neis[1]
        }else{
          sample(neis,1)
        }
      getpath=function(acc){
        e = tail(acc,1)
        v = tail_of(g,e);
        if(V(g)[v]==V(g)[t]){
          return(acc)
        }else{
          newneis = getOutEdges(g,v)
            if(length(newneis)==1){
              newacc = list.append(acc,newneis[1])
            }else{
              if(length(newneis)!=0){
                randomedge = sample(newneis,1)
                newacc = list.append(acc,randomedge)
              }else{
                browser()
                newacc = acc
              }
            }
            return(getpath(newacc))
        }
      }
      outpath = getpath(list(firstEdge))
      return(outpath)
    }else{
      return(list())
    }
  }

 reduceGraph <- function (g,comparison) {
     #while(edge_connectivity(g,sv(comparison),tv(comparison))>0){
    spath = randomStream(g,sv(comparison),tv(comparison))
    if(length(unlist(spath))==0){
      return(g)
    }else{
      gg = reducePath(g,comparison,spath)
      return(reduceGraph(gg,comparison))
    }
  }

  # ptm <- proc.time()
  # gg <- reduceGraph (initRowGraph(comparison), comparison)
  reduceGraph (initRowGraph(comparison), comparison)
  # executionTime <- proc.time() - ptm
  # print(c("execution time",executionTime))

  names(contribution) <- directs
  contribution <- 100 * contribution
  #if(sum(contribution) != 100){
    #browser()
    #stop("contribution not adding up to 100")
  #}else{
    # return(list(gg=gg,g=dg,hatMatrix=c1,contribution=contribution))
    return(list(contribution=contribution,
                streams = streams,
                names=directs))
  #}
}

