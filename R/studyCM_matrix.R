
## Contribution matrix from
## https://github.com/esm-ispm-unibe-ch/flow_contribution/tree/master/R
##


# ## H matrix
# Hkrahn=netmeta:::nma.krahn(nmt,tau.preset = nmt$tau)$H
# X.full=netmeta:::nma.krahn(nmt,tau.preset = nmt$tau)$X.full
# direct=netmeta:::nma.krahn(nmt,tau.preset = nmt$tau)$direct
# X=netmeta:::nma.krahn(nmt,tau.preset = nmt$tau)$X.full[direct$comparison,,drop=FALSE]
# Vd=diag(direct$seTE^2,nrow=length(direct$seTE),ncol=length(direct$seTE))
#
# H <- X.full %*% solve(t(X) %*% solve(Vd) %*% X) %*% t(X) %*% solve(Vd)
# colnames(H)<-rownames(X)
# round(H,4)
#
# source("getContributionMatrix.R")
# trt.CM<-getContributionMatrix(H)  ## from pkg, per comparison contribution matrix
#
#
#### source("getContributionMatrix.R")

getComparisonContribution <- function(H, comparison){

  #library(igraph)

  # hatmatrix <- getHatMatrix (indata,type,model,tau, sm)
  directs <- colnames(H)

  #hatMatrix <- H

  #rownames(hatMatrix) <- rownames(H)

  split <- function (dir) {strsplit(dir,":")}

  dims <- dim(H)

  #rows of comparison matrix
  comparisons <- unlist(lapply(rownames(H),unlist))

  comparisonToEdge <- function (comp) unlist (split(comp))

  directlist <- unlist(lapply(lapply(directs,split),unlist))
  # print(c("dir",directs))

  edgeList <- matrix(directlist, ncol = 2, byrow = TRUE)
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
      if(H[comparison,comp]>0){
        # print(c("not switched",directs[comp],hatMatrix[comparison,comp]))
        return (c(sv(directs[comp]),tv(directs[comp])))
      }else{
        # print(c("switched",directs[comp],hatMatrix[comparison,comp]))
        return (c(tv(directs[comp]),sv(directs[comp])))
      }
    })
    dedgeList <- matrix(unlist(dedgeList), ncol = 2, byrow = TRUE)
    # gg <- setFlow(g,comparison)
    # E(gg)$weight <- rep(0,dims[2])
    # return(gg)
    flows<-abs(H[comparison,])
    dg <- graph_from_edgelist(dedgeList , directed = TRUE)
    E(dg)[]$weight <- rep(0,dims[2])
    E(dg)[]$flow <- abs(H[comparison,])
    V(dg)[]$label <- V(dg)[]$name
    # E(dg)[]$label <- E(dg)[]$flow
    dg <- set.edge.attribute(dg,'label',value = E(dg))
    # print(c("isdirected",is.directed(dg)))
    return(dg)
  }

  contribution = rep(0,dims[2])
  names(contribution) <- c(1:dims[2])

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
      return(
        suppressWarnings(get.shortest.paths(g,sv(comparison),tv(comparison),mode="out",output="epath",weights=NA)$epath)
        )
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
  reduceGraph(initRowGraph(comparison), comparison)
  # executionTime <- proc.time() - ptm
  # print(c("execution time",executionTime))

  names(contribution) <- directs
  contribution <- 100 * contribution

  # return(list(gg=gg,g=dg,hatMatrix=hatmatrix,contribution=contribution))
  return(list(contribution=contribution,
              names=directs))
}

#### source("getStudyContribution.R")

getStudyContribution <- function(H, comparison, nmt) {
  contribution = getComparisonContribution(H, comparison)
  #model = hatmatrix$model
  # main data frame
  #dfr = hatmatrix$forstudycontribution
  dfr =  data.frame(studlab=nmt$studlab
                    , treat1=nmt$treat1
                    , treat2=nmt$treat2
                    , seTE=nmt$seTE
                    , seTE.adj=nmt$seTE.adj
  )
  #tau = 0
  #if (model=="random"){
  tau = nmt$tau
  #}
  dfr$comp = paste(dfr$treat1,dfr$treat2,sep=":")
  dfr$w.adj = 1 / ((dfr$seTE.adj)^2+(tau)^2)

  studyContribution = function (direct){
    aux = dfr[dfr$comp==direct,]
    normfac = sum(aux[,"w.adj"])
    per = contribution$contribution[direct]
    studycontr = lapply(row.names(aux),function(row){
      w = aux[row,"w.adj"]
      out = data.frame(study=''
                       , contribution=0
                       ,comparison='')
      out$study = aux[row,"studlab"]
      out$contribution = w * per / normfac
      out$comparison = direct
      return(out)
    })
    return(studycontr)
  }

  outlist = Reduce(function(acc,col){stw=studyContribution(col);return(c(acc,stw))},contribution$names,list(),accumulate=F)

  comps = unlist(lapply(outlist, function(r) {r$comparison}))
  studies = unlist(lapply(outlist, function(r) {r$study}))
  contrs = unlist(lapply(outlist, function(r) {r$contribution}))

  studyRow=data.frame(study=studies,contribution=contrs,comparison=comps)

  return(list(comparisonRow = contribution,
              studyRow = studyRow,
              row = comparison
  ))
}

# nmakrahn <- utils::getFromNamespace("nma.krahn", "netmeta")
# utils::getFromNamespace("chkclass", "meta")

chkclass <- function(x, class, name = NULL) {
  ##
  ## Check class of R object
  ##
  if (is.null(name))
    name <- deparse(substitute(x))
  ##
  if (!inherits(x, class))
    stop("Argument '", name,
         "' must be an object of class \"",
         class, "\".", call. = FALSE)
  ##
  invisible(NULL)
}

compsplit <- function(x, split) {

  if (split %in% .special.characters)
    split <- paste("\\", split, sep = "")

  res <- strsplit(x, split)

  res
}

nma.krahn <- function(x, tau.preset = 0, sep.trts = x$sep.trts) {


  chkclass(x, "netmeta")


  if (is.na(tau.preset))
    tau.preset <- 0


  if (is.null(sep.trts))
    sep.trts <- ":"


  n <- x$n


  if (x$reference.group == "")
    trts <- colnames(x$A.matrix)
  else
    trts <- c(x$reference.group,
              colnames(x$A.matrix)[colnames(x$A.matrix) != x$reference.group])


  studies.pre <- data.frame(studlab = x$studlab,
                            treat1 = x$treat1, treat2 = x$treat2,
                            TE = -x$TE,
                            seTE = sqrt(x$seTE^2 + tau.preset^2),
                            narms = x$narms[match(x$studlab, x$studies)],
                            stringsAsFactors = FALSE)
  ##
  studies <- studies.pre <- studies.pre[order(studies.pre$studlab), ]


  twoarm   <- any(studies$narms == 2)
  multiarm <- any(studies$narms > 2)
  selmulti <- studies$narms > 2


  sel <- studies.pre$treat2 == x$reference.group
  ##
  studies$treat1[sel] <- studies.pre$treat2[sel]
  studies$treat2[sel] <- studies.pre$treat1[sel]
  studies$TE[sel] <- -studies.pre$TE[sel]
  studies <- data.frame(studies,
                        comparison = paste(studies$treat1, studies$treat2, sep = sep.trts))


  comparison.num.poss <- n * (n - 1) / 2
  comparisons <- levels(factor(as.character(studies$comparison)))
  comparison.num <- length(comparisons)


  trts.poss <- rep(NA, comparison.num.poss)
  k <- 1
  for (i in 1:(n - 1))
    for (j in (i + 1):n) {
      trts.poss[k] <- paste(trts[i], trts[j], sep = sep.trts)
      k <- k + 1
    }


  direct <- matrix(NA, nrow = comparison.num, ncol = 6)
  ##
  colnames(direct) <- c("comparison", "TE", "seTE",
                        "TE.2arm", "seTE.2arm", "n.2arm")
  ##
  direct <- data.frame(direct)
  j <- 0
  ##
  for (i in names(table(studies$comparison))) {
    j <- j + 1
    ##
    TE.i <- studies$TE[studies$comparison == i]
    seTE.i <- studies$seTE[studies$comparison == i]
    m1 <-
      suppressWarnings(metagen(TE.i, seTE.i, sm = x$sm,
                               warn = FALSE, method.tau.ci = ""))
    ##
    direct$comparison[j] <- i
    direct$TE[j] <- m1$TE.fixed
    direct$seTE[j] <- m1$seTE.fixed
    ##
    if (sum(studies$comparison == i & !selmulti) > 0) {
      TE.i   <- studies$TE[studies$comparison == i & studies$narms == 2]
      seTE.i <- studies$seTE[studies$comparison == i & studies$narms == 2]
      m2 <-
        suppressWarnings(metagen(TE.i, seTE.i, sm = x$sm,
                                 warn = FALSE, method.tau.ci = ""))
      ##
      direct$TE.2arm[j] <- m2$TE.fixed
      direct$seTE.2arm[j] <- m2$seTE.fixed
      direct$n.2arm[j] <- m2$k
    }
  }


  if (multiarm) {
    multistudies <- split(studies[selmulti, ], as.character(studies$studlab[selmulti]))
    multistudies <- lapply(multistudies,
                           function(x)
                             x[which(x$treat1 == names(which.max(table(x$treat1)))), ]
    )
    multistudies <- lapply(multistudies,
                           function(x)
                             x[order(x$treat2), ]
    )
    ##
    des <- lapply(multistudies,
                  function(x)
                    paste(c(x$treat1[1], x$treat2), collapse = sep.trts)
    )
    multistudies <- data.frame(unsplit(multistudies,
                                       rep(names(multistudies),
                                           unlist(lapply(multistudies,
                                                         function(x)
                                                           nrow(x)
                                           )
                                           )
                                       )
    ),
    design = unsplit(des,
                     rep(names(multistudies),
                         unlist(lapply(multistudies,
                                       function(x)
                                         nrow(x)
                         )
                         )
                     )
    )
    )
    ##
    multistudies <- data.frame(multistudies,
                               des = paste(multistudies$comparison, multistudies$design, sep = "_"))
    ##
    row.names(studies) <- NULL
    multistudies2 <- split(studies[selmulti, ], as.character(studies$studlab[selmulti]))
    multistudies2 <- lapply(multistudies2,
                            function(x)
                              x[do.call(order, x[, c("treat1","treat2")]), ]
    )
    multistudies2 <- lapply(multistudies2,
                            function(x)
                              rbind(
                                x[x$treat1 == names(which.max(table(x$treat1))), ],
                                x[x$treat1 %in% names(table(x$treat1)[-which.max(table(x$treat1))]), ])
    )
    multistudies2 <- unsplit(multistudies2,
                             rep(names(multistudies2),
                                 unlist(lapply(multistudies2,
                                               function(x)
                                                 nrow(x)
                                 )
                                 )
                             )
    )
  }

  studies <- data.frame(studies, design = studies$comparison)
  if (multiarm & sum(is.na(direct$seTE.2arm)) > 0)
    direct2 <- data.frame(direct[!is.na(direct$seTE.2arm), ])
  else
    direct2 <- direct
  ##
  direct2 <- data.frame(direct2)

  if (length(unique(studies$design)) == 1)
    return(invisible(NULL))

  V.design <- diag(direct2$seTE.2arm^2,
                   nrow = length(direct2$seTE.2arm),
                   ncol = length(direct2$seTE.2arm))


  if (multiarm) {
    sp <- split(multistudies2, multistudies2$studlab)
    armM <- unlist(lapply(split(multistudies2$narms, multistudies2$studlab),
                          function(x)
                            x[1]
    )
    )
    ##
    covs <- lapply(sp,
                   function(x) {
                     n <- x$narms[1]
                     k <- 0
                     m <- matrix(NA, nrow = n - 1, ncol = n - 1)
                     for (i in 1:(n - 2)) {
                       for (j in (i + 1):(n - 1)) {
                         m[i, j] <- (x$seTE[i]^2 + x$seTE[j]^2 - x$seTE[n + k]^2) / 2
                         m[j, i] <- (x$seTE[i]^2 + x$seTE[j]^2 - x$seTE[n + k]^2) / 2
                         k <- k + 1
                       }
                     }
                     diag(m) <- x$seTE[1:(n - 1)]^2
                     m
                   }
    )
    ##
    V3 <- NA
    ##
    for (i in 1:length(covs))
      V3 <- adiag(V3, covs[[i]])
    ##
    V3 <- V3[-1, -1]
    ##
    if (sum(!selmulti) == 0) {
      V.studies <- V3
    }
    else {
      if (sum(!selmulti) < 2)
        V.2arm <- matrix(studies$seTE[!selmulti]^2)
      else
        V.2arm <- diag(studies$seTE[!selmulti]^2)
      ##
      V.studies <- adiag(V.2arm, V3)
    }
    colnames(V.studies) <- c(as.character(studies$design[!selmulti]),
                             as.character(multistudies$design))
    rownames(V.studies) <- c(as.character(studies$design[!selmulti]),
                             as.character(multistudies$design))
    ##
    multicomp <- names(which(table(multistudies$design) > 0))
    V3.agg <- NA
    TE.agg <- NA
    ##
    for (i in 1:length(multicomp)) {
      studlabM <- unique(multistudies$studlab[multistudies$design == multicomp[i]])
      ncovs <- covs[names(covs) %in% studlabM]
      l <- sapply(ncovs, solve)
      dim <- multistudies$narms[multistudies$studlab == studlabM[1]][1] - 1
      covs3 <- solve(matrix(apply(l, 1, sum), nrow = dim))
      V3.agg <- adiag(V3.agg, covs3)
      m <- matrix(NA, nrow = dim, ncol = length(studlabM))
      for (j in 1:length(studlabM))
        m[, j] <- matrix(l[, j], nrow = dim) %*%
        multistudies$TE[multistudies$studlab == studlabM[j]]
      ##
      TE.agg <- c(TE.agg, covs3 %*% apply(m, 1, sum))
    }

    TE.agg <- TE.agg[-1]
    V3.agg <- V3.agg[-1, -1]

    V <- adiag(V.design, V3.agg)
    ##
    nam <- rep(multicomp, unlist(lapply(split(multistudies, multistudies$design),
                                        function(x)
                                          x$narms[1]
    )
    ) - 1
    )
    ##
    if (any(twoarm))
      rownames(V) <- colnames(V) <- c(direct2$comparison, nam)
    else
      rownames(V) <- colnames(V) <- nam
    ##
    TE.dir <- c(direct2$TE.2arm, TE.agg)
  }
  else {
    V <- adiag(V.design)
    rownames(V) <- direct2$comparison
    colnames(V) <- direct2$comparison
    TE.dir <- direct2$TE.2arm
    V.studies <- diag(studies$seTE[!selmulti]^2)
    colnames(V.studies) <- rownames(V.studies) <- as.character(studies$comparison[!selmulti])
  }
  ##
  if (min(eigen(V, only.values = TRUE)$values)<0)
    stop("Covariance matrix is not non-negative definite.")


  fX <- function(n) {
    possK <- n * (n - 1) / 2
    X <- matrix(0, nrow = possK, ncol = n - 1)
    X[1:(n - 1), 1:(n - 1)] <- diag(rep(-1, n - 1))
    X[n * (n - 1) / 2, (n - 2):(n - 1)] <- cbind(1, -1)
    if (n * (n - 1) / 2 - (n - 1) > 1) {
      l <- n
      j <- n - 2
      u <- n + j - 1
      for (k in 1:(n - 3)) {
        X[l:u, k:(n - 1)] <- cbind(1, diag(rep(-1, n - k - 1)))
        j <- j - 1
        l <- u + 1
        u <- l + j - 1
      }
    }
    X
  }
  ##
  X.full <- fX(n)
  rownames(X.full) <- trts.poss
  colnames(X.full) <- trts.poss[1:n - 1]
  ##
  X.obs2.design <- X.full[direct2$comparison, , drop = FALSE]


  if (multiarm) {
    num.basics.design <- unlist(lapply(split(multistudies, multistudies$design),
                                       function(x)
                                         x$narms[1]
    )
    ) - 1
    ##
    basics <- lapply(split(multistudies, multistudies$design),
                     function(x)
                       split(x, x$studlab)[[1]]$comparison
    )
    basics <- unsplit(basics, rep(1:length(multicomp), num.basics.design))
    ##
    X.obs3.design <- X.full[as.character(basics), ]
    rownames(X.obs3.design) <- rep(multicomp, num.basics.design)
    X.obs <- rbind(X.obs2.design, X.obs3.design)
  }
  else
    X.obs <- X.obs2.design
  ##
  H <- X.full %*% solve(t(X.obs) %*% solve(V) %*% X.obs) %*% t(X.obs) %*% solve(V)
  TE.net <- H %*% TE.dir


  covTE.net.base <- solve(t(X.obs) %*% solve(V) %*% X.obs)
  co <- NA
  for (i in 1:(n - 2)) {
    for (j in 2:(n - 1)) {
      if (i != j && i < j) {
        co <- c(co,
                diag(covTE.net.base)[i] +
                  diag(covTE.net.base)[j] -
                  2 * covTE.net.base[i, j])
      }
    }
  }
  ##
  covTE.net <- c(diag(covTE.net.base), co[-1])


  comps <- as.character(studies$comparison[!selmulti])
  studlabs <- as.character(studies$studlab[!selmulti])
  ##
  if (multiarm) {
    comps <- c(comps, as.character(multistudies$comparison))
    studlabs <- c(studlabs, as.character(multistudies$studlab))
  }


  X.obs.studies <- X.full[comps, ]


  H.studies <- X.full %*%
    solve(t(X.obs.studies) %*% solve(V.studies) %*% X.obs.studies) %*%
    t(X.obs.studies) %*% solve(V.studies)
  ##
  colnames(H.studies) <- studlabs


  network <- data.frame(TE = TE.net, seTE = sqrt(covTE.net))


  if (multiarm) {
    len.designs <- c(rep(1, length(direct2$comparison)),
                     unlist(lapply(compsplit(multicomp, sep.trts),
                                   function(x)
                                     length(x) - 1
                     )
                     )
    )
    freq <- rep(c(direct2$n.2arm,
                  unlist(lapply(split(multistudies, multistudies$design),
                                function(x)
                                  length(names(table(x$studlab)))
                  )
                  )
    ),
    len.designs)
    narms <- rep(c(rep(2, nrow(direct2)),
                   unlist(lapply(compsplit(multicomp, sep.trts),
                                 function(x)
                                   length(x)
                   )
                   )
    ),
    len.designs)
    ##
    design <- data.frame(design = c(direct2$comparison,
                                    rep(multicomp,
                                        unlist(lapply(compsplit(multicomp, sep.trts),
                                                      function(x)
                                                        length(x)
                                        )
                                        ) - 1
                                    )
    ),
    comparison = c(direct2$comparison,
                   as.character(unlist(lapply(split(multistudies,
                                                    multistudies$design),
                                              function(x)
                                                as.character(
                                                  unlist(
                                                    split(x, x$studlab)[[1]]["comparison"]
                                                  )
                                                )
                   )
                   )
                   )
    ),
    narms = narms,
    freq = freq,
    TE.dir = TE.dir,
    seTE.dir = sqrt(diag(V)))
  }
  else {
    len.designs <- c(rep(1, length(direct2$comparison)))
    freq <- rep(c(direct2$n.2arm), len.designs)
    narms <- rep(c(rep(2, nrow(direct2))), len.designs)
    ##
    design <- data.frame(design = colnames(V),
                         comparison = colnames(V),
                         narms = rep(2, length(direct2$comparison)),
                         freq = direct2$n.2arm,
                         TE.dir = TE.dir,
                         seTE.dir = sqrt(diag(V)))
  }
  ##
  rownames(design) <- NULL
  ##
  design <- data.frame(design,
                       TE.net = network[as.character(design$comparison), "TE"],
                       seTE.net = network[as.character(design$comparison), "seTE"])


  if (multiarm)
    studies <- rbind(studies[!selmulti, ], multistudies[, 1:8])
  ##
  studies <- studies[, c("studlab", "treat1", "treat2",
                         "TE", "seTE", "narms",
                         "design", "comparison")]
  ##
  studies <- merge(studies, design[, names(design) != "narms"],
                   by = c("design", "comparison"))
  ##
  studies <- studies[, c("studlab", "design", "comparison", "treat1", "treat2",
                         "narms", "freq", "TE", "seTE",
                         "TE.dir", "seTE.dir", "TE.net", "seTE.net")]
  ##
  studies_lim <- studies[which(studies$narms == 2), ]
  studies_mult <- studies[which(studies$narms > 2), ]
  studies <- rbind(studies_lim[order(studies_lim$studlab), ],
                   studies_mult[do.call(order, studies_mult[, c("studlab","treat1","treat2")]), ])

  res <- list(n = n,
              k = x$k,
              d = length(unique(design$design)),
              trts = trts,
              comparisons = comparisons,
              studies = studies,
              direct = direct,
              network = network,
              design = design,
              multicomp = if (multiarm) multicomp else NULL,
              X.obs = X.obs,
              X.full = X.full,
              V = V,
              V.studies = V.studies,
              H = H,
              H.studies = H.studies,
              sep.trts = sep.trts)

  class(res) <- "nma.krahn"

  res
}

#######################################################################################
#' To calculate the contribution matrix.
#'
#' @title Contribution matrix of frequentist net-meta
#'
#' @return matrix
#'
#' @importFrom igraph E graph_from_edgelist set.vertex.attribute V set.edge.attribute delete_edges get.shortest.paths E<- V<-
#' @importFrom reshape2 dcast
#' @importFrom magic adiag
#'
#' @param nmt results from model_netmeta function
#' @param comp.all all comparisoin
#'
#' @export
#' @examples
#'
#' LDT1 <- read.csv(system.file("extdata", "HR_SH_D.csv", package = "net.meta"))
#' trt1 <- read.table(system.file("extdata", "HR_SH_D.txt", package = "net.meta"),
#'     header=TRUE,quote = '"', stringsAsFactors=FALSE)
#'
#' trt1$description <- factor(trt1$description, trt1$description)
#' LDT1$study <- factor(LDT1$study, unique(LDT1$study))
#'
#' nmt1 <- model_netmeta(long.data = LDT1,
#' treatment=LDT1$treatment,
#' id.treatments = trt1,
#' reference = "A",
#' outcome = "HR")
#'
#' name1 <- NULL
#' for(i in 1:(length(trt1$id)-1)){
#'   name1 <- c(name1, paste0(trt1$id[i], ":", trt1$id[-(1:i)]))
#' }
#'
#' study.CM1 <- studyCM_matrix(name1, nmt1)
#'


studyCM_matrix <- function(
  comp.all,   ## pairwise treatment
  nmt){

  ## H matrix
  Hkrahn=nma.krahn(nmt,tau.preset = nmt$tau)$H
  X.full=nma.krahn(nmt,tau.preset = nmt$tau)$X.full
  direct=nma.krahn(nmt,tau.preset = nmt$tau)$direct
  X=nma.krahn(nmt,tau.preset = nmt$tau)$X.full[direct$comparison,,drop=FALSE]
  Vd=diag(direct$seTE^2,nrow=length(direct$seTE),ncol=length(direct$seTE))

  H <- X.full %*% solve(t(X) %*% solve(Vd) %*% X) %*% t(X) %*% solve(Vd)
  colnames(H)<-rownames(X)

## study contribution
study.CM <- NULL

for (i in 1:length(comp.all)){
study.cm <- getStudyContribution(H,comp.all[i],nmt)$studyRow[,1:2]
colnames(study.cm) <- c("study", comp.all[i])
study.cm$id <- seq_len(dim(study.cm)[1])
w.study.cm<-dcast(study.cm, id~study, value.var = comp.all[i])
study.CM <- rbind(study.CM, colSums(w.study.cm, na.rm = TRUE))
}
dimnames(study.CM)[[1]] <- comp.all

CM <- as.data.frame(study.CM[,-1])[,nmt[["studies"]]]


return(CM)

}
