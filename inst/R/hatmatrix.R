getHatMatrix <- function(indata,type,model="fixed",tau=NA, sm){
  
  require(netmeta)
  library(meta)
  library(plyr)
  
  D <- indata
  
  #network meta-analysis
  
  if (type=="long_binary"){
    Dpairs=pairwise(treat=t,event=r,n=n, data=D, studlab = id, sm= sm, allstudies = T)
    metaNetw<-netmeta(TE,seTE,treat1,treat2,studlab,data=Dpairs,sm=sm,comb.fixed =F,comb.random = T)
  } 
  
  if (type=="long_continuous"){
    Dpairs=pairwise(treat=t,mean=y,sd=sd,n=n,data=D, studlab =id, sm=sm)
    metaNetw<-netmeta(TE,seTE,treat1,treat2,studlab,data=Dpairs,sm=sm,comb.fixed =F,comb.random = T, tol.multiarm=0.05)
  }
  
  if (type=="iv"){
    metaNetw=netmeta(effect,se,t1,t2,id,data=D,sm=sm,comb.fixed =F,comb.random = T, tol.multiarm=0.05)
  }
  
  #inconsistency
  #design by treatment
  dbt=cbind(decomp.design(metaNetw)$Q.inc.random[,1],decomp.design(metaNetw)$Q.inc.random[,2],decomp.design(metaNetw)$Q.inc.random[,3])
  colnames(dbt)=c("Q_dbt","df","pv_dbt")
  
  #side-splitting
  sideSplit=netsplit(metaNetw)
  
  if (model=="fixed"){
    Direct=sideSplit$direct.fixed$TE
    DirectL=sideSplit$direct.fixed$lower
    DirectU=sideSplit$direct.fixed$upper
    Indirect=sideSplit$indirect.fixed$TE
    IndirectL=sideSplit$indirect.fixed$lower
    IndirectU=sideSplit$indirect.fixed$upper
    SideIF=sideSplit$compare.fixed$TE
    SideIFlower=sideSplit$compare.fixed$lower
    SideIFupper=sideSplit$compare.fixed$upper
    SideZ=sideSplit$compare.fixed$z
    SidePvalue=sideSplit$compare.fixed$p
    PropDir=sideSplit$prop.fixed
  }
  
  if (model=="random"){
    Direct=sideSplit$direct.random$TE
    DirectL=sideSplit$direct.random$lower
    DirectU=sideSplit$direct.random$upper
    Indirect=sideSplit$indirect.random$TE
    IndirectL=sideSplit$indirect.random$lower
    IndirectU=sideSplit$indirect.random$upper
    SideIF=sideSplit$compare.random$TE
    SideIFlower=sideSplit$compare.random$lower
    SideIFupper=sideSplit$compare.random$upper
    SideZ=sideSplit$compare.random$z
    SidePvalue=sideSplit$compare.random$p
    PropDir=sideSplit$prop.random
  }
  
  side=cbind(c(Direct),c(DirectL),c(DirectU),c(Indirect),c(IndirectL),c(IndirectU),
             c(SideIF),c(SideIFlower),c(SideIFupper),c(SideZ),c(SidePvalue),c(PropDir))
  
  rownames(side) <- c(sideSplit$comparison)
  colnames(side) <- c("Direct","DirectL","DirectU",
                      "Indirect","IndirectL","IndirectU",
                      "SideIF","SideIFlower","SideIFupper","SideZ","SidePvalue","PropDir")
  
  #H matrix
  
  if(model=="fixed"){
    Hkrahn=netmeta:::nma.krahn(metaNetw,tau.preset = 0)$H
    X.full=netmeta:::nma.krahn(metaNetw,tau.preset = 0)$X.full
    direct=netmeta:::nma.krahn(metaNetw,tau.preset = 0)$direct
    X=netmeta:::nma.krahn(metaNetw)$X.full[direct$comparison,,drop=FALSE]
    Vd=diag(direct$seTE^2,nrow=length(direct$seTE),ncol=length(direct$seTE))
    
    H <- X.full %*% solve(t(X) %*% solve(Vd) %*% X) %*% t(X) %*% solve(Vd)
    colnames(H)<-rownames(X)
    
  }
  
  if(model=="random"){
    Hkrahn=netmeta:::nma.krahn(metaNetw,tau.preset = metaNetw$tau)$H
    X.full=netmeta:::nma.krahn(metaNetw,tau.preset = metaNetw$tau)$X.full
    direct=netmeta:::nma.krahn(metaNetw,tau.preset = metaNetw$tau)$direct
    X=netmeta:::nma.krahn(metaNetw,tau.preset = metaNetw$tau)$X.full[direct$comparison,,drop=FALSE]
    Vd=diag(direct$seTE^2,nrow=length(direct$seTE),ncol=length(direct$seTE))
    
    H <- X.full %*% solve(t(X) %*% solve(Vd) %*% X) %*% t(X) %*% solve(Vd)
    colnames(H)<-rownames(X)
    
  }
  
  #NMA heteorogeneity results
  
  heterVarNtw=metaNetw$tau^2
  Qmeasures=matrix(c(metaNetw$Q,metaNetw$Q.heterogeneity,metaNetw$Q.inconsistency),nrow=1,ncol=3)
  colnames(Qmeasures) <- c( "Q overall"
                          , "Q heterogeneity"
                          , "Q inconsistency")
  
  NMAheterResults=cbind(heterVarNtw,Qmeasures)
  
  #NMA treatment effects, confidence and prediction intervals
  
  if (model=="fixed"){
    TE.nma=-metaNetw$TE.fixed[lower.tri(metaNetw$TE.fixed)]
    seTE.nma=metaNetw$seTE.fixed[lower.tri(metaNetw$seTE.fixed)]
    UCI.nma=-metaNetw$lower.fixed[lower.tri(metaNetw$lower.fixed)]
    LCI.nma=-metaNetw$upper.fixed[lower.tri(metaNetw$upper.fixed)]
    PrU.nma=-metaNetw$lower.predict[lower.tri(metaNetw$lower.predict)]
    PrL.nma=-metaNetw$upper.predict[lower.tri(metaNetw$upper.predict)]
    PropD=c(metaNetw$prop.direct.fixed)
  } 

  if (model=="random"){
    TE.nma=-metaNetw$TE.random[lower.tri(metaNetw$TE.random)]
    seTE.nma=metaNetw$seTE.random[lower.tri(metaNetw$seTE.random)]
    UCI.nma=-metaNetw$lower.random[lower.tri(metaNetw$lower.random)]
    LCI.nma=-metaNetw$upper.random[lower.tri(metaNetw$upper.random)]
    PrU.nma=-metaNetw$lower.predict[lower.tri(metaNetw$lower.predict)]
    PrL.nma=-metaNetw$upper.predict[lower.tri(metaNetw$upper.predict)]
    PropD=c(metaNetw$prop.direct.random)
  } 
  
  NMA=cbind(c(TE.nma),c(seTE.nma),c(LCI.nma),c(UCI.nma),c(PrL.nma),c(PrU.nma),c(PropD))
  colnames(NMA)<-c("NMA treatment effect", "se treat effect", "lower CI", "upper CI", "lower PrI", "upper PrI","PropDirNetmeta")

  print(rownames(metaNetw$TE.fixed))

  forleaguetable = 
              list( TE.fixed = metaNetw$TE.fixed
              , lower.fixed  = metaNetw$lower.fixed
              , upper.fixed  = metaNetw$upper.fixed
              , TE.random    = metaNetw$TE.random
              , lower.random = metaNetw$lower.random
              , upper.random = metaNetw$upper.random
              , treatnames = rownames(metaNetw$TE.fixed)
              )

  forstudycontribution =
    data.frame( studlab=metaNetw$studlab
              , treat1=metaNetw$treat1
              , treat2=metaNetw$treat2
              , seTE=metaNetw$seTE
              , seTE.adj=metaNetw$seTE.adj
              )
  
  
  NMAresults=cbind(side,NMA)

  #pairwise meta-analysis
  
  if (type=="long_binary"){
    comp<-paste(Dpairs$treat1,rep(":",length(Dpairs$studlab)),Dpairs$treat2)
    metaPairw<-metagen(Dpairs$TE, Dpairs$seTE, sm=sm,
                       comb.random=(model=="random"),byvar=comp)
  }
  
  if (type=="long_continuous"){
    comp<-paste(Dpairs$treat1,rep(":",length(Dpairs$studlab)),Dpairs$treat2)
    metaPairw<-metagen(Dpairs$TE, Dpairs$seTE, sm=sm,
                       comb.random=(model=="random"),byvar=comp)
  }
  
  if (type=="iv"){
    comp<-paste(D$t1,rep(":",length(D$id)),D$t2)
    
    metaPairw<-metagen(D$effect, D$se, sm=sm,
                       comb.random=(model=="random"),byvar=comp)
  }
  
  #Pairwise meta-analysis heteorogeneity results
  heterVarPairwise <- cbind(c(metaPairw$tau.w^2), c(metaPairw$I2.w), 
                            c(metaPairw$lower.I2.w), c(metaPairw$upper.I2.w))
  
  Pairwise=cbind(heterVarPairwise)
  
  rownames(Pairwise) <- metaPairw$bylevs
  colnames(Pairwise) <- c( "tau2"
                         , "I2"
                         , "I2 lower"
                         , "I2 upper")

  #results
  dbt=as.data.frame(dbt)
  NMAheterResults=as.data.frame(NMAheterResults)
  NMAresults=as.data.frame(NMAresults)
  Pairwise=as.data.frame(Pairwise)
  
  return(list( colNames=colnames(H)
             , rowNames=rownames(H)
             , rowNamesPairwise=rownames(Pairwise)
             , colNamesPairwise=colnames(Pairwise)
             , Pairwise=Pairwise
             , NMAresults=NMAresults
             , rowNamesNMAresults=rownames(NMAresults)
             , colNamesNMAresults=colnames(NMAresults)
             , NMAheterResults=NMAheterResults
             , H=H
             , dbt=dbt
             , colNamesdbt = colnames(dbt)
             , model = model
             , sm=sm
             , forleaguetable=forleaguetable
             , forstudycontribution=forstudycontribution
             , tau = metaNetw$tau
             )
  )
}
