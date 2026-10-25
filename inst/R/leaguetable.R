leaguetable<-function(fromhatmatrix,model,sm){
 
  x=fromhatmatrix
  
  meta:::cilayout(bracket = "(", separator = ", ")
  oldopts <- options(width = 100)
  result = NULL
  treatnames = x$treatnames
  
  if (model=="fixed"){
    
    if(sm=="OR"| sm=="RR" | sm=="HR"){
      TE.fixed.x    <- exp(x$TE.fixed)
      lower.fixed.x <- exp(x$lower.fixed)
      upper.fixed.x <- exp(x$upper.fixed)
    }
 
    if (sm=="MD" | sm=="SMD" | sm=="RD"){
      TE.fixed.x    <- x$TE.fixed
      lower.fixed.x <- x$lower.fixed
      upper.fixed.x <- x$upper.fixed
    }

    TE.fixed.x    <- format(round(TE.fixed.x, 3))
    lower.fixed.x <- round(lower.fixed.x, 3)
    upper.fixed.x <- round(upper.fixed.x, 3)
    ##
    nl.f <- paste(TE.fixed.x, meta:::formatCI(lower.fixed.x, upper.fixed.x))
    ##
    nl.f <- matrix(nl.f, nrow = nrow(TE.fixed.x), ncol = ncol(TE.fixed.x))
    diag(nl.f) <- treatnames
    
    result = nl.f
    #write.table(as.data.frame(nl.f),file="LeagueTableFixed.csv",sep=";",row.names=FALSE,col.names=FALSE,qmethod="double")
  }
 
  if (model=="random"){
    ##
    if(sm=="OR"| sm=="RR" | sm=="HR"){
      TE.random.x    <- exp(x$TE.random)
      lower.random.x <- exp(x$lower.random)
      upper.random.x <- exp(x$upper.random)
    }
    
    if (sm=="MD" | sm=="SMD" | sm=="RD"){
    TE.random.x    <- x$TE.random
    lower.random.x <- x$lower.random
    upper.random.x <- x$upper.random
    }
    
    TE.random.x    <- format(round(TE.random.x, 3))
    lower.random.x <- round(lower.random.x, 3)
    upper.random.x <- round(upper.random.x, 3)
    ##
    nl.r <- paste(TE.random.x, meta:::formatCI(lower.random.x, upper.random.x))
    ##
    nl.r <- matrix(nl.r, nrow = nrow(TE.random.x), ncol = ncol(TE.random.x))
    diag(nl.r) <- treatnames
    
    #write.table(as.data.frame(nl.r),file="LeagueTableRandom.csv",sep=";",row.names=FALSE,col.names=FALSE,qmethod="double")
    result = nl.r
  }
  return(result)
}
