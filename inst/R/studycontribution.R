#Gives contributions of individual studies for a single comparison
# arguments hatmatrix list from getHatMatrix and comparison the string
# identifying the hatmatrix row separated by ":"
getStudyContribution = function (hatmatrix, comparison) {
  contribution = getComparisonContribution(hatmatrix, comparison)
  model = hatmatrix$model
  # main data frame
  dfr = hatmatrix$forstudycontribution
  tau = 0
  if (model=="random"){
    tau = hatmatrix$tau
  }
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

  return(list( comparisonRow = contribution
             , studyRow = studyRow
             , row = comparison
             ))
}
