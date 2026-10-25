getHatmatrixFromDB = function(refid,model="random",sm){
  require(nmadata)
  indata = readByID(refid,format="long")
  type = indata$type
  if(missing(sm)){
    if(type == "binary"){ 
      sm = "OR"           
    }else{                
      sm = "SMD"          
    }                     
  }
  C = getHatMatrix(indata$data,type=longType(indata),model,sm,tau="NA")
  return(list(hm=C, dataset=indata)) 
}   
