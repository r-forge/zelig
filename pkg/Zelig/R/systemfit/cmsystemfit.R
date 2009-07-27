cmsystemfit<-function(formu,omit=NULL,...){
  tr<-terms.multiple(formu,omit)  
  ev<-attr(tr,"term.labels") 
  dv<-all.vars(attr(tr,"variables")[[2]],unique=FALSE)
  om<-attr(tr,"omit")
  syst<-res<-list()
  for(i in 1:length(dv)){
    syst[[i]]<- ev[om[i,]==0]
    res[[i]]<-paste(dv[i],"~")
    for(j in 1:(length(syst[[i]])-1)){
      res[[i]]<-paste(res[[i]],syst[[i]][j])
      res[[i]]<-paste(res[[i]],"+")    
    }
    res[[i]]<-paste(res[[i]],syst[[i]][length(syst[[i]])])
    res[[i]]<-as.formula(res[[i]])
            
  }
  return (res)
}
