multi<-function(...){
  res<-list()
  mf<-match.call(expand.dots=TRUE)
  for(i in 2:length(mf)){
    leveli<-eval(mf[[i]])
    levelnamei<-names(mf)[[i]]
    dta<-leveli[[2]]
    if(class(dta)!="MI")
      if(is.data.frame(dta[[1]])){
        for(j in 1:length(dta)){
          newlevelname<-paste(levelnamei,j,sep="")
          res[[newlevelname]]<-list(formula=NULL, data=NULL)
          res[[newlevelname]][[1]]<-leveli[[1]]
          res[[newlevelname]][[2]]<-leveli[[2]][[j]]
        }
      }
      else{
        res[[levelnamei]]<-leveli
        names(res[[levelnamei]])<-c("formula","data")
      }
}
  class(res)<-c("multi", class(res))
return(res)
}


