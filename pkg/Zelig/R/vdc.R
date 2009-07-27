zeligListModels<-function(inZeligOnly=TRUE) {
     if (inZeligOnly) {
    		tmp = ls(envir=asNamespace("Zelig"),pattern="^zelig2")
     } else { 
    		tmp = c( ls(envir=asNamespace("Zelig"),pattern="^zelig2"),
         		apropos("zelig2",mode="function"))
     }
     sub("zelig2","", tmp)
}





zeligInstalledModels<-function(inZeligOnly=TRUE,schemaVersion="1.1") {
  chkpkgs<-function(name)  {
    zd=zeligDescribeModelXML(name,schemaVersion=schemaVersion)
    if (is.null(zd)) {
      return (FALSE)
    }
    zdpd= zeligModelDependency(name)[,1]
    if (is.null(zdpd)) {
      return(TRUE)
    }
    ow=options(warn=-1)
    ret=sapply(zdpd,function(x) require(x,character.only=TRUE)==TRUE)
    options(ow)
    return (ret)
  }
  models<-zeligListModels(inZeligOnly=inZeligOnly)
     # Not being the trusting sort, lets check to see if we can run
     # a dummy formula. If not -- almost always means that something
     # required() is missing
     tmpModels<-sapply(models,chkpkgs)
  models[which(tmpModels)]
}

zeligGetSpecial<-function(modelName) {
	 modelDesc = zeligDescribeModel(modelName)
	 return(modelDesc$parameters[[1]]$specialFunction)
}

zeligModelDependency<-function(modelName,repos="") {
        zd= zeligDescribeModel(modelName)

        if (is.null(zd)) { return (NULL) }

        zdpd=zd[which(names(zd)=="package")]
        
        cbind(sapply(zdpd,function(x)x$name),
                sapply(zdpd,function(x){if (is.null(x$CRAN))
                {rv<-repos} else{rv<-x$CRAN};rv;}))
      }


zeligDescribeModel<-function(name,force=FALSE,schemaVersion="1.1") {
    res=try(eval(call(paste("describe.",name,sep=""))),silent=TRUE)
    if (inherits(res,"try-error")) {
        if (force) {
                res=describe.default()
        } else {
                res=NULL
        }
    }
#    res$name<-name           # only here we have access to the model name, so add it to the list.
    if(!is.null(res)){
   res<-check.full(res,name)
  }
    return(res)
}

zeligDescribeModelXML<-function(modelName,force=FALSE,schemaVersion="1.1") {
	zd = zeligDescribeModel(modelName,force,schemaVersion)
	if (is.null(zd)) {
		return(NULL)
	} else {
		return(zmodel2string(zd))
	}

}


zmodel2string<-function(x) {
     xmlList(x)
}
printZeligSchemaInstance<-function(filename=NULL, serverName=NULL,vdcAbsDirPrefix=NULL){
	# open connection 
	schemaURL<-'http://gking.harvard.edu/zelig';
	if (is.null(serverName)) {
		serverName<-system('hostname -f', intern=TRUE)
	}
	if (is.null(vdcAbsDirPrefix)){
		locationURL<-paste('http://', serverName, '/VDC/Schema/analysis/ZeligInterfaceDefinition.xsd',sep="");
	} else {
		locationURL<-paste('file://', vdcAbsDirPrefix, '/VDC/Schema/analysis/ZeligInterfaceDefinition.xsd',sep="");
	}
	schemaLocation<-paste(schemaURL, ' ', locationURL, sep='');
	con<-"";
	if (!is.null(filename)){
		con<-file(filename,"w");
	}
	cat(file=con, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<zelig xmlns=\"",schemaURL,"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"",schemaLocation,"\">\n", sep="");
	mssg<- sapply(zeligInstalledModels(),function(x){cat(file=con,zmodel2string(zeligDescribeModel(x)),sep="")},simplify=FALSE);
	cat(file=con,"\n</zelig>\n",sep="");
}


xmlList<-function(z){
  if(is.null(z))
    return ("")
  mins<-c()
  maxs<-c()
  for(i in 1:length(z$parameters)){
    mins<-c(mins,z$parameters[[i]]$equations[[1]])
    maxs<-c(maxs,z$parameters[[i]]$equations[[2]])
  }
  min<-sum(mins)
  if(any(!is.finite(maxs)))
    max<-Inf
  else
    max<-sum(maxs)
  if(max !=1)
    return("")
  res<-paste("<model name=",'"',z$name,'"'," label=",'"',categories()[[z$category]],'"', sep="")
  if(!(is.na(z$parameters[[1]]$specialFunction)))
    res<-paste(res," specialFunction=",'"',z$parameters[[1]]$specialFunction,'"',sep="")
  res<-paste(res,">\n",sep="")
  res<-paste(res,"<description>",z$description, "</description>\n",sep="")
  url <- paste("http://gking.harvard.edu/zelig/doc/",z$name,".pdf",sep="")
  #if(z$name=="irtkd")
  #  res<-paste(res,"<helpLink url=",'"',"http://gking.harvard.edu/zelig/docs/_TT_irtkd_TT__tex2htm.html",'"',sep="")
  #else
  #res<-paste(res,"<helpLink url=",'"',modelURL(z$name,z$description),'"',sep="")
res<-paste(res,"<helpLink url=",'"',url,'"',sep="")

  res<-paste(res,"/>\n", sep="")
  if(any(!(is.null(z$package)))){
    res<-paste(res,"<packageDependency",sep="")
    if(!(is.na(z$package$name)))
      res<-paste(res," name= ",'"',z$package$name,'"',sep="")
    if(!(is.na(z$package$version)))
      res<-paste(res," version= ",'"',z$package$version,'"',sep="")
    if(!is.null(z$package$CRAN) && !(is.na(z$package$CRAN)))
      res<-paste(res," CRAN= ",'"',z$package$CRAN,'"',sep="")  
    res<-paste(res,"/>\n",sep="")
  }
  res<-paste(res,"<formula minEquations=",'"',min,'"',sep="")
  if(is.finite(max))
    res<-paste(res," maxEquations=",'"',max,'"',sep="")
  if(max==1)
    res<-paste(res," simulEq=",'"',0,'"',sep="")
  res<-paste(res,">\n",sep="")
  
  res<-paste(res,"<equation name=",'"',names(z$parameters)[[1]],'"',">\n",sep="")
  if(!(z$name %in% c("exp","lognorm","weibull"))){    ##we are going to delete this !!!
    if(z$parameters[[1]]$depVar){
      res<-paste(res,"<outcome",sep="")
      if(!is.na(z$parameters[[1]]$specialFunction))                 
        {
          if(is.finite(z$parameters[[1]]$varInSpecialFunction[[2]] ))
            res<-paste(res," maxVar=",'"',z$parameters[[1]]$varInSpecialFunction[[2]],'"',sep="")
          res<-paste(res," minVar=",'"',z$parameters[[1]]$varInSpecialFunction[[1]],'"',sep="")
        }
      else
        {
          if(z$parameters[[1]]$depVar !=TRUE)
            res<-paste(res," minVar=",'"',0,'"'," maxVar=",'"',0,'"',sep="")
        }
      res<-paste(res,">\n")
      for(i in 1:length(modeling.types()$depVar[[z$category]]))
        res<-paste(res,"<modelingType>",modeling.types()$depVar[[z$category]][[i]],"</modelingType>\n",sep="")
      res<-paste(res,"</outcome>\n",sep="")
    }
  } else
  res<-paste(res,durOutput())
                                        #explanatory
  if(z$parameters[[1]]$expVar){
    res<-paste(res,"<explanatory ")
    if(z$parameters[[1]]$expVar == TRUE)
      res<-paste(res," minVar=",'"',1,'"',sep="")
    else
      res<-paste(res," minVar=",'"',0,'"'," maxVar=",'"',0,'"',sep="")
    res<-paste(res,">\n")
    
 
    for(i in 1:length(modeling.types()$expVar[[z$category]]))
      res<-paste(res,"<modelingType>",  modeling.types()$expVar[[z$category]][[i]],"</modelingType>\n",sep="")
    res<-paste(res,"</explanatory>\n",sep="")
  }
  res<-paste(res,"</equation>\n",sep="")
   res<-paste(res,"</formula>\n",sep="")
   if(z$parameters[[1]]$expVar)
     sext<-2
  else
    sext<-0
  res<-paste(res,"<setx maxSetx=",'"',sext,'"',"/>\n",sep="")
  res<-paste(res,"</model>\n")
  return(res)
}



check.full<-function(z,name){
 # we suppose that describe.model pass the check
  z$name<-name
  if(is.null(z$package))
  z$package<-list(name=NA,version=NA, CRAN=NA)
  
  for (i in length(z$parameters)){
  if(is.null(z$parameters[[i]]$specialFunction)) z$parameters[[i]]$specialFunction<-NA
  if(is.null(z$parameters[[i]]$varInSpecialFunction)) z$parameters[[i]]$varInSpecialFunction<-NA
}
 return(z)
  
}


modelURL<-function(modelName,modelDesc){
  baseUrl<-"http://gking.harvard.edu/zelig/docs/"
  spec<-"_TT_"
  res<-paste(baseUrl,spec,modelName,spec,"_",substr(modelDesc,0, 13-nchar(modelName)) ,".html",sep="")
  res<-gsub(".","_",res,fixed=TRUE)
  res<-gsub(" ","_",res,fixed=TRUE)
  res
}

modeling.types <-function(){
  res<-list(
            expVar=list(continuous=c("continuous","discrete","nominal","ordinal","binary"),
              dichotomous=c("continuous","discrete","nominal","ordinal","binary"),
              ordinal=c("continuous","discrete","nominal","ordinal","binary"),
              bounded=c("continuous","discrete","nominal","ordinal","binary"),
              multinomial=c("continuous","discrete","nominal","ordinal","binary"),
              count=c("continuous","discrete","nominal","ordinal","binary"),
              mixed=c("continuous","discrete","nominal","ordinal","binary"),
              ei=c("continuous","discrete","nominal","ordinal","binary")
              ),
            depVar=list(
              continuous="continuous",
              dichotomous="binary",
              ordinal="ordinal",
              bounded="continuous",
              multinomial=c("nominal","ordinal"),
              count="discrete",
              mixed=c("continuous","discrete","nominal","ordinal","binary"),
              ei=c("continuous")
              )
            )
  res
}


## this is a temporary function; Is going to be removed after we change "describe" for this duration models;

durOutput <-function(){
res<-"<outcome minVar=\"1\" maxVar=\"1\" label=\"Duration\">\n<modelingType>continuous</modelingType>\n</outcome>\n<outcome maxVar=\"1\" minVar=\"0\" label=\"Censored\">\n<modelingType>binary</modelingType>\n</outcome>\n"
return (res)
  
}
