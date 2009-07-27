getModelCitation <- function (modelName){
  citeobj <- .getModelCitation(modelName)
  authors <- citeobj$mauthors
  year <- citeobj$myear
  title <- citeobj$mtitle
  const <- citeobj$zconst
  url <- citeobj$zurl
  res <- paste("How to cite this model in Zelig:\n", authorsToText(authors),". ", year ,". ", descToText(title,modelName), " ", const, " ", url, "\n" , sep="")
  res
  
}

authorsToText <- function(auths){
  howmany <- length(auths)
  
  ## if empty it's Kosuke, Gary, Olivia
  if (howmany == 0) {
    return ("Kosuke Imai, Gary King, and Oliva Lau")
  }

  ## if aurhor return it
  if (howmany == 1){
    return(auths[[1]])
  } 

  ## if 2 just an "and" in between
  if (howmany == 2) {
    return (paste(auths[[1]]," and ", auths[[2]], sep = ""))
  }

  ## separate by comma and an "and" before the last author
  res <- paste(auths[1:(howmany - 1)], collapse = ", ", sep="")
  res <- paste(res, ", and ", auths[[howmany]], sep = "")
  return(res)  
}

.getModelCitation <- function(modelName){
  
  zconst <- "in Kosuke Imai, Gary King, and Olivia Lau, \"Zelig: Everyone's Statistical Software,\""
  zurl <- "http://gking.harvard.edu/zelig"
  descObject <- Zelig:::zeligDescribeModel(modelName)
  if (!is.null(descObject)){
    mauthors <- descObject$authors
    mtitle <- descObject$description
    myear <- descObject$year
    
  }
  list(mauthors=mauthors, mtitle=mtitle, myear=myear, zconst=zconst, zurl=zurl)
  
}

descToText <- function(desc, modelName){
  paste('"',modelName,": ",desc,'"',sep="")
}



.getModelCitationTex <- function (modelName){
  citeobj <- .getModelCitation(modelName)
  authors <- citeobj$mauthors
  year <- citeobj$myear
  title <- citeobj$mtitle
  const <- citeobj$zconst
  url <- citeobj$zurl
  res <- paste(authorsToText(authors),". ", year ,". ", descToText(title,modelName), " ", const, "\\url{", url, '}' , sep="")
  res
  
}
