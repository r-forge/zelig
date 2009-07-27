### 
### DESCRIPTION: Returns matrix of details corresponding to packages
###              Zelig depends on directly or indirectly
###              Every row corresponds to  a dependent package with the name,
###              version installed in server,the node (parent pkg),and URL 
###              Dependent packages are not repeated (no duplicates) and the highest
###              version is reported 
###              
### USE    matinst <- zelig.all.packages()
###        matinst <- zelig.all.packages(zmat)
###        zmat, matrix of zelig dependencies--OR-- directory to get the matrix 
###
### OUTPUT: zmat is reduced to unique package rows (no repetitions)
###         that Zelig depends on and that are installed in server.
###         Rows for each package that Zelig depends, which are unique;
###         
### AUTHOR Elena Villalon
###        evillalon@iq.harvard.edu
###
###        Last modified: 01/18/2007
###
### 
zelig.all.packages <- function(zmat=matrixDependencies(uniqPkg=FALSE))
{

  if(length(zmat) <= 0 )
    stop("Provide matrix of dependencies")
  
  zmat <- getRidVersion(zmat)
  dm <- dim(zmat)
  if(length(dm) <= 0)
    return(zmat); 
  pkgorg <- zmat[dm[1], ] ###last row
  lv0 <- trim.blanks(zmat[dm[1],"depth"])
  if(lv0 != "0"){
    ix0 <- grep("^0$", zmat[,"depth"])
    if(length(ix0)) pkgorg <- zmat[ix0, ]
  }
  cl <- ncol(zmat)
  nmorg <- trim.blanks(pkgorg["Package"])
  names(nmorg) <- NULL
  ncl <- ncol(zmat)
  pkginst <- zmat[, "Package"]

  pkginst <- unique.default(pkginst) ###preserve order
  ln <- length(pkginst)
  ix <- match(nmorg,pkginst) ###put the root package in last entry or element
  if(is.na(ix)) 
    pkginst <- c(pkginst,nmorg)
  
  if(!is.na(ix) && !identical(nmorg, pkginst[ln])) {
    pkginst <- pkginst[-ix]
    nm <- names(pkginst)
    pkginst <- c(pkginst,nmorg)
    names(pkginst) <- c(nm, nmorg)
    
  }

  zelstm  <- split.data.frame(zmat, zmat[,"Package"]) ###may not preserve order
### to preserve the original order of package rows in zmat
  ind <- sapply(pkginst, match,names(zelstm) )
  zelstm <- zelstm[ind] 
### I do not know if several versions of the same package
### are stored in the data matrix;
### keep lowest depth of dependency closer to root pkg
  
  zelst <- lapply(zelstm, function(mat){
    vers <- na.omit(mat[,"Version"])
    ix1 <- grep("1", mat[,"depth"])
    
    if(length(ix1) > 1){ ###returns fst level dependency
      ixz <- c(grep("1z", mat[,"depth"]),grep("1Z", mat[,"depth"]))
      if(length(ixz))
        return(mat[ixz, ]) ###first from describe, "1z"
      else
        return(mat[ix1, ]) ###snd level "1"
    }

    return(mat[nrow(mat),])})  ##returns lowest level

  ind <- sapply(pkginst, match,names(zelst) ) ###preserver order of rows packages
  zelst <- zelst[ind]
  
  nmzelig <- names(zelst)
  
  nmzelig <- sapply(nmzelig, FUN="trim.blanks")
  zmatuq <- matrix(unlist(zelst), ncol=cl, byrow=T)
  
  colnames(zmatuq) <- colnames(zmat)
  
  rwnm <- zmatuq[, "Package"]
  rownames(zmatuq) <- zmatuq[, "Package"]
  zelig <- trim.blanks(pkgorg["Package"])
  names(zelig) <- NULL
  
  return(zmatuq)
}

### 
### DESCRIPTION: Returns/Creates matrix of details corresponding to packages
###              Zelig depends on directly or indirectly
###              and are installed in the local machine.
###              Every row corresponds to  a dependent package with the name,
###              and version,installed in local machine, which migth not 
###              be the same as the server where Zelig is running. 
###
### USES:     installed.packages(priority=prior,noCache=!cache)
###           prior="NA"(large number), prior="high" (small number); 
###           prior=NULL which picks up all in "NA" and "high".
###           matinst <- zelig.installed.packages(mat <- zelig.all.packages(zmat))
###              
### INPUT  
###        zmat = matrixDependencies(), is matrix of dependencies,
###        as obtained from create.zelig.all.packages("Zelig")
###        and then applying  zelig.all.packages
###        that eliminates duplicates packages (fst column)
###        disp boolean to show the putput in a GUI
###        prior is the priority of installed.packages
###        cache also input for parameter noCahe of installed.packages
###        libpath library directory to search for; example, libpath= "~/.R/library-x86_64"
###
### OUTPUT: Matrix similar to available.packages, but only those packages
###         that Zelig depends on and that are installed in the local machine.
###         Rows for each package that Zelig depends on;
###         columns are the name, and the locally installed version
###         Also returns the R.version installed locally,as the last row of the matrix 
###             
###         
### AUTHOR Elena Villalon
###        evillalon@iq.harvard.edu
###
###        Last modified: 01/18/2007
###
zelig.installed.packages <- function(zmat=matrixDependencies(), disp=F, prior=NULL,cache=F, libpath=NULL)
{

  if(length(zmat) <= 0){
    stop("Provide dependency matrix as from...zelig.all.packages")
    
  }
  localinst <- installed.packages(lib.loc=libpath,priority=prior,noCache=!cache)
  localinst <- getRidVersion(localinst)
###  colnames(localinst)
###  "Package"  "LibPath"  "Version"  "Priority" "Bundle"   "Contains"
###  "Depends"  "Imports"  "Suggests" "Built"
###  rownames(localinst)    names of the packages installed
  no1 <- sessionInfo()$R.version$major
  no2 <- sessionInfo()$R.version$minor
  vv <- paste(no1,".",no2, sep="")
  Rvers <- paste("R.version ", no1,".",no2, sep="")
  
  zrw   <- rownames(zmat) ### names of packages derived from Zelig
  zrw   <- unlist(sapply(zrw, FUN="trim.blanks"))
  zrwuq <- unique.default(zrw)
  
###eliminate duplicate rows if any
  if(length(zrwuq) < length(zrw)){
    zmat <-  zelig.all.packages(zmat)
    zrw   <- rownames(zmat)
    zrw   <- unlist(sapply(zrw, FUN="trim.blanks"))
    zrwuq <- unique.default(zrw)
  }
  
  lrw   <- rownames(localinst) ### names packages installed in local machine
  lrw   <- unlist(sapply(lrw, FUN="trim.blanks"))
  lrwuq <- unique.default(lrw)
  
  ind <- sapply(zrwuq, match, lrwuq)
  
  pkginst <- NULL
  
  if(length(ind) > 0){
    ind <- na.omit(ind)
    pkginst <- lrwuq[ind]
    names(pkginst) <- lrwuq[ind]
    
  }
  if(length(pkginst) <= 0){
    message("No ",zrw[length(zrw)]," descendent packages installed in locally")
    print(Rvers)
    return(list())
  }
###print(pkginst)
  
  pkgin <- unique.default(intersect(zrwuq, lrwuq))
  names(pkgin) <- pkgin
  if(length(pkginst) <= 0 && length(pkgin) <= 0){
    message("No ", zmat[1,1]," descendent packages installed in locally")
    return(Rvers)
  }
  
### no need just for testing.  Commented out because of warnings
### messages that I do not want to fix. 
###  if(any(sort(na.omit(pkgin)) != sort(na.omit(pkginst)))){

###    stop("Bad calculation of package installed")
###  }
  
  ind <- sapply(pkginst, match, lrw)
  
  matinst  <- NULL
  versinst <- NULL
  if(length(ind) > 0){
    
    matinst  <- localinst[ind,]
    pkgsinst <- matinst[, "Package"]
    versinst <- matinst[, "Version"]
    
    rownames(matinst) <- pkgsinst
    
  }
  lst <- list()
  ind <- 1:nrow(matinst)
  lst <- sapply(ind, function(n){
    vec <- paste(matinst[n, "Package"],"  ", matinst[n, "Version"],  sep="")
    return(vec)
  })
  lst <- sapply(lst, FUN="trim.blanks")
  lst <- unlist(lst)
  lst <- unique.default(lst)

  if(disp)
    res   <- menu(lst, graphics=disp)
  ix <- sapply(c("Package", "Version"), match, colnames(localinst))
  ix <- na.omit(unlist(ix))
  ret <- matinst[, ix]
  ret <- rbind(ret, R=c("R", vv))
  return(ret)}

### DESCRIPTION stand alone function that gets the R version
###             in the local environment and, if zmat is provided,
###             in the environment were Zelig was built. We can
###             also obtained the matrix zmat from 'file'.
###
### OUTPUT: the R version in the local and Zelig environments

getRVersion <- function(zmat=matrixDependencies())
{

  if(length(zmat) <= 1 && zmat=="")
    stop("Provide matrix dependencies")

  no1 <- sessionInfo()$R.version$major
  no2 <- sessionInfo()$R.version$minor
  vv  <- paste(no1,".",no2, sep="")
  Rvers <- paste("R ", no1,".",no2, sep="")
  lst <- c(local=Rvers)
  

  rw  <- rownames(zmat)
  rw  <- sapply(rw, FUN="trim.blanks")
  
  ln  <- nrow(zmat)
  pkgorg <- zmat[ln,"Package"]
  lv0 <- trim.blanks(zmat[ln,"depth"])
  if(lv0 != "0"){
    ln <- grep("^0$", zmat[,"depth"])
    if(length(ln)) pkgorg <- zmat[ln,"Package"]
  }
  
  if(length(dim(zmat)) > 0)
    Rserver <- zmat[ln, "Node"]
  else
    Rserver <- zmat[["Node"]]
  nm  <- names(lst)
  lst <- c(lst, Rserver)
  names(lst) <- c(nm, pkgorg)
  
  lst <- sapply(lst,function(m) strsplit(m, " ")[[1]][2])
  return(lst)
  
}
### DESCRIPTION stand alone function that gets version of Zelig
###             in the local environment and, if zmat is provided,
###             in the environment were Zelig was built. We can
###             also obtained the matrix zmat from 'file'.
###
### OUTPUT: version of Zelig in both the local and Zelig environments

getZeligVersion <-  function(zmat=matrixDependencies(),lib.loc=NULL){
  
  if(length(zmat) <= 1 && zmat=="")
    stop("Provide matrix dependencies")

  pk <- "Zelig"
  
  if(class(zmat) != "try-error"){
    if(length(dim(zmat)) > 0)
      { ### make sure itr is level "0"
        ln <- nrow(zmat)
        pk <- zmat[ln,"Package"]
        lv0 <- trim.blanks(zmat[ln,"depth"])
        if(lv0 != "0"){
          ln <- grep("^0$", zmat[,"depth"])
          if(length(ln)) pk <- zmat[ln,"Package"]
        }
      }else
    pk <- zmat[["Package"]]
  }
  
  desc <- try(packageDescription(pk,lib.loc=lib.loc), silent=TRUE)
  Zvers <- NULL
  Zvers <- desc$Version
  if(length(Zvers) <= 0)
    message(pk," is not installed locally")

  lst <- c(local=Zvers)
  if(class(zmat) =="try-error")
    return(lst)
  rw  <- rownames(zmat)
  rw  <- sapply(rw, FUN="trim.blanks")
  ix  <- c( grep("Zelig", rw),grep("zelig", rw)) 
  if(length(ix) <= 0) ix <- nrow(zmat)
  if(length(dim(zmat)) >0)
    Zserver <- zmat[ix, "Version"]
  else
    Zserver <- zmat[["Version"]]

  lst <- c(lst, Zideal=Zserver)
  names(lst) <- c("local", pk)
  return(lst)
  
}
###DESCRIPTION: Takes the matrix of Zelig dependencies, zmat
###             and the name of the library to search for Zelig.
###             Finds the versions locally and in the server
###             Returns a vector with versions and report differences
###
###
compareZeligVersion <- function(zmat=matrixDependencies(),lib.loc=NULL){
  vec <- getZeligVersion(zmat,lib.loc)
  if(length(vec) <= 0)
    return(vec)
### only one row
  
  localvers <- vec[["local"]]
  ix <- grep("local", names(vec))
  
  zvers <- vec[[-ix]]
  
  res <- compareVersion(localvers, zvers)
  if(res != 0){
    message("Local installed version for ",names(vec)[2] , " differ from Zideal")
    return(list())
  }
  return(vec)
}



### 
### DESCRIPTION: Returns/Creates matrix of details corresponding to packages
###              Zelig depends on directly or indirectly
###              and are NOT installed in the local environment.
###              Every row corresponds to  a dependent package with the name,
###              the version, the parent (node), relation, URL to download it,
###              and depth in graph or level of dependency. 
###
### USES:        installed.packages()
###              
### USE   zmat is the matrix return with create.zelig.all.packages("Zelig")
###       zmat <- zelig.all.packages(matrixDependencies()),
###       Calls zelig.installed.packages, and will
###       find derived packages installed locally,
###       i.e. required or derived from zelig and live in local machine
###       Compares then and returns new packages 
###       level, which depth of dependencies pkg are derived from; default 1
###       cache, use the installed.packages matrix from environmnet or  calculate
###              from fresh as in noCache of installed.packages
###
### INPUT lib.loc libraries directories to search for.
###        zmat is matrix of dependencies stored in system files
###         matin, matrix with packages installed locally that belongs to zmat
###         matin <- zelig.installed.packages(zmat,cache=noCache,libpath=lib.loc)
###         level, the depth from root package
###        (level of dependency of Zelig package)
###       
### OUTPUT: Matrix similar to available.packages,newpkgmat, but only those packages
###         that Zelig depends on and that are NOT installed in the local machine.
###         Rows for each package that Zelig depends; columns are
###         the name, version, parent (node) and type of dependency, URL,
###         and level of graph
###         
### AUTHOR Elena Villalon
###        evillalon@iq.harvard.edu
###
###        Last modified: 01/24/2007
### 
zelig.new.packages <- function(lib.loc=NULL,zmat=matrixDependencies(),matin=NULL,level=1 )
{
####Input arguments################
### present a graphic of results
  disp  <- FALSE
### whether to use cache information, correspond to noCache=!cache
  cache <- FALSE
#################################################

  pkgnm <- zmat[dim(zmat)[1],1]
  zrw <- zmat[, "Package"]
  zrwuq <- unique.default(zrw)
  lrw   <- rownames(matin)
### names packages installed in local machine derived from zelig
  lrw   <- unlist(suppressWarnings(sapply(lrw, FUN="trim.blanks")))
  lrwuq <- unique.default(lrw)
  dec <- 0
  if(length(grep("^R",lrwuq[length(lrwuq)], extended=TRUE)) > 0) dec <- 1
###  print(zrwuq)

  ind <- sapply(lrwuq, match, zrwuq)
  ind <- na.omit(unlist(ind))

  if(length(ind) < (length(lrwuq)-dec))
    stop("Bad calculation of zelig.installed.packages or bad input for locally installed")    
  
  if(length(ind) <= 0)
    stop("Provides the installed pkgs with zelig.installed.packages")
  
  if(length(zrwuq[ind]) >= length(zrwuq)){
    message("All ",pkgnm ," pkgs are installed.CHECK their versions")
    lst <- NULL
    return(lst)
  }

  pkgsnoinst <- zrwuq[-ind]
###  print(zrwuq[-ind])
  pkgnoin <- unique.default(setdiff(zrwuq, lrwuq))
  names(pkgnoin) <- pkgnoin

  if(length(pkgsnoinst) <= 0 && length(pkgnoin) <= 0){
    
    message("All ",  pkgnm, " pkgs are installed. CHECK their versions")
    lst <- NULL
    return(lst)
  }
  
###  if(length(na.omit(pkgnoin)) && length(na.omit(pkgsnoinst)))
###    if(any(sort(na.omit(pkgnoin)) != sort(na.omit(pkgsnoinst)))){
  
###    stop("Bad calculation of package installed")
###  }

  newpkgmat <- NULL
  pkgmod <- sapply(pkgsnoinst,function(m) paste("^",m,"$",sep=""))
  ind <- unlist(sapply(pkgmod,grep,zrw,extended=TRUE))
  
  if(length(ind) > 0){
    ind <- unlist(ind)
    ix <- suppressWarnings(grep.exact(zrw[ind], pkgsnoinst))$index ### you do not need this with extended=T
    if(length(ix) > 0) ind <- ind[-ix]
  }
  if(length(ind) > 0){
    newpkgmat <- zmat[ind,]
    rwn <- rownames(newpkgmat)
    rwn <- unlist(suppressWarnings(sapply(rwn, FUN="trim.blanks")))
    rownames(newpkgmat) <- rwn
  }


  ints  <- intersect(pkgsnoinst, zrw)
  rwz <- suppressWarnings(sapply(rownames(zmat), FUN="trim.blanks"))
  rwz <- unlist(rwz)
  rownames(zmat) <- rwz
  matnoin <- zmat[rownames(zmat) %in% ints,]
  if(length(matnoin)== ncol(zmat))
    matnoin <- matrix(matnoin, nrow=1)

  if(length(ind) <= 0){
    message("No new packages ")
    ##return(Rvers)  ## this var does not exists, r cmd check complains ## ferdi
    return (NULL)
  }
###for display
  nrw <- nrow(newpkgmat)
  if(length(nrw) <= 0){
    nrw <- length(newpkgmat)
    lst <- as.list(paste(newpkgmat["Package"],"  ", newpkgmat["Version"], sep=""))
  }else{
    ind <- as.list(1:nrw)
    lst <- sapply(ind, function(n){
      vec <- paste(newpkgmat[n, "Package"],"  ", newpkgmat[n, "Version"], sep="")
      
      return(vec)
    })
  }

  lst <- suppressWarnings(sapply(lst, FUN="trim.blanks"))
  lst <- unlist(lst)
  lst <- unique.default(lst)
### if(disp) ###controls the Selection of packages as well
  if(disp)
    res <- menu(lst, graphics=disp)
###select.list(ls)

  if(length(level) > 0 || !is.na(level))
    newpkgmat <- suppressWarnings(selectLevels(newpkgmat, level))
  
  return(newpkgmat)
}
### 
### DESCRIPTION: Returns/Creates matrix of details corresponding to packages
###              Zelig depends on directly or indirectly
###              and are  installed in the local machine.
###              Every row corresponds to  a dependent package with the name,
###              the version, the parent, the relation to the parent.
###              The version is the pkg version in the local machine, which
###              could not be the same as the server where Zelig is running. 
###
### USES:        installed.packages()
###              
### USE  matrixDependencies() with zideal 
###      matin is matrix of two columns (pkg, vers) with locally installed
###      packages that are in zideal (derived from Zelig) as obtained from 
###      zelig.installed.packages. 
###      level, which depth of dependencies pkg are derived from; default 1
###
### INPUT   lib.loc libraries directories to search for.
###         zmat, matrix with packages dependencies
###         matin, matrix with packages installed locally that belongs to zmat
###         matin <- zelig.installed.packages(zmat,cache=noCache,libpath=lib.loc)
###         level, the depth from root package
###
###       
### OUTPUT: Matrix similar to available.packages,but only those packages
###         that Zelig depends and that are NOT installed in the local machine.
###         Rows for each package that Zelig depends; columns are
###         the name, version, parent and type of relation (dependency)
###         from parent description.
###         
### AUTHOR Elena Villalon
###        evillalon@iq.harvard.edu
###
###        Last modified: 02/06/2007
###                   

zelig.old.packages <- function(lib.loc=NULL,zmat= matrixDependencies(),matin=NULL, level=1 )
{
####Input arguments################
### zmat is the matrix of dependencies stored in system files  
### present a graphic of results
  disp  <- FALSE
### whether to use cache information, correspond to noCache=!cache
  cache <- FALSE
#################################################
  
### colnames(zmat) "Package" "Version" "Node" "Relation" "URL" "depth"
  
  rwm <- rownames(matin)
  if(length(rwm) > 0) {
    rwm <- sapply(rwm, FUN="trim.blanks")
    ir <- grep("^R$",rwm)
    if(length(ir)){
      matin <- matin[-ir, ]
      rwm <- rwm[-ir]
    }
    rownames(matin) <- rwm
  }
  zrwm <- rownames(zmat)
  zrwm <- sapply(zrwm, FUN="trim.blanks")
  rownames(zmat) <- zrwm
  ind <- unlist(sapply(rownames(matin), match, rownames(zmat)))
  ind <- na.omit(ind)
  zmatinst <- zmat[ind, ]
 
  
  ix <- sapply(rownames(matin),match,rownames(zmatinst))
  ix <- na.omit(unlist(ix))
  zmatinst <- zmatinst[ix, ]
  diffmat <- NULL
  for(n in 1:nrow(matin))
    {
      lpkg  <- trim.blanks(matin[n, 1])
      lvers <- trim.blanks(matin[n, 2])
      zpkg  <- trim.blanks(zmatinst[n,"Package"])
      zvers <- trim.blanks(zmatinst[n,"Version"])
      zdepth <- trim.blanks(zmatinst[n,"depth"])
      if(!identical(lpkg, zpkg))
        stop("Packages name should be identicals")
      if(!identical(zvers,lvers)){
        diffmat <- rbind(diffmat,c(matin[n,],Zideal =zvers,depth=zdepth))
        rownames(diffmat) <- diffmat[,"Package"]
      }
    }
  if(length(diffmat) <= 0){
    message("All packages version are up-to-date")
    lst <- NULL
    return(lst)
  }
  
  if(!is.na(level))
    diffmat <- selectLevels(diffmat, level)
  if(length(diffmat) > 0)
    diffmat <- check.advance.versions(diffmat)
  
  return(diffmat)

}
### DESCRIPTION: Given the matrix output of zelig.new.packages, or zelig.old.packages
###              select the level of dependencies specified by the integer parameter depth
###              Returns mat with only the rows in depth
###
selectLevels <- function(mat, depth){
  
  if(is.na(depth))
    return(mat)
  
  d1z <- "1Z"

  if(!length(grep("1[z-Z]", depth)))
    depth <- as.numeric(depth)
  if(length(dim(mat))<=0 || dim(mat)[1] <= 1){
    nmvec <- names(mat)
    nmcol <- colnames(mat)
    ix  <- unique.default(c(grep("depth", nmvec),grep("depth", nmcol)))
    mat.depth <- mat[[ix]]
    if(length(grep("^1[z-Z]$",mat.depth,extended=TRUE))<=0){
      bool1 <-  as.numeric(mat.depth) <= depth 
    }else{
      bool1 <- mat.depth <= "1z" || mat.depth <= "1Z"
    }
    bool2 <- mat.depth <= as.character(depth)
    
    bool <- c(bool1, bool2)
    
    if(length(bool) && is.na(all(bool)))
      bool <- length(grep(depth,mat.depth))
    else if(length(bool2) && is.na(bool2))
      bool <- length(grep(depth,mat.depth)) || bool1
    else if(length(bool1) && is.na(bool1))
      bool <- length(grep(depth,mat.depth)) || bool2
    else
      bool <- length(grep(depth,mat.depth)) || bool2 || bool1
    
    if(bool) return(mat)
    
    message("No packages found at level selected of dependencies")
###    print(mat)
    return(list())
    
  }
  
  mat.depth <- mat[,"depth"]
  ord <- order(mat.depth)
  mat <- mat[ord, ]
  mat.depth <- mat[,"depth"]  
  
  ix   <- grep("^1[z-Z]*$|^0$", mat.depth)
  ln   <- length(ix)
  if(depth=="1z" || depth=="1Z")
     return(mat[sort(ix),])
  if(as.numeric(depth) <= 1)
    return(mat[sort(ix),])
  
  vv <- paste("^",2:depth,"$", sep="")
  
  ind <- unlist(sapply(vv, grep, mat.depth))
  if(length(ind) <= 0)
    return(mat[sort(ix), ])
  
  ix <- unique.default(c(ind, sort(ix)))
  
  return(mat[ix, ])
  
}
### DESCRIPTION: finds if the locally installed versions of
###              Zelig derived packages are higher than those
###              of the required by the Zelig installation or zideal.
###              If that is the case it rermoves the row corresponding
###              to the package from the matrix mat
###
### INPUT: matrix of 4 columns from zelig.old.packages
###        with name of pkg, version local, zelig version and depth of dependency
###
### OUTPUT same matrix mat but with possible rows removed.
###        Those rows correspond to packages that have local versions larger than Zelig versions
### helper function tro zelig.old.packages

check.advance.versions <- function(mat){
  ind.to.rm <- NULL
  if(length(mat) <= 0)
    return(mat)
### only one row
  if(length(dim(mat)) <= 0 )
    {
      localvers <- mat[["Version"]]
      ix <- grep("Version", names(mat))
      
      zvers <- mat[[3]]
      res <- compareVersion(localvers, zvers)
      if(res >= 1){
        message("Local installed version for ", mat["Package"], " higher than Zideal")
        return(list())
      }
      return(mat)
    }
  
  
### more than one package
  for(n in 1:nrow(mat)){
    localvers <- mat[n,"Version"]
    zvers <- mat[n,"Zideal"]
    res <- compareVersion(localvers, zvers)
    if(res >= 1){
      message("Local installed version for ", mat[n,"Package"], " higher than Zideal")
      ind.to.rm <- c(ind.to.rm, n)
      next;
    }
  }
  if(length(ind.to.rm) > 0)
    mat <- mat[-ind.to.rm, ]
  return(mat)
  
}

### DESCRIPTION: Compares version numbers of Zelig derived packages
###              that are installed in the local machine with those
###              from the environment where Zelig was built, dependency level first.
###              Also, obtaine the list of packages that are part of Zelig and
###              not installed in the local machine, level 1. 
###              Update the outdated packages and installed new pkgs on the fly.
###
### USES: zmat  matrix with dependencies as, zmat <- zelig.all.packages()
###       --OR-- zmat <- matrixDependencies()
###       zelig.installed.packages, zelig.old.packages and
###       zelig.new.packages, pkg.update
###
### INPUT  destdir to save the download packages
###        installWithVers as in install.packages, save pkg as YourCast_version#
###        noCache as in install.packages, get it from fresh
###        dependencies as in install.packages
###        repos repository to download from 
###        lib.loc directory path to search for packages
###
###
### Elena Villalon
### evillalon@iq.harvard.edu
###

zeligDepUpdate <- function(destdir=NULL,installWithVers=FALSE,lib.loc=NULL, repos="http://cran.r-project.org")
{
  
####Input arguments################
### zmat is the matrix of dependencies stored in system files  
  zmat  <- matrixDependencies()
### level of dependency
  level <- 1
### whether to install all dependent packages
  dependencies <- TRUE
### whether to use cache information, correspond to noCache=!cache
  noCache <- TRUE
####whether to install existent packages with older versions
  oldpkg <- TRUE
### whether to install newer packages in the dependency list of Zelig
  newpkg <- TRUE
#################################################
  vec <- compareZeligVersion(zmat,lib.loc)
  if(length(vec) <= 0)
    stop("Update ",zmat[nrow(zmat),"Package"])

  zrw <- zmat[, "Package"]
  zrwuq <- unique.default(zrw)
  if(length(zrwuq) < length(zrw))
    zmat <- zelig.all.packages(zmat) ### eliminates duplicates pkgs
  Rvers <- getRVersion(zmat)
  Rvers <- unlist(Rvers)
  val <- compareVersion(Rvers[[1]], Rvers[[2]])
  if(val != 0) stop("Local R version different from Zelig required version")
  dm <- dim(zmat)
  if(length(level) <= 0 || is.na(level))
    level <- check.max.depth(zmat)
  
  if(length(dm) <= 0 || dm[1]==1){
    message(zmat[1,1]," has no dependents")
    return(zmat)
  }else
  pkgnm <- zmat[dm[1], 1]

  if(!length(destdir))
    destdir <- getwd()
  
  zrw   <- rownames(zmat) ### names of packages derived from Zelig
  zrw   <- unlist(sapply(zrw, FUN="trim.blanks"))
  zrwuq <- unique.default(zrw)
  zold <- NULL
  znew <- NULL
  zinst <- zelig.installed.packages(zmat, disp=F, prior=NULL,cache=!noCache, libpath=lib.loc)
  
  if(oldpkg)
    zold  <- zelig.old.packages(lib.loc=lib.loc,zmat,zinst,1)
  
  if(newpkg)
    znew <-  zelig.new.packages(lib.loc=lib.loc,zmat,zinst,1)
  if(!length(zold) && !length(znew)) {
    message("All Zelig derived packages up-to-date")
    return(list())
  }

  if(length(zold)>0)    
    if(length(dim(zold)) <= 0)
      ret <- pkg.update(zold,zmat,depend=dependencies,versinst=installWithVers,destdir=destdir,repos=repos)
    else
      ret <- apply(zold,1, FUN="pkg.update",zmat,dependencies,installWithVers,destdir,repos)

  if(length(znew) >0)
    if(length(dim(znew)) <= 0)
      ret <- pkg.update(znew,zmat,depend=dependencies,versinst=installWithVers,destdir=destdir,repos=repos)
    else 
      ret <- apply(znew,1, FUN="pkg.update",zmat,dependencies,installWithVers,destdir,repos) 
  
}
### DESCRIPTION: helper function to install packages derived from Zelig that 
###              are either installed in the local machine and outdated or
###              they are not all 
###              
### INPUT: vec contains the name of package and the desired version to install
###        mat is the matrix of Zelig dependencies that is used to check the URL
###        dependencies same as in install.packages
###        repos repository to download from 
###
### USES: install.packages
###
pkg.update <- function(vec,mat,depend=F, versinst=T,destdir=NULL, repos="http://cran.r-project.org")
{
  pkg <- vec["Package"]
  names(pkg)  <- NULL
  pkg  <- trim.blanks(pkg)
  vers <- vec["Zideal"]
  names(vers) <- NULL
  ix  <- match(pkg, rownames(mat))
  url <- NULL
  if(!is.na(ix)){
    url <- try(mat[ix,"URL"], silent=T)
    
    if(class(url)=="try-error") url <- repos
    if(url=="CRAN") url <- repos
  }
  
  message("Trying url....", url)
  message("For package...", pkg)
  message("Destination directory for source files...", destdir)
  
  install.packages(pkg,repos=url,installWithVers=versinst,dependencies=depend,destdir=destdir)
  return(list())
}

trim.blanks <- function(x) {
### at the beginning of string"^" gets anny number (+) of white spaces
  f <- x
  if(length(x))
    f <- na.omit(x)
  
  if(length(f) <= 0)
    return(x)
  if(length(f)>1)
    print(f)
  if(f=="" )
    return(x)
  x <- sub("^[[:space:]]*(.*)", "\\1",x) ###get \n\t
  x <- sub('^ +', '', x) ###get white spaces
  
### at the ending of string"$" gets anny number (+) of white spaces
  
  x <- sub("(.*)[[:space:]]*$", "\\1", x)
  x <- sub(' +$', '', x)
  return(x)
}
### DESCRIPTION: Utility function to check the results of applying grep
###              grep may not get the exact full name but uses a loose
###              regex to get all names that contains the input words
###              For example grep("abc", c("abc", "pab", "dabcm", "clr, "abc""))
###              will return 1, 3, 5. This function eliminates 3, counting characters
###              NOTE: no need to apply this function if you use grep with extended=TRUE
###              grep("^describe$", c("describe", "le describe", "desc", "describeport"), extended=T)
###              gets only [1] 1
###
### NOTE: match will get the exact full string and will dismiss anything
###       that is not an exact match: match("abc", "pabcm")=NA; however,
###       it only finds the first occurance,
###       i.e. match("abc",c("pabcqr", "abc", "lmn","vabc","abc"))= 2
###       Same as grep("^abc$", c("pabcqr", "abc", "lmn","vabc","abc"), extended=T)
###  
### USES:        grep
###              
### INPUT:  matinst <- grep.exact(); outcome a vector of character
###         we want to check for correctness.
###         input is another vector of the strings
###         that need to be found in the outcome.  
###        
### OUTPUT: It checks that outcome and input contain the same values 
###         and eliminate those that are not exact match between outcome and input.
###         Returns outcome with all not exact matches eliminated; and
###         index ix of the strins that were eliminated from the
###         original outcome.
###         
### AUTHOR Elena Villalon
###        evillalon@iq.harvard.edu
###
###        Last modified: 01/24/2007
### 
grep.exact <- function(outcome, input){
  ind <- 1:length(outcome) 
  names(ind) <- outcome
  
  ix <- sapply(ind, function(n){
    ret <- NULL
    if(length(outcome[n]) <= 0)
      return(ret)
    nm  <- trim.blanks(outcome[n])
    pkg <- trim.blanks(input[n])
    
    if(nchar(nm) != nchar(pkg))
      ret <- n
    return(ret)})
  ix <- unlist(ix)
  if(length(ix) > 0)
    outcome <- outcome[-ix]
  lst <- list(list(outcome=outcome), list(index=ix))
  return(lst)
}
### DESCRIPTION Helper function. If the package have attached the version number
###             it removes them.Example, "YourCast_2.9-8" becomes "YourCast"
###
getRidVersion <- function(zmat, oneonly=FALSE){
  nm <- NULL
  
  if(oneonly && length(grep("_", zmat)) <= 0)
    return(zmat)
  else if(oneonly){
    nm <- sub("(.*)_([-0-9.]*)","\\1", zmat)
    nm <- trim.blanks(nm)
    return(nm)
  }

  if(length(dim(zmat)) <= 0 || dim(zmat)[1] <=1){
    pkginst <- zmat["Package"]
    
  }else{
    pkginst <- zmat[,"Package"]
  }
  
  pkginst <- sapply(pkginst, function(nm){
    if(length(grep("_", nm)) <= 0)
      return(nm)
    nm <- sub("(.*)_([-0-9.]*)","\\1", nm)
    nm <- trim.blanks(nm)
  })

  pkginst <- unlist(pkginst)
  if(length(dim(zmat)) <= 0|| dim(zmat)[1] <=1){
    zmat["Package"] <- pkginst
    
  }else{

    zmat[,"Package"] <- pkginst
    rownames(zmat) <-  zmat[,"Package"]
    
    
  }
  return(zmat)
}

###   
check.max.depth <- function(zmat,level="depth"){
  clnm <- colnames(zmat)
  ix <- grep(level, clnm)
  if(length(ix) <= 0){
    message("Level of dependency not included")
    return(NULL)
  }
  vec <- zmat[,ix]
  vec[vec == "1z"] <- "1"
  vec[vec == "1Z"] <- "1"
  return(max(as.numeric(vec)))
  
}
###DESCRIPTION takes a file that stores the matrix with Zelig
###            dependencies as obtained create.zelig.all.packages
###            Returns the object or matrix stored in the file
###            If uniquePkg=T, then it eliminates duplicates pkgs in the matrix
###            of dependencies and only the lowest dependency level is included.
###
matrixDependencies <- function(file=system.file("zideal", "zideal.RData", package="Zelig"), uniqPkg=TRUE){
  zmat <- try(eval(as.symbol(load(file))))
  if(class(zmat) == "try-error"){
    message("Bad input file ", file)
    return(NULL)
  }
  if(uniqPkg)
    zmat <- zelig.all.packages(zmat)
  return(zmat)
}
###
### DESCRIPTION: Compares the packages in Zelig matrix of dependencies,
###              from the environment where Zelig was built, 
###              with those that are installed in the local machine 
###              at dependency level first, level=1.
###              Finds those packages with older versions and thoes that are not installed 
###              
###
### INPUT: lib.loc library to find locally packages that are installed
###
### USES:zelig.installed.packages, zelig.old.packages and
###      zelig.new.packages, getRVersion, compareZeligVersion
###
### OUTPUT list with tow elements, znew for Zelig packages that are not installed
###        and zold for Zelig packages that are installed wqith older versions. 
###      
### Elena Villalon
### evillalon@iq.harvard.edu
###
zeligDepStatus <- function(lib.loc=NULL)
{
####Hiden input arguments################
### zmat is the matrix of dependencies stored in system files  
  zmat  <- matrixDependencies()
### matin matrix of locally installed packages
  matin <- NULL
### level of dependency
  level <- 1
### present a graphic of results
  disp  <- FALSE
### whether to use cache information, correspond to noCache=!cache
  cache <- FALSE
#################################################
  vec <- compareZeligVersion(zmat,lib.loc)
  if(length(vec) <= 0)
    stop("Update ",zmat[nrow(zmat),"Package"])
  lst <- getRVersion(zmat)
  lst <- unlist(lst)
  val <- compareVersion(lst[[1]], lst[[2]])
  if(val != 0)
    stop("User R version different from Zelig required R version")
  
  if(length(level) <= 0 || is.na(level))
    level <- check.max.depth(zmat)

  zrw   <- rownames(zmat) ### names of packages derived from Zelig
  zrw   <- unlist(suppressWarnings(sapply(zrw, FUN="trim.blanks")))
  zrwuq <- unique.default(zrw)

  if(length(zrwuq) < length(zrw)){
    zmat <-  zelig.all.packages(zmat)
    zrw   <- rownames(zmat)
    zrw   <- unlist(suppressWarnings(sapply(zrw, FUN="trim.blanks")))
    zrwuq <- unique.default(zrw)
  }
 
  dm <- dim(zmat)
  
  if(length(dm) <= 0||dm[1]==1){
    message(zmat[1,1]," has no dependents")
    return(zmat)
  }else
  pkgnm <- zmat[dm[1], 1]
  
###  colnames(localinst)
###  "Package"  "LibPath"  "Version"  "Priority" "Bundle"   "Contains"
###  "Depends"  "Imports"  "Suggests" "Built"
###  rownames(localinst)    names of the packages installed

  if(length(matin) <= 0)
    matin <- zelig.installed.packages(zmat,disp=F,cache=cache,libpath=lib.loc)
  if(all(is.na(matin))|| length(matin) <= 0)
    {
      message("No packages derived from ",pkgnm, " are installed locally")
      if(length(level) > 0 || !is.na(level))
        newpkgmat <- suppressWarnings(selectLevels(zmat, level))
      else
        newpkgmat <- zmat
      
      tmp <- cbind(newpkgmat[,"Package"], rep(NA,nrow(newpkgmat)), newpkgmat[,"Version"])
      colnames(tmp) <- c("Package", "Version", "Zideal")
      ret <- c(znew=tmp)
      return(ret)
    }
  
###matin packages that zelig depends on and are installed locally:
### matin has two columns pkg name and local version

  zold <- zelig.old.packages(lib.loc, zmat, matin,level)
  dmold <- dim(zold)
  if(length(dmold) && dmold[1]>=1)
    zold <- zold[, c("Package", "Version", "Zideal")]
  else if(length(zold))
    zold <- zold[c("Package", "Version", "Zideal")]
  else
    zold <- NULL
 
  znew <- zelig.new.packages(lib.loc, zmat,matin,level)
  dmnew <- dim(znew)
  tmp <- znew
  if(length(dmnew) && dmnew[1] >= 1){
    tmp <- cbind(znew[,"Package"], rep(NA,nrow(znew)), znew[,"Version"])
    colnames(tmp) <- c("Package", "Version", "Zideal")
  }else if(length(znew)){
    tmp <- cbind(Package=znew["Package"], Version=rep(NA,1), Zideal=znew["Version"])
   
  }else
  tmp <- NULL
    
  
  mat <- rbind(zold,tmp)
  if(length(dim(mat))){
    pkgnm <- mat[,"Package"]
    rownames(mat) <- pkgnm
  }else{
    pkgnm <-  mat["Package"]
   
  }
  return(mat)
}




