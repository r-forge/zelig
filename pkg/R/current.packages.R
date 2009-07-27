current.packages <- function(package){

  required.packages <- function(pack) { 
    mylib <- dirname(system.file(package = pack))
    description <- packageDescription(pack, lib = mylib)       
    depends <- description$Depends
    if (!is.null(depends)) {
      depends <- strsplit(depends, ", ")[[1]]
      Rdepends <- pmatch("R (", depends)
      if (is.na(Rdepends)) {
        Rdepends <- pmatch("R(", depends)
        if (is.na(Rdepends))
          Rdepends <- match("R", depends)
      }
      if (!is.na(Rdepends)) 
        depends <- depends[-Rdepends]
    }
    suggests <- description$Suggests
    if (!is.null(suggests)) 
      suggests <- strsplit(suggests, ", ")[[1]]
    total <- c(depends, suggests)
    if (!is.null(total)) 
      total <- unlist(strsplit(total, "\n"))
    if (!is.null(total))
      total <- unlist(strsplit(total, ","))
    if (!is.null(total)) {
      conditions <- grep(")", total)
      if (length(conditions) > 0) { 
        for (i in conditions) 
          total[i] <- strsplit(total[i], " \\(")[[1]][1]
      }
      return(total)
    }
    else
      return(NULL)
  }
  old <- packages <- required.packages(package)

  check.start <- 1
  check.end <- length(packages)-1
  while(check.end < length(packages)) {
    check.end <- length(packages)
    for (i in check.start:check.end)
      packages <- c(packages, required.packages(packages[i]))
    check.start <- check.end+1
    packages <- na.omit(unique(packages))
  }

  ver <- array(NA, length(packages) + 1)
  for (i in 1:length(packages)) {
    mylib <- dirname(system.file(package = packages[i]))
    if (sum(!is.na(packageDescription(packages[i], lib = mylib))))
      ver[i+1] <- packageDescription(packages[i], lib = mylib)$Ver
    else
      stop()
    names(ver)[i+1] <- packages[i]
  }
  ver[1] <- paste(paste(paste(R.Version()$major, R.Version()$minor, sep = "."),
                        R.Version()$status, sep = " "),
                  R.Version()$svn, sep = " svn: ")
  names(ver)[1] <- "R"
  vv <- as.matrix(ver)
  colnames(vv) <- "Version"
  noquote(vv)
}

