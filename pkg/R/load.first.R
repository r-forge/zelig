.onAttach <- function(...) {
  mylib <- dirname(system.file(package = "Zelig"))
  ver <- packageDescription("Zelig", lib = mylib)$Version
  builddate <- packageDescription("Zelig", lib = mylib)$Date
  cat(paste("## \n##  Zelig (Version ", ver, ", built: ", builddate, ")\n", sep = "")) 
  cat("##  Please refer to http://gking.harvard.edu/zelig for full\n",
      "##  documentation or help.zelig() for help with commands and\n",
      "##  models supported by Zelig.\n##\n\n", sep="")
  cat("##  Zelig project citations:\n",
      "##    Kosuke Imai, Gary King, and Olivia Lau. (2009).\n", 
      "##    ``Zelig: Everyone's Statistical Software,''\n", 
      "##    http://gking.harvard.edu/zelig.\n",
      "##  and\n",
      "##    Kosuke Imai, Gary King, and Olivia Lau. (2008).\n", 
      "##    ``Toward A Common Framework for Statistical Analysis\n", 
      "##    and Development,'' Journal of Computational and\n",
      "##    Graphical Statistics, Vol. 17, No. 4 (December)\n", 
      "##    pp. 892-913. \n\n",
      "##  To cite individual Zelig models, please use the citation format printed with\n",
      "##  each model run and in the documentation.\n##\n", sep="")
  if(!any(search()=="package:MASS"))
    require(MASS) 
  if(!any(search()=="package:boot"))
    require(boot) 
  library.dynam("stats")

  ## add viggnettes menu
  addVigs2WinMenu("Zelig")

}
