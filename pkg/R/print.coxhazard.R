print.coxhazard <- function(x,...){
  haz <- matrix(x, ncol=1)
  colnames(haz) <- "hazard"
  rownames(haz) <- rownames(x)
  print(haz)
}


