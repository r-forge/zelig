Diff <- function(Y, d, ds=NULL, per=NULL){
  mc <- match.call()
  name <- as.name(mc[[2]])
  list(name=name, d=d, ds=ds, per=per)
}
