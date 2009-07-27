mi <- function(...) {
  res <- list(...)
  class(res) <- c("mi", "list")
  return(res)
}
