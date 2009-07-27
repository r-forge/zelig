packageConflicts <- function(str) {
  paths <- search()
  str <- paste("package:", str, sep = "")
  if (str %in% paths)
    do.call(detach, list(str))
  return(invisible(0))
}
