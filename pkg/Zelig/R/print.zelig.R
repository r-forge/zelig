print.zelig <- function (x, digits = max(3, getOption("digits") - 3), ...) 
{
    cat("\nModel:", x$zelig.call$model, "\n", sep = " ")
    cat("Number of simulations:", x$call$num, "\n\n", sep = " ")
    idx <- unlist(x$qi.name)
    cat("Available Quantities of Interest: \n")
    for (i in 1:length(x$qi.name)){
      cat(paste("  qi$", names(x$qi)[i], " = ", idx[[i]], "\n", sep = ""))
    }
    cat("\nPlease use summary() to obtain more information. \n")
    invisible(x)
}
