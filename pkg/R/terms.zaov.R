terms.zaovlist <- function(object,...){
        Terms<- attr(object,"terms")
        indError <- attr(Terms, "specials")$Error
        if (length(indError)){
                errorterm <- attr(Terms, "variables")[[1 + indError]]
                formula <- update.formula(Terms, paste(". ~ .-", deparse(errorterm, 
                           width = 500, backtick = TRUE), "+", deparse(errorterm[[2]], 
                           width = 500, backtick = TRUE)))
                terms.formula(formula)
        } else {
                terms.formula(object)
        }
}
