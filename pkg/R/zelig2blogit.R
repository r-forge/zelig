zelig2blogit <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf[[1]] <- VGAM::vglm
  mf$family <- as.name("blogit")
  mf$... <- NULL
  formula<-parse.formula(formula,model)
  tmp <- cmvglm(formula, model, 3)
  mf$formula <- tmp$formula 
  mf$constraints <- tmp$constraints

  mf$model <- mf$constant <- mf$M <- NULL
  mf$robust <- NULL
  as.call(mf)
}

  blogit <- function() binom2.or(zero=NULL)
