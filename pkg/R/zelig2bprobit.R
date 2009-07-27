zelig2bprobit <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf[[1]] <- VGAM::vglm
  mf$family <- as.name("bprobit")
   formula<-parse.formula(formula,model)
  tmp <- cmvglm(formula, model,3)
  mf$formula <- tmp$formula
  mf$constraints <- tmp$constraints

  mf$model <- mf$constant <- mf$M <- NULL

  as.call(mf)
}

  bprobit <- function() binom2.rho(zero=NULL)

