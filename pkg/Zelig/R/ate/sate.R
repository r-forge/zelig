sate <- function(formula, model, treat, data, sims, zARGS = NULL, sARGS = NULL) {
  tmp <- as.formula(paste(deparse(formula[[2]]), deparse(formula[[1]]),
                          paste(treat, "+", deparse(formula[[3]]))))
  D <- model.frame(tmp, data = data)
  idx <- names(model.frame(formula, data = D))
  if (treat %in% idx) 
    one.reg <- TRUE
  else
    one.reg <- FALSE
  tname <- treat
  treat <- D[[treat]]
  check <- unique(treat)
  if (length(na.omit(check)) > 2)
    stop("Treatment indicator must be binary.")
  treat <- as.numeric(as.factor(treat))
  if (!all(unique(treat) %in% c(0,1))) {
    treat[treat == min(treat)] <- 0
    treat[treat == max(treat)] <- 1
  }
  if (one.reg) {
    z.out <- zelig(formula, data = D, model = model, ... = zARGS)
    x.out <- setx(z.out, fn = NULL, cond = TRUE)
    tidx <- match(tname, colnames(x.out))
    x.out[, tidx] <- 1 - x.out[, tidx]
    x.all <- x.out[, 2:ncol(x.out)]
    x.all <- as.data.frame(x.all)
    s.out <- sim(z.out, x = x.all[, 2:nrow(x.all)], num = sims, ... = sARGS)
    pr.all <- matrix(as.numeric(s.out$qi$pr), nrow = sims, ncol = nrow(x.all))
### replace with model.response
    y.all <- matrix(x.out[,1], nrow = sims, ncol nrow(x.out), byrow = TRUE)
    te.all <- y.all - pr.all
    te.all[, treat == 0] <- -1 * te.all[, treat == 0]
    s.out$qi <- s.out$qi.name <- NULL
    s.out$qi <- list(sate = apply(te.all, 1, mean),
                     satt = apply(te.all[, treat == 1], 1, mean))
    s.out$qi.name <- list(sate = "Sample Average Treatment Effect for Everyone: E[Y(1)-Y(0)]",
                          satt = "Sample Average Treatment Effect for Treated: E[Yobs(1) - Ymiss(1)]")
    return(s.out)
  }
  else {
    z0 <- zelig(formula, data = D[treat == 0,], model = model, ... = zARGS)
    z1 <- zelig(formula, data = D[treat == 1,], model = model, ... = zARGS)
    x0 <- setx(z0, fn = NULL, cond = TRUE)
    x1 <- setx(z1, fn = NULL, cond = TRUE)
    tidx <- match(tname, colnames(x0))
    x0[, tidx] <- 1 - x0[, tidx]
    x1[, tidx] <- 1 - x1[, tidx]
    y0 <- 
    
    s0 <- sim(z0, x = x0, num = round(sims / 2), ... = sARGS)
    s1 <- sim(z1, x = x1, num = round(sims / 2), ... = sARGS)
    pr.all <- rbind(s0$qi$pr, s1$qi$pr)
    y.all <- 
