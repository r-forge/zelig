ternarypoints <- function(object, pch = 19, col = "blue", ...){
    s <- rowSums(object)
    if (any(s <= 0))
        stop("each row of the input `object' must have a positive sum")
    object <- object/s
    top <- sqrt(3)/2
    xp <- object[, 2] + object[, 3]/2
    yp <- object[, 3] * top
    points(xp, yp, pch = pch, col = col, ...)
}
