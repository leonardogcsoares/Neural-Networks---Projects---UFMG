yperceptron <- function(xvec, w, par) {

    if (par == 1)
        xvec <- cbind(-1, xvec)

    u <- xvec %*% w
    # y <- as.double((xvec %*% w) >= 0)
    y <- as.double((u >= 0))

    return ((as.matrix(y)))
}
