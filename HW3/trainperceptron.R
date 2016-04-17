# Trains a simple perceptron
# xin: Input Nxn dataset matrix
# eta: weight update step
# tol: error goal
# maxepocas: maximum number of epochs allowed
#par: par == 1 indicates that -1 needs to be augmented to xin
trainperceptron <- function (xin, yd, eta, tol, maxepocas, par) {

    dimxin <- dim(xin)      # Dimension of the dataset
    N <- dimxin[1]            # Number of patterns
    n <- dimxin[2]            # Input dimension

    if(par == 1) {
        wt <- as.matrix(runif(n+1) - 0.5)
        xin <- cbind(-1, xin)
    }
    else
        wt <- as.matrix(runif(n) - 0.5)

    nepocas <- 0
    eepoca <- tol + 1

    #Initialize vector error
    evec <- matrix(nrow=1, ncol=maxepocas)
    while ((nepocas < maxepocas) && (eepoca > tol)) {
        ei2 <- 0

        xseq <- sample(N)
        #Random sequence for training
        for (i in 1:N) {
            # Pattern for random sequence
            irand <- xseq[i]
            yhati <- as.double((xin[irand, ] %*% wt) >= 0)
            ei <- yd[irand] - yhati
            dw <- eta * ei * xin[irand, ]

            # Weight update
            wt <- wt+dw

            # Accumulate error
            ei2 <- ei2 + ei*ei
        }

        # Number of epochs
        nepocas <- nepocas + 1
        evec[nepocas] <- ei2/N

        #Error per epoch
        eepoca <- evec[nepocas]
    }

    retlist <- list(wt, evec[1:nepocas])
    return (retlist)
}
