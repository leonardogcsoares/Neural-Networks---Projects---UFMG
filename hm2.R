rm(list=ls())         #Clear local ambient
library('corpcor')    #Imports "pseudoinver"
library('boxplotdbl') #Imports "boxplot"


#Sample Generation function
fg <- function(x) {
    return (0.5*x^2 + 3*x + 10)
}

# Generates the H matrix given the X vector and number of degrees desired.
generateH <- function(x, degrees) {
    X <- matrix(ncol=degrees, nrow=length(x))
    for (i in 1:degrees) {

        # Guarentees that the last exponential will be 1, not 0
        exponential <- degrees - i + 1

        X[,i] <- x^exponential
    }

    #Adds the last column of 1's
    X <- cbind(X, rep(1, length(x)))
}

#Sample parameters
N <- 20
xMin <- -15
xMax <- 10
numDegrees <- 10

averageQuadraticError <- matrix(ncol=numDegrees, nrow=20)
variance <- matrix(ncol=numDegrees, nrow=20)

for (row in 1:20) {
    #Generating the samples
    xRand <- runif(n=N, min=xMin, max=xMax)
    yRand <- fg(xRand) + 10*rnorm(N)

    # Plot of the samples generated
    # plot(xRand, yRand, type='p', col='red', xlim=c(-15, 10), ylim=c(-10, 120), xlab='x', ylab='y')

    quadraticErrors <- matrix(ncol=numDegrees, nrow=N)
    for (i in 1:numDegrees) {

        H <- generateH(xRand, i)
        weights <- pseudoinverse(H) %*% yRand

        p <- H %*% weights
        # Average Quadratic Error calculation
        quadraticErrors[,i] <- (fg(xRand) - p)^2
        sum <- 0
        for (j in 1:length(quadraticErrors)) {
            sum <- sum + quadraticErrors[i]
        }
        # averageQuadraticError[row,i] <- sum/N
        averageQuadraticError[row, i] <- sum/length(quadraticErrors)

        # Variance calculation
        sum <- 0
        meanX <- mean(p)
        for (j in 1:length(p)) {
            sum <- sum + (p[i] - meanX)^2
        }
        variance[row, i] <- sum/length(p)
    }
}

boxplot(averageQuadraticError, main="Erro Médio Quadrático", xlab="Degree")
boxplot(variance, main="Variancia", xlab="Degree")
