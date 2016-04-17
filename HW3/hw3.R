rm(list=ls())
library('plot3D')
source(file='C:\\Users\\leonardogcsoares\\RStudio Projects\\Redes Neurais Projects\\HW3\\trainperceptron.R')
source(file='C:\\Users\\leonardogcsoares\\RStudio Projects\\Redes Neurais Projects\\HW3\\yperceptron.R')

# data(iris)
s1 <- 0.4
s2 <- 0.4
nc <- 200
# Classe 1 e 2
xc1 <- matrix(rnorm(nc*2), ncol=2)*s1 + t(matrix((c(2,2)), ncol=nc, nrow=2))
xc2 <- matrix(rnorm(nc*2), ncol=2)*s2 + t(matrix((c(4,4)), ncol=nc, nrow=2))

plot(xc1[, 1], xc1[, 2], col='red', xlim=c(0, 6), ylim=c(0, 6), xlab='x_1', ylab='x_2')
par(new=T)
plot(xc2[, 1], xc2[, 2], col='blue', xlim=c(0, 6), ylim=c(0, 6), xlab='', ylab='')

x1_reta <- seq(6/100, 6, 6/100)
x2_reta <- -x1_reta + 6
par(new=T)
plot(x1_reta, x2_reta, type='l', col='orange', xlim=c(0, 6), ylim=c(0, 6), xlab='', ylab='')

## Train perceptron here
ntrain <- 30
sequenceC1 <- sample(100)
xc1treina <- xc1[sequenceC1[1:ntrain],]
yc1treina <- matrix(0, nrow=ntrain)
sequenceC2 <- sample(100)
xc2treina <- xc2[sequenceC2[1:ntrain],]
yc2treina <- matrix(1, nrow=ntrain)

xc1teste <- xc1[sequenceC1[(ntrain+1):100],]
yc1teste <- matrix(0, nrow=(100-ntrain))
xc2teste <- xc2[sequenceC2[(ntrain+1):100],]
yc2teste <- matrix(0, nrow=(100-ntrain))

xin<-as.matrix(rbind(xc1treina, xc2treina))
yd <- rbind(yc1treina, yc2treina)
# xInTest <- as.matrix(rbind(xc1teste, xc2teste))
# yTest <- rbind(yc1teste, yc2teste)

retlist <- trainperceptron(xin, yd, 0.1, 0.01, 100, 1)
w <-as.matrix(unlist(retlist[1]))

seqi <- seq(0, 6, 0.1)
seqj <- seq(0, 6, 0.1)
M <- matrix(0, nrow=length(seqi), ncol=length(seqj))

ci <- 0
for (i in seqi)      {
    ci <- ci + 1
    cj <- 0
    for (j in seqj) {
        cj <- cj + 1
        x <- as.matrix(cbind(i, j))
        # testPerceptron <- yperceptron(x, w, 1)
        M[ci, cj] <- yperceptron(x, w, 1)
        # M[ci, cj] <- 1
    }
}

plot(xc1[, 1], xc1[, 2], col='red', xlim=c(0, 6), ylim=c(0, 6), xlab='x_1', ylab='x_2')
par(new=T)
plot(xc2[, 1], xc2[, 2], col='blue', xlim=c(0, 6), ylim=c(0, 6), xlab='', ylab='')
par(new=T)
contour(seqi, seqj, M, xlim=c(0, 6), ylim=c(0, 6), xlab='', ylab='')

persp3D(seqi, seqj, M, counter=T, theta=55, phi=30, r=40, d=0.1, expand=0.5,
    ltheta=90, lphi=180, shade=0.4, ticktype="detailed", nticks=5)
