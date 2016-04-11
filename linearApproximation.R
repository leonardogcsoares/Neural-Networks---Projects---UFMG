rm(list=ls())
library('corpcor')

# Geracao de dados

N<-20
x<-runif(n=N, min=-15, max=10)
xgrid-<seq(-15,10,0.1)
yr<-(0.5*x^2 + 3*x + 10) + 10*rnorm(length(x))
ygrid<-(0.5*xgrid^2 + 3*xgrid + 10)

#Aproximacao de dois graus
H<-cbind(x^2,x,1)
w<-pseudoinverse(H) %*% yr

#Aproximacao
Hgrid<-cbind(xgrid^2,xgrid,1)
yhat<-H %*% w
yhatgrid<-Hgrid %*% w