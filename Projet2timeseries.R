install.packages("xts")
install.packages("stats")
install.packages("astsa")
install.packages("forecast")

library(forecast)
library(AER)
library(tswge)
library(xts)
library(stats)
library(astsa)

#traitement de la base de données
setwd("C:/Users/Luke/Desktop/Time series")
x <- read.table("simdata.csv", header = TRUE)

mode(x)
View(x)
n <- length(x)

data = x[,2]
data = as.numeric(data)
aux = plotts.sample.wge(data)
aux = acf2(data)

mode(data)
#simulation arma par rapport au jeu de données
result <- est.arma.wge(data,p = 3,q=1)
result$phi
result$theta
par.ar <- rbind(result$phi, result$se.phi, result$theta,result$se.theta)
row.names(par.ar) <- c("phi","phi.se","theta","theta.se")
print(par.ar,digits=3)
sim = plotts.true.wge(n = n, theta = result$theta, phi = result$phi)
ddata = diff(data)
aux = plotts.sample.wge(ddata)

ldata = log(data)
dldata = diff(ldata)
aux = plotts.sample.wge(dldata, lag = 40)
factor.wge(dlata$phi)
aux = acf2(dldata)


?arima
#recherche automatique d'une arima par rapport au jeu de données

auto = auto.arima(dldata)

#Simulation arima
n = length(dldata)
arima1 = arima(ldata, order = c(2,1,3))
coef(arima1)
#recherche automatique d'une arima par rapport au jeu de données
auto = auto.arima(dldata)

#Simulation arima
n = length(dldata)
arima1 = arima(ldata, order = c(13,3,2))
factor.wge(phi)
coef(arima1)
#coeff = c(arima1$coef[c(1,5)], -arima1$coef[c(2:4)])
coeff = c(arima1$coef)
phi = coeff[1:13]
theta = coeff[14:15]
print(coeff,digits = 3)
sim = plotts.true.wge(n=n, theta = theta, phi = phi, vara = arima1$var.coef)
aux = acf2(sim$data)
?arima
