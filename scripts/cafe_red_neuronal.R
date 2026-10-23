#Librerias
library(tidyverse)
library(readxl)
library(forecast)
library(lubridate)
library(tseries)
library(autoplotly)
library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)

#Cargar Datos
Coffee <- read.csv("datos/cafe_arabica.csv")

#Lo ejecutamos como una serie de tiempo
Y <- ts(Coffee, start = 1960, frequency = 12)
Y

#Graficamos
par(mfrow = c(1,2))
g1<-autoplot(Y, colour = "#6B8E23", plot = F)+
  ggtitle("Coffee")+
  ylab("Indice General")+
  xlab("TIME")
g1
g2<-autoplot(acf(Y, plot = F), conf.int.fill = "#CDC8B1", main="Autocorrelación Simple",ylim=c(-1,1),ci.col="black",ylab="",col = "#6B8E23")
g2
grid.arrange(g1,g2)

#Descomponemos la serie
descomp = decompose(Y)
autoplot(descomp)

ndiffs(Y)

#Diferenciamos la serie.
DY <- diff(Y, lag = 1)
autoplot(DY) +
  ggtitle("Diferencias Coffee") +
  ylab("Índice General")

autoplot(acf(DY, plot = F), conf.int.fill = "#CDC8B1", main="Autocorrelación Simple",ylim=c(-1,1),ci.col="black",ylab="",col = "#6B8E23")

#Test Dickey Fuller
adf.test(DY)

#Modelo ARIMA
model <- auto.arima(DY, trace = T, stepwise = F, approximation =F, max.d = 0)
print(model)
checkresiduals(model)
dev.off()

#Estimacion del modelo
predARIMA <- forecast(model, 8, level = 95)
plot(predARIMA)
last_Y = Y[85]
pred_mod1 = predARIMA$mean
pron_real = cumsum(pred_mod1) + last_Y

pron_real_ts = ts(pron_real, start = c(2019,2), frequency=12)
ts.plot(Y, pron_real_ts, lty = c(1,3), col = c(5,2))

#Como red neuronal

set.seed(42)
neural_network <- nnetar(Y)
autoplot(neural_network)

checkresiduals(neural_network)


#Prediccion
predRN <- forecast(neural_network, h=12, level = 95)
plot(predRN)

#Pronostico
pronostico <- as.data.frame(predmodeloRN)
pronostico
