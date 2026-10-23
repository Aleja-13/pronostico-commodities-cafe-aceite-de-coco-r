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
require(RSNNS)
require(quantmod)

#Cargar Datos
CocoOil <- read.csv("datos/7.csv")

#Lo ejecutamos como una serie de tiempo
CocoOil.ts <- ts(log(CocoOil), start = 1960, frequency = 12)

#Graficamos
autoplot(CocoOil.ts, colour = "#080d4a", plot = F)+
  ggtitle("Coconut Oil")+
  ylab("Indice General")+
  xlab("TIME")

ndiffs(CocoOil.ts)
#Diferenciamos la serie.
diff.CocoOil.ts  <- diff(CocoOil.ts, lag = 1)

autoplot(diff.CocoOil.ts) +
  ggtitle("Diferencias Coconut Oil") +
  ylab("Índice General")

#Test Dickey Fuller
adf.test(diff.CocoOil.ts)

#Modelo ARIMA
model <- auto.arima(diff.CocoOil.ts, trace = T, stepwise = F, approximation =F, max.d = 0)
print(model)
checkresiduals(model)

#Estimacion del modelo
pred <- forecast(Y, 8, level = 95)
plot(pred)

#Como red neuronal
###definimos la variable train
train<-1:700

#variables de entrenamiento de la serie
y<-as.zoo(CocoOil.ts)
x1<-Lag(y,k=1)
x2<-Lag(y,k=2)
x3<-Lag(y,k=3)
x4<-Lag(y,k=4)
x5<-Lag(y,k=5)
x6<-Lag(y,k=6)
x7<-Lag(y,k=7)
x8<-Lag(y,k=8)
x9<-Lag(y,k=9)
x10<-Lag(y,k=10)
slog<-cbind(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)

#Eliminamos los valores NA producidos
slog<-slog[-(1:10),]

####valores de entrada y salida de la red neuronal
inputs<-slog[,2:11]
outputs<-slog[,1]

#crear una red de Elman y entrenarla
fit<-elman(inputs[train],
           outputs[train],
           size=c(3,1),
           learnFuncParams=c(0.01),
           maxit=10000)

plotIterativeError(fit)

#Prediccion
y<-as.vector(outputs[-train])
plot(y,type="l")

#predicción sobre la serie original
pred<-predict(fit,inputs[-train])
lines(pred,col="red")

#prueba con una red de Jordan

fit2<-jordan(inputs[train],
            outputs[train],
            size=6,
            learnFuncParams=c(0.0001),
            maxit=100000)

plotIterativeError(fit2)


pred<-predict(fit2,inputs[-train])
plot(y,type="l")
lines(pred,col="red")

print(fit2)
