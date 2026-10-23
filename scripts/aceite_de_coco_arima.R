Coconut Oil
#Instalamos Librerias
rm(list = ls())
library(tseries)
library(fpp2)
library(tseries)
library(quantmod)
library(ggplot2)
library(forecast)

#Agregamos los datos:
datos <- read.csv("datos/aceite_de_coco.csv")
View(datos)

# Ver la fecha de la primera observacion
head(datos)
tail(datos)

#Serie de tiempo
coil.ts <- ts(datos, start = c(1960,01), end = c(2021,07), frequency = 12)
#Graficamos
plot.ts(coil.ts)

# Los datos son mensuales; la serie no esta ajustada estacionalmente

datos<-as.numeric(datos$`Coconut oil`)

logCoil.ts <- ts(log(datos), frequency = 12, 
                 start = c(1960, 01), end = c(2021, 07))
autoplot(logCoil.ts)

#Nos fijamos en la descomposicion de la serie para mirar la tendencia
coil.dec <- decompose(logCoil.ts)
autoplot(coil.dec)

# Ajuste estacional
logcoil.nonseas <- logCoil.ts - seasonal(coil.dec)
autoplot(logcoil.nonseas)


# Prueba de estacionariedad serie con ajuste estacional
adf.test(logcoil.nonseas) # H0: serie no estacionaria
pp.test(logcoil.nonseas) # H0: serie no estacionaria
kpss.test(logcoil.nonseas) # H0: serie estacionaria
# ConclusiÃ³n: la serie con ajuste estacional no es estacionaria.

#Descomponemos la serie
coil.dec2 <- decompose(logcoil.nonseas)
autoplot(coil.dec2)

# Diferenciar la serie
diff.COIL <- diff(logcoil.nonseas)
autoplot(diff.COIL)

# Prueba de estacionariedad serie a la diferencia
adf.test(diff.COIL) # H0: serie no estacionaria
pp.test(diff.COIL) # H0: serie no estacionaria
kpss.test(diff.COIL) # H0: serie estacionaria


# Identificación del proceso
acf(diff.COIL)
# Se detecta patrón AR => usar PACF para determinar el posible orden
pacf(diff.COIL)
# p = 3

#Realizamos 3 modelos con la direfencia

modelo1 <- Arima(diff.COIL, order = c(3,1,0), include.constant = T)
modelo1 # Todos los coeficientes son significativos

modelo2 <- Arima(diff.COIL, order = c(3,1,1), include.constant = T)
modelo2 #no son significativos los coeficientes;

modelo.auto <- auto.arima(diff.COIL, seasonal = F,
                     max.p = 12, max.q = 12, allowdrift = T)
modelo.auto # ar1, ar2, ma1, no son significativos,  mejor AICc, BIC

checkresiduals(modelo1) 
checkresiduals(modelo2) 
checkresiduals(modelo.auto) 


### Realizamos las predicciones por longitud

l <- length(diff.COIL)
l.70.train <- .7*l
l.70.fcast <- l - l.70.train
logcoil.train.70.1 <- modelo1$fitted[1:l.70.train]
logcoil.train.70.2 <- modelo2$fitted[1:l.70.train]
logcoil.train.70.auto <- modelo.auto$fitted[1:l.70.train]
fcast.1.70 <- forecast(logcoil.train.70.1, h = l.70.fcast)
fcast.2.70 <- forecast(logcoil.train.70.2, h = l.70.fcast)
fcast.auto.70 <- forecast(logcoil.train.70.auto, h = l.70.fcast)
valid.series.70 <- window(ts(diff.COIL), start = l.70.train+1)
plot(fcast.1.70, main = "Validación pronóstico modelo1, 70%")
lines(valid.series.70, col = "red", lty = 2)
plot(fcast.2.70, main = "Validación pronóstico modelo2, 70%")
lines(valid.series.70, col = "red", lty = 2)
plot(fcast.auto.70, main = "Validación pronóstico modelo.auto, 70%")
lines(valid.series.70, col = "red", lty = 2)
accuracy(fcast.1.70)
accuracy(fcast.2.70)
accuracy(fcast.auto.70)


l.80.train <- .8*l
l.80.fcast <- l - l.80.train
logcoil.train.80.1 <- modelo1$fitted[1:l.80.train]
logcoil.train.80.2 <- modelo2$fitted[1:l.80.train]
logcoil.train.80.auto <- modelo.auto$fitted[1:l.80.train]
fcast.1.80 <- forecast(logcoil.train.80.1, h = l.80.fcast)
fcast.2.80 <- forecast(logcoil.train.80.2, h = l.80.fcast)
fcast.auto.80 <- forecast(logcoil.train.80.auto, h = l.80.fcast)
valid.series.80 <- window(ts(diff.COIL), start = l.80.train+1)
plot(fcast.1.80, main = "Validación pronóstico modelo1, 80%")
lines(valid.series.80, col = "red", lty = 2)
plot(fcast.2.80, main = "Validación pronóstico modelo2, 80%")
lines(valid.series.80, col = "red", lty = 2)
plot(fcast.auto.80, main = "Validación pronóstico modelo.auto, 80%")
lines(valid.series.80, col = "red", lty = 2)
accuracy(fcast.1.80)
accuracy(fcast.2.80)
accuracy(fcast.auto.80)

l.90.train <- .9*l
l.90.fcast <- l - l.90.train
logcoil.train.90.1 <- modelo1$fitted[1:l.90.train]
logcoil.train.90.2 <- modelo2$fitted[1:l.90.train]
logcoil.train.90.auto <- modelo.auto$fitted[1:l.90.train]
fcast.1.90 <- forecast(logcoil.train.90.1, h = l.90.fcast)
fcast.2.90 <- forecast(logcoil.train.90.2, h = l.90.fcast)
fcast.auto.90 <- forecast(logcoil.train.90.auto, h = l.90.fcast)
valid.series.90 <- window(ts(diff.COIL), start = l.90.train+1)
plot(fcast.1.90, main = "Validación pronóstico modelo1, 90%")
lines(valid.series.90, col = "red", lty = 2)
plot(fcast.2.90, main = "Validación pronóstico modelo2, 90%")
lines(valid.series.90, col = "red", lty = 2)
plot(fcast.auto.90, main = "Validación pronóstico modelo.auto, 90%")
lines(valid.series.90, col = "red", lty = 2)
accuracy(fcast.1.90)
accuracy(fcast.2.90) 
accuracy(fcast.auto.90)


# Comparacion mejor modelo ARIMA con metodo Holt
# Metodo de Holt: la serie no tiene parte estacional
# ARIMA: fcast.2.70, fcast.2.80, fcast.2.90

logcoil.train.70.holt <- window(ts(diff.COIL), end = l.70.train)
logcoil.holt70 <- holt(logcoil.train.70.holt, h = l.70.fcast)
autoplot(logcoil.holt70)
accuracy(fcast.2.70)
accuracy(logcoil.holt70) 

logcoil.train.80.holt <- window(ts(diff.COIL), end = l.80.train)
logcoil.holt80 <- holt(logcoil.train.80.holt, h = l.80.fcast)
autoplot(logcoil.holt80)
accuracy(fcast.2.80)
accuracy(logcoil.holt80) 

logcoil.train.90.holt <- window(ts(diff.COIL), end = l.90.train)
logcoil.holt90 <- holt(logcoil.train.90.holt, h = l.90.fcast)
autoplot(logcoil.holt90)
accuracy(fcast.2.90)
accuracy(logcoil.holt90) 

# Conclusión: el método de Holt proporciona una mejor validación de pronóstico


# Pronóstico 12 meses en adelante: diferencias entre los dos métodos

#Modelo Arima
Predicciones<-forecast(modelo.auto,h = 12)
Predicciones
autoplot(Predicciones)

#Metodo de Holt
Predicciones.holt <- holt(diff.COIL, h = 12)
Predicciones.holt
autoplot(Predicciones.holt)

