rm(list = ls())
library(tseries)
library(fpp2)
library(tseries)
library(quantmod)
library(ggplot2)
library(forecast)
library(seasonal)

#Leemos los datos
datos <- read.csv("datos/cafe_arabica.csv")
View(datos)

#Serie de tiempo
coffee.ts <- ts(datos, start = c(1960,01), end = c(2021,07), frequency = 12)
#Graficamos
plot.ts(coffee.ts)

#Descomponemos la serie
coffee.dec <- decompose(coffee.ts)
autoplot(coffee.dec)

# Pruebas de estacionariedad
adf.test(coffee.ts) # H0: la serie no es estacionaria
# Rechazamos H0: la serie es estacionaria

pp.test(coffee.ts) # H0: la serie no es estacionaria
# Rechazamos H0: la serie es estacionaria

kpss.test(coffee.ts) # H0: la serie es estacionaria
# Rechazamos H0: la serie no es estacionaria

# En la ultima prueba La serie no es estacionaria podemos hacer una diferencia 
# pero verificamos con un ACF


# IdentificaciÃ³n del proceso
acf(coffee.ts) # Se observa un descenso lento del proceso comunes en series no estacionarias
# Se detecta patrÃ³n AR => usar PACF para determinar el posible orden
pacf(coffee.ts)
# p = 2

# Realizamos una Diferencia

diff.coffee <- diff(coffee.ts)
autoplot(diff.coffee)

# Pruebas de estacionariedad
adf.test(diff.coffee) # H0: la serie no es estacionaria
# Rechazamos H0: la serie es estacionaria

pp.test(diff.coffee) # H0: la serie no es estacionaria
# Rechazamos H0: la serie es estacionaria

kpss.test(diff.coffee) # H0: la serie es estacionaria
# Rechazamos H0: la serie es estacionaria


# IdentificaciÃ³n del proceso
acf(diff.coffee)
# Se detecta patrÃ³n AR => usar PACF para determinar el posible orden
pacf(diff.coffee)
# p = 1


### Realizamos los modelos

m1 <- Arima(coffee.ts, order = c(1,1,0), include.constant = T)
m1 # Todos los coeficientes son significativos


m.auto <- auto.arima(coffee.ts, seasonal = F,
                     max.p = 12, max.q = 12, allowdrift = T)
m.auto # todos son significativos

m2 <- Arima(coffee.ts, order = c(1,1,3), include.constant = T)
m2 # los coeficientes no son significativo;

# mejor AICc, BIC para el modelo m.auto

checkresiduals(m1) # p-value = 0.0008047
checkresiduals(m.auto) # p-value = 0.01279; mejor que el p-value de los otros modelos
checkresiduals(m2) # p-value = 0.001055; mejor que el p-value del modelo "m1"

### Realizamos las predicciones por longitud

#PAra el 70%
l <- length(coffee.ts)
l.70.train <- .7*l
l.70.fcast <- l - l.70.train
coffee.train.70.1 <- m1$fitted[1:l.70.train]
coffee.train.70.2 <- m2$fitted[1:l.70.train]
coffee.train.70.auto <- m.auto$fitted[1:l.70.train]
#Predicciones
fcast.1.70 <- forecast(coffee.train.70.1, h = l.70.fcast)
fcast.2.70 <- forecast(coffee.train.70.2, h = l.70.fcast)
fcast.auto.70 <- forecast(coffee.train.70.auto, h = l.70.fcast)
valid.series.70 <- window(ts(coffee.ts), start = l.70.train+1)
plot(fcast.1.70, main = "Validación pronóstico m1, 70%")
lines(valid.series.70, col = "red", lty = 2)
plot(fcast.2.70, main = "Validación pronóstico m2, 70%")
lines(valid.series.70, col = "red", lty = 2)
plot(fcast.auto.70, main = "Validación pronóstico m.auto, 70%")
lines(valid.series.70, col = "red", lty = 2)

#Precision del modelo
accuracy(fcast.1.70) 
accuracy(fcast.2.70) 
accuracy(fcast.auto.70)#Mejor



#PAra el 80%
#longitudes
l.80.train <- .8*l
l.80.fcast <- l - l.80.train
coffee.train.80.1 <- m1$fitted[1:l.80.train]
coffee.train.80.2 <- m2$fitted[1:l.80.train]
coffee.train.80.auto <- m.auto$fitted[1:l.80.train]
#Predicciones
fcast.1.80 <- forecast(coffee.train.80.1, h = l.80.fcast)
fcast.2.80 <- forecast(coffee.train.80.2, h = l.80.fcast)
fcast.auto.80 <- forecast(coffee.train.80.auto, h = l.80.fcast)
valid.series.80 <- window(ts(coffee.ts), start = l.80.train+1)
#Graficos
plot(fcast.1.80, main = "Validación pronóstico m1, 80%")
lines(valid.series.80, col = "red", lty = 2)
plot(fcast.2.80, main = "Validación pronóstico m2, 80%")
lines(valid.series.80, col = "red", lty = 2)
plot(fcast.auto.80, main = "Validación pronóstico m.auto, 80%")
lines(valid.series.80, col = "red", lty = 2)
#Presision
accuracy(fcast.1.80)
accuracy(fcast.2.80) 
accuracy(fcast.auto.80) # mejor


#PAra el 90%
#longitudes
l.90.train <- .9*l
l.90.fcast <- l - l.90.train
coffee.train.90.1 <- m1$fitted[1:l.90.train]
coffee.train.90.2 <- m2$fitted[1:l.90.train]
coffee.train.90.auto <- m.auto$fitted[1:l.90.train]
#Predcciones
fcast.1.90 <- forecast(coffee.train.90.1, h = l.90.fcast)
fcast.2.90 <- forecast(coffee.train.90.2, h = l.90.fcast)
fcast.auto.90 <- forecast(coffee.train.90.auto, h = l.90.fcast)
valid.series.90 <- window(ts(coffee.ts), start = l.90.train+1)
#Graficos
plot(fcast.1.90, main = "Validación pronóstico m1, 90%")
lines(valid.series.90, col = "red", lty = 2)
plot(fcast.2.90, main = "Validación pronóstico m2, 90%")
lines(valid.series.90, col = "red", lty = 2)
plot(fcast.auto.90, main = "Validación pronóstico m.auto, 90%")
lines(valid.series.90, col = "red", lty = 2)
#Precision
accuracy(fcast.1.90) # mejor
accuracy(fcast.2.90) 
accuracy(fcast.auto.90)

# SelecciÃ³n mejor modelo
# 1. Criterios de informaciÃ³n: modelo "m.auto"
# 2. DiagnÃ³stico de ruido blanco: modelo "m.auto"
# ValidaciÃ³n del pronÃ³stico: modelo "m.auto"
# El modelo "m.auto" es el mejor

# Comparación mejor modelo ARIMA con mÃ©todo Holt
# Método de Holt: la serie no tiene parte estacional
# ARIMA: fcast.2.70, fcast.2.80, fcast.2.90

coffee.train.70.holt <- window(ts(coffee.ts), end = l.70.train)
coffee.holt70 <- holt(coffee.train.70.holt, h = l.70.fcast)
accuracy(fcast.2.70)# mejor
accuracy(coffee.holt70) 

coffee.train.80.holt <- window(ts(coffee.ts), end = l.80.train)
coffee.holt80 <- holt(coffee.train.80.holt, h = l.80.fcast)
accuracy(fcast.2.80)# mejor
accuracy(coffee.holt80) 

coffee.train.90.holt <- window(ts(coffee.ts), end = l.90.train)
coffee.holt90 <- holt(coffee.train.90.holt, h = l.90.fcast)
accuracy(fcast.2.90)# mejor
accuracy(coffee.holt90) 

# ConclusiÃ³n: en los tres casos el modelo arima proporciona una mejor validación de pronóstico
# (tercer dígito)

# Pronóstico 12 meses en adelante: diferencias entre los dos mÃ©todos
coffee.holt <- holt(coffee.ts, h = 12)
coffee.holt
#          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#Aug 2021       4.597311 4.301216 4.893406 4.144472 5.050149
#Sep 2021       4.704638 4.259803 5.149473 4.024322 5.384954
#Oct 2021       4.811965 4.234718 5.389212 3.929142 5.694788
#Nov 2021       4.919292 4.214837 5.623748 3.841920 5.996664
#Dec 2021       5.026619 4.196205 5.857034 3.756610 6.296629
#Jan 2022       5.133946 4.176980 6.090913 3.670392 6.597501
#Feb 2022       5.241273 4.156181 6.326366 3.581767 6.900780
#Mar 2022       5.348601 4.133245 6.563957 3.489874 7.207327
#Apr 2022       5.455928 4.107834 6.804021 3.394197 7.517659
#May 2022       5.563255 4.079745 7.046765 3.294422 7.832087
#Jun 2022       5.670582 4.048853 7.292311 3.190361 8.150803
#Jul 2022       5.777909 4.015085 7.540733 3.081902 8.473916

autoplot(coffee.holt)

# Pronóstico con el mejor modelo ARIMA
forecast(m.auto,h = 12)
#        Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#Aug 2021       4.535396 4.260997 4.809795 4.115739 4.955053
#Sep 2021       4.529578 4.093769 4.965388 3.863065 5.196091
#Oct 2021       4.500249 3.927002 5.073496 3.623543 5.376954
#Nov 2021       4.459923 3.774317 5.145530 3.411378 5.508469
#Dec 2021       4.421619 3.646012 5.197226 3.235431 5.607807
#Jan 2022       4.385234 3.534368 5.236100 3.083947 5.686521
#Feb 2022       4.350672 3.435230 5.266114 2.950624 5.750720
#Mar 2022       4.317842 3.346017 5.289667 2.831564 5.804120
#Apr 2022       4.286658 3.264978 5.308337 2.724134 5.849181
#May 2022       4.257036 3.190844 5.323227 2.626436 5.887635
#Jun 2022       4.228898 3.122655 5.335141 2.537045 5.920751
#Jul 2022       4.202170 3.059658 5.344683 2.454848 5.949493

autoplot(forecast(m.auto,h = 12))
