library(readr)
library(janitor)
library(here)
library(ggplot2)
library(magrittr)
require(forecast)
require(xts)
require(ggplot2)


# Cargar data frame

data <- read.csv2('./data/IngresosApple.csv')

ventas <- data$Ingresos

fecha <- seq(as.Date('2008/04/01'),as.Date('2017/07/01'), by = 'quarter')

# Create a XTS object

xventas <- xts(ventas, order.by = fecha)

# plot series

autoplot(xventas) + ylab("Ventas") + ggtitle("Ventas Trimestrales Apple") + 
  xlab("Trimestres")

df_new <- data.frame(value = as.vector(xventas),
                     time = time(xventas))
ggplot(df_new) + geom_point(aes( x = time,y = value)) + geom_line(aes(x = time,y = value)) + 
  ylab("Ventas") + ggtitle("Ventas Trimestrales Apple") + xlab("Trimestres")



####### Modelos ETS ########

omision_muestra <- 3

total_observaciones <- length(xventas)

#sub_muestra

muestra_ventas <- window(xventas,start = index(xventas[1]), 
                         end = index(xventas[total_observaciones - omision_muestra]))

# seleccion modelo ETS

modelo_ets <- ets(muestra_ventas)

# modelo para el forecast

forecast_modelo_ets <- forecast(modelo_ets)

# Results

summary(forecast_modelo_ets)

# comparación valores actuales y predicción

matrix(c(forecast_modelo_ets$mean[1:omision_muestra],
         xventas[(total_observaciones - omision_muestra + 1):total_observaciones]),
       ncol = 2)

# Predicciones y Precisión

tsventas <- ts(coredata(xventas), start = c(2008, 2), frequency = 4)
modelo_ets <- ets(window(tsventas, end = 2016 + 3/4))
ets_forecast_ventas <- forecast(modelo_ets,h = omision_muestra)

# accuracy(etsfit,window(tsVentas,start=2017))

forecast:::testaccuracy(ets_forecast_ventas$mean,window(tsventas, start = 2017),
                        test = NULL, d = NULL, D = NULL)


##### Modelos ARIMA #####


autoplot(xlventas)+ylab("Ventas")+ggtitle("Ventas Trimestrales CocaCola")+xlab("Trimestres")

xlventas <- log(xventas)

#Difference

ggtsdisplay(xlventas)

ggtsdisplay(diff(xlventas)) # tendencias en 4

ggtsdisplay(diff(xlventas,4)) # PACF corta en 1

ggtsdisplay(diff(diff(xlventas,4),1))

# Modelo ARIMA

modelo_arima <- auto.arima(muestra_ventas, lambda = 0)
summary(modelo_arima)

# ARIMA (0,1,1)

# Análisis de los residuos

ggtsdisplay(modelo_arima$residuals)

# Predicción ARIMA

forecast_modelo_arima <- forecast(modelo_arima)

ggplot(df_new) + geom_point(aes(x = time,y = value)) + geom_line(aes(x = time,y = value)) + 
  autolayer(forecast_modelo_arima,alpha = 0.4) + ggtitle("ARIMA: Predicción CocaCola")

# comparación valores actuales y predicción

matrix(c(forecast_modelo_arima$mean[1:omision_muestra],
         xventas[(total_observaciones - omision_muestra + 1):total_observaciones]),
       ncol = 2)





















