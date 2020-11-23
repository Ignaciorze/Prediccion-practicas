

library(openxlsx)
library(TSA)
library(ggplot2)
library(forecast)
library(zoo)
library(here)
library(tsoutliers)
library(lmtest)

data <- read.xlsx(("./data/data.xlsx"), colNames = TRUE, detectDates = TRUE)
head(data)

sum(is.na(data)) 

crest <- data$Crest
colgate <- data$Colgate
generateDate <- seq(as.Date('1958/01/08'), as.Date('1963/04/23'), by = 'week')
xcrest <- xts(crest, order.by = generateDate)
xcolgate <- xts(colgate, order.by = generateDate)

#Vamos a pasarlo a trimestre para operar mejor

xcrest <- to.weekly(xcrest)
zcrest <- as.zoo(xcrest$xcrest.Close)
xcolgate <- to.weekly(xcolgate)
zcolgate <- as.zoo(xcolgate$xcolgate.Close)


data_comparacion_temporal <- ts(data[,c(3,4)], start = 1958, frequency = 52)

autoplot(data_comparacion_temporal, facets = FALSE) +
  ggtitle("Colgate vs Crest") +
  xlab("Tiempo") +
  ylab("Cuota de Mercado")


autoplot(zcolgate) + geom_point() +
  ylab("Ventas") + ggtitle("Cuota Colgate") + xlab("Semanas") + 
  ggtitle("Cuota Colgate")



## Modelo Arima


### Modelo ARIMA para Colgate

omision_muestra <- 16

total_observaciones <- length(zcolgate)

#sub_muestra

ocolgate <- window(zcolgate,start = index(zcolgate[1]), 
                   end = index(zcolgate[total_observaciones - omision_muestra]))

ocrest <- window(zcrest,start = index(zcrest[1]), 
                 end = index(zcrest[total_observaciones - omision_muestra]))


modelo_arima_colgate = auto.arima(ocolgate)
summary(modelo_arima_colgate)

ggtsdisplay(modelo_arima_colgate$residuals)


#box-Ljung Test
Box.test(modelo_arima_colgate$residuals,lag = 3, fitdf = 3, type = "Lj")
Box.test(modelo_arima_colgate$residuals,lag = 8, fitdf = 3, type = "Lj")
Box.test(modelo_arima_colgate$residuals,lag = 12, fitdf = 3, type = "Lj")
fventas.arima = forecast(modelo_arima_colgate)


fcolgate <- forecast(modelo_arima_colgate, h = 16)
plot(fcolgate)
head(summary(fcolgate))


### Modelo Arima: Crest

modelo_arima_crest = auto.arima(ocrest)
summary(modelo_arima_crest)


ggtsdisplay(modelo_arima_crest$residuals)


#box-Ljung Test
Box.test(modelo_arima_crest$residuals,lag = 3, fitdf = 3, type = "Lj")
Box.test(modelo_arima_crest$residuals,lag = 8, fitdf = 3, type = "Lj")
Box.test(modelo_arima_crest$residuals,lag = 12, fitdf = 3, type = "Lj")
fventas.arima = forecast(modelo_arima_crest)


fcrest = forecast(modelo_arima_crest, h = 16)
plot(fcrest)


## Outliers detectados


detectAO(modelo_arima_colgate) 
detectAO(modelo_arima_crest) 
checkresiduals(modelo_arima_colgate)


detectIO(modelo_arima_colgate) 
detectIO(modelo_arima_crest) 
checkresiduals(modelo_arima_crest)

## Modelo de Intervención

arimax_colgate <- arimax(ocolgate, order = c(0, 1, 1),
                         xtransf = data.frame(first = 1*(seq(ocolgate) >= 135)
                         ),
                         transfer = list(c(0,0)),
                         method = 'ML') 


arimax_crest <- arimax(ocrest, order = c(0, 1, 1), 
                       xtransf = data.frame(primero = 1*(seq(ocrest) >= 135)),
                       xreg = data.frame(error136 = 1*(seq(ocrest) == 136),
                                         error138 = 1*(seq(ocrest) == 138)),
                       transfer = list(c(0,0)),
                       method = 'ML') 


coeftest(arimax_colgate)
coeftest(arimax_crest) 



## Función de Transferencia

crest_134 <- window(crest, end = 134) 
colgate_134 <- window(colgate, end = 134) 
crest_134_diff <- diff(crest_134) 
colgate_134_diff <- diff(colgate_134) 


modelo_transferencia <- arimax(colgate_134_diff,
                               order = c(0,1,1),
                               include.mean = TRUE,
                               xtransf = crest_134_diff,
                               transfer = list(c(0,15)), 
                               method = "ML")
coeftest(modelo_transferencia)

plot(modelo_transferencia$coef[2:15], type = 'h')


modelo_transferencia_final <- arimax(colgate_134_diff, 
                                     include.mean = TRUE, 
                                     fixed = c(NA,NA,0,0,NA),
                                     xtransf = crest_134_diff,
                                     transfer = list(c(1,2)), 
                                     method = "ML")

summary(modelo_transferencia_final)













