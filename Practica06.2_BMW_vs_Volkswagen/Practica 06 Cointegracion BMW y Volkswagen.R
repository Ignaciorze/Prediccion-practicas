
library(quantmod)
require(devtools)
library(PairTrading)
library(urca)


## Carga de datos


bmw <- getSymbols("BMW.DE" ,from = "2014-01-01",to = "2020-11-25", auto.assign = FALSE)
volkswagen <- getSymbols("VOW.DE" ,from = "2014-01-01",to = "2020-11-25", auto.assign = FALSE)

#Define workdata

xbmw = Ad(bmw)
xvolkswagen = Ad(volkswagen)


xbmw = na.exclude(xbmw)
xvow = na.exclude(xvolkswagen)
price_pair = merge(xbmw,xvow)

adf.test(price_pair[,1],k = 0) # test dicky fuller. K es 0. Hipotesis nula es no estacionaria. Es no setacionaria y hay que hacer una diferencia
adf.test(price_pair[,2],k = 0)

adf.test(price_pair[,1],k = 6) # autoregresivo de orden 6 para saber si los errores no son estacionarias
adf.test


test_1 <- ur.df(price_pair[,1],type = "none",selectlags = "AIC",lags = 10) # selecciona la k con AIC con maximo de 10 retardos
summary(test_1) # 0.608 es no estacionaria. necesita una diferencia


test_2 <- ur.df(price_pair[,1],type = "trend",selectlags = "AIC",lags = 10) # selecciona con tendencia
summary(test_2) # Value of test-statistic is: -2.4009 2.8114 3.5747. 
# Solo hay que fijase en el tau3 para la estacionaridad

#Estimate parameters & plot spread
reg <- EstimateParameters(price_pair, method = lm) # Estima los parametros de la relacion de equilibrio entre los dos pares
str(reg) # cobertura es de 0.09
plot(reg$spread) # relacion de equilibrio. Cuanto mas abajo, mas separados, cuando suben vuelve. Si sigue hacia abajo podemos tener perdidas.
# Spread es el error del modelo de equilibrio en la relacion a largo plazo

#check stationarity
IsStationary(reg$spread, 0.1) # dicky fuller dice que si que es estacionario, pero el otro no. Hay que tener cuidado

#estimate parameters for back test
params <- EstimateParametersHistorically(price_pair, period = 180) # coge 180 datos

#create & plot trading signals
signal <- Simple(params$spread, 0.05) # creo la señal. Cunaod debo de entrar y salir en funcion del spread
plot(params$spread)
par(new = TRUE)
barplot(signal,col = "blue",space = 0, border = "blue",xaxt = "n",yaxt = "n",xlab = "",ylab = "")
# La parte de arriba significa que estoy dentro en la estrategia y abajo fuera en la estrategia
# Siempre estamos dentro del mercado. Arriba compramos uno y vendiendo otro. 


#Performance of pair trading
return.pairtrading <- Return(price_pair, lag(signal), lag(params$hedge.ratio))
plot(100 * cumprod(1 + return.pairtrading)) # beneficio de la estrategia durante todo este tiempo




















