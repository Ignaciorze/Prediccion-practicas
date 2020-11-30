library(quantmod) 
library(forecast)
library(fGarch)
library(vars)
library(ggplot2)



## Leer datos

#get data from yahoo
bmw <- getSymbols("BMW.DE" ,from = "2014-01-01",to = "2020-11-25", auto.assign = FALSE)
volkswagen <- getSymbols("VOW3.DE" ,from = "2014-01-01",to = "2020-11-25", auto.assign = FALSE)

#Define workdata

xbmw = Ad(bmw)
xvolkswagen = Ad(volkswagen)

### Generar rentabilidad diaria

#Calculate Daily Arithmetic Return

rbmw = dailyReturn(xbmw, type = 'log',leading = FALSE)
rvolkswagen = dailyReturn(xvolkswagen, type = 'log',leading = FALSE)


# Exclude NA (First data)

rbmw = na.exclude(rbmw)
rvolkswagen = na.exclude(rvolkswagen)

## Exploración de los datos

# Se ha recogido los rendimientos diarios de las dos cotizaciones y se han
# excluido todos los valores pérdidos que no se han podido recoger en el dataset.
# En primer lugar vamos a observar la evolución y el comportamiento de las cotizaciones de ambas series
# y vamos a estudiar sus rendimientos y sus posibles variaciones.

### Gráficas de BMW

autoplot(xbmw, facets = FALSE) +
  ggtitle("Cotización de BMW") +
  xlab("Tiempo") +
  ylab("Cuota de Mercado")

plot.zoo(cbind(xbmw, rbmw),main = paste("BMW y Rentabilidad"),xlab = "años",ylab = c("Precio","rentabilidad"))
grid(lwd = 2)

# Pico alto en 2015 y 2016, caida en 2020 (coronavirus)

### Gráficas de Volkswagen

autoplot(xvolkswagen, facets = FALSE) +
  ggtitle("Cotización de Volkswagen") +
  xlab("Tiempo") +
  ylab("Cuota de Mercado")


plot.zoo(cbind(xvolkswagen, rvolkswagen),main = paste("Volkswagen y Rentabilidad"),xlab = "años",ylab = c("Precio","rentabilidad"))
grid(lwd = 2)

# Investigar años 2015 y 2016 para volkswagen, caida significativa.



## Estimar un modelo GARCH para BMW

# ACF & PACF 
# VolProxy=abs(dRentCont) # absolute value
VolProxy = rbmw ^ 2 #squared

# La volatilidad es la rentabilidad al cuadrado. 
# Es una variable no observable mientras que la rentabilidad si lo es. 

tsdisplay(VolProxy) 

# Podemos ver que la volatilidad no es ruido blanco ya que se encuentran fuera de 
# las bandas por lo que realizar un modelo GARCH sería lo más apropiado en esta 
# situación. 

# Test de Lagrange

#LM test
archTest <- function(rtn,m=10){
  # Perform Lagrange Multiplier Test for ARCH effect of a time series
  # rtn: time series
  # m: selected AR order
  # TSAY(2013)
  y = (rtn - mean(rtn))^2
  T = length(rtn)
  atsq = y[(m + 1):T]
  x = matrix(0,(T - m),m)
  for (i in 1:m) {
    x[,i] = y[(m + 1 - i):(T - i)]
  }
  md = lm(atsq~x)
  summary(md)
}

archTest(rbmw,20)

# El p-valor es inferior al nivel de significación por lo que existe la posibilidad
# de realizar un modelo GARCH.



# En primer lugar realizaremos un modelo GARCH 1

#ARCH(1)
m1 = garchFit(~1+garch(1,0),data = rbmw,trace = F) # Fit an ARCH(1) model
summary(m1)

# Modelo ARCH 1 estima solo hasta el alfa 1. omega es w. Luego hace los tests. 
# Los dos primeros son los de normalidad (Jaque-Bera y Shapiro-Wilk). Podemos ver
# que no siguen una distribución normal.
# El resto nos indican que son ruido blanco. Por lo tanto el modelo GARCH que he estimado no es correcto
# porque los errores al cuadrado son ruido blanco y seguimos teniendo errores GARCH

resi = residuals(m1,standardize = T) #residuals
resi = xts(resi,order.by = index(rbmw)) #residuals as xts
tsdisplay(resi^2)

# Los errores del modelo siguen teniendo erroes GARCH y proponemos un modelo GARCH (1,1)


m2 = garchFit(~1+garch(1,1),data = rbmw,trace = F) # Fit an GARCH(1,1) model
# nos muestra que no hay errores GARCH

resi = residuals(m2,standardize = T) #residuals
resi = xts(resi,order.by = index(dRentCont)) #residuals as xts
tsdisplay(resi^2) #acf pacf residuals
plot(m2)

#  el 2 es la volatidad estimada para BMW. Vemos que al final del periodo la volatilidad
# de los rendimientos es más alto, esto puede concordar conel periodo de Coronavirus.
# El qq plot nos muestra que los errores en las colas no son normales. Esto es tipico en
# las series de rendimientos y lo vamos a corregir suponiendo un modelo GARCH 
# donde los errores son una t-student (media 0 y dt = 1)

#t-student
m3 = garchFit(~1+garch(1,1),data = rbmw,trace = F,cond.dist = "std")
summary(m3)

# Los test de normalidad no tiene sentido mirarlos aqui porque estamos usando la t-student

plot(m3)

#  El qq plot tiene muchos menos puntos que antes. Tiene algun atipico. Los problemas
#  siempre vienen en las caidas negativas en finanzas.Lo de la t student es habitual en finanzas

v1 = volatility(m3)  # Volatilidad anual
v1 = xts(v1,order.by = index(rbmw)) #  volatility as XTS
plot(sqrt(252)*v1) # Volatilidad anual 

resi = residuals(m3,standardize = T) # Standardized residuals
resi = xts(resi,order.by = index(rbmw)) # Standardized residuals as XTS
tsdisplay(resi^2) # comprobamos que son ruido blanco los errores

# Sigue habiendo errores que no son ruido blanco, esto se puede deber a que a la hora
# de realizar las predicciones y la reciente situación de coronavirus que ha supuesto
# un cambio significante en la evolucion de los rendimientos de BMW pueda afectar a nuestro modelo
# de predicción.


plot(resi)

head(predict(m3)) #forecast volatility

## Prediccion modelos GARCH

# predice la media y la volatilidad (std. deviation)
predict(m3,n.ahead = 20,plot = TRUE,conf = .9,nx = 100) 

# plot 100 data with 90% confidence
# conf es el nivel de confianza. nx es la muestra
# Si el activo esta por encima de la linea azul, vendemos porque teoricamente va a bajar. 
#  Para trading esta decision es fundamental. 




## Plantear un modelo VAR mensual entre los dos activos

# En primer lugar se obtiene los rendimientos mensuales de ambas series temporales

rbmw_monthly = monthlyReturn(xbmw, type = 'log',leading = FALSE)
rvolkswagen_monthly = monthlyReturn(xvolkswagen, type = 'log',leading = FALSE)

# generar vector
bmw_vw = cbind(rbmw_monthly,rvolkswagen_monthly) # vector con las dos series temporales
# Mis rendimientos son estacionarios

colnames(bmw_vw) = c("BMW","Volkswagen")
bmw_vw = na.omit(bmw_vw)

#Seleccionar modelo

VARselect(bmw_vw) # ver que modelo selecciona
# con el AIC selecciona el 1 (retardos) por lo que p = 1 y  k = 2 porque tenemos 2 variables 

# estimamos
model.var = VAR(bmw_vw)
summary(model.var)

# la l es de lag. Desde el punto de vista estadistico es un modelo bueno a pesar 
# de su bajo R^2 cuadrado. Pero vamos a ver desde el punto de vista predictivo cuan eficaz es. 
# Realizamos un modelo VAR sin las constantes

model.var1 = VAR(bmw_vw,type = "none") # le quito la constante
summary(model.var1)

# El modelo mejora un poco con respecto al anterior en cuanto a su R^2.

# causalidad de granger

causality(model.var1) 

# vemos que le ocurre al modelo. Nos sirve para ver si hay efecto bidireccional.
# La hipotesis nula es que BMW no causa a Volkswagen. La aceptamos. El modelo nos vale
# La relacion instantanea nos dice que si que hay relacion instantanea. Son relaciones dinámicas por
# lo que los modelos VAR si que nos sirven en este caso.

# respuesta al impulso. Una subida de telefonica como afectaria a iBEX. Solo podemos ver los efectos que tiene una variable sobre otra.
#  No tiene mucho sentido hacerlo

model.ri = irf(model.var1)
model.ri
plot(model.ri) 

# Hay efecto contemporaneo (cuando sube telefonica tambien IBEX, porque IBEX contiene a Telefonica).
# Lo ideal seria que la linea estuviese mas por encima. Cuando pasa por la linea roja es que no tiene efecto. El efecto medio es la linea negra
# El primer grafico es de telefonica con telefonica y el segundo es telefonica con el IBEX

## prediccion modelo VAR

predict(model.var1, n.ahead = 8, ci = 0.95) 

# Hace una prediccion negativa positiva de telefonica mientras que en el IBEX cada vez es mas pequeño.


















