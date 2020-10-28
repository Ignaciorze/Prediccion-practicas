library(readr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(car)
library(MASS)
library(dplyr)
library(readr)
nba <- read_csv("data/nba.csv")
View(nba)
# Analisis descriptivo ---------
summary(nba)
dim(nba)


# Modelo de regresion 1 Con todas las varaibles ---------- 

#Quitamos jugadores repetidos con valores NaN
nba_con_jugadores <- nba %>% distinct(Player, .keep_all = TRUE)

# Seleccionamos las variables relevantes para el modelo
nba_sin_categoricas <- nba_con_jugadores %>% dplyr::select(Salary,Age,NBA_DraftNumber, 
                                                    G:VORP) 

regresion1 <- lm(Salary~., data = nba_sin_categoricas)

summary(regresion1)

# Observamos que las varaibles relevantes en este modelo indicadas con un 
# p-vaor inferior al 5% son la edad, la posición en el draft, los partidos jugados 
#  y los minutos por partido. Podemos destacar de esta información que cuantos 
# más partidos juegue un jugador menos cobrará menos.

#  Estudio de la regresión. Normalidad.

qqPlot(regresion1, labels = row.names(nba), id.method = "identify",
       simulate = TRUE, main = "Q-Q Plot")

# [1] 114 326

# Estudio de la regresión. Análisis del error. ----
# Histograma + densidad + normal + rug
residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks = nbreaks, freq = FALSE,
       xlab = "Studentized Residual",
       main = "Distribution of Errors")
  rug(jitter(z), col = "brown")
  curve(dnorm(x, mean = mean(z), sd = sd(z)),
        add = TRUE, col = "blue", lwd = 2)
  lines(density(z)$x, density(z)$y,
        col = "red", lwd = 2, lty = 2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty = 1:2, col = c("blue","red"), cex = .7)}

residplot(regresion1)

# Vemos que los valores del error en la regresión se concentran en gran medida
# entre los valores -1 y 1 con valores extremos en +2 llegando hasta +4.

# Test de Shapiro-Wilk
vResid <- resid(regresion1)
shapiro.test(vResid)

# W = 0.9709, p-value = 3.509e-08. #  Rechazamos la hipotesis nula. 
# Los errores no han sido generados por una distribución normal. 

#  Multicolinealidad ----

# Es la existencia de alta correlación entre los predictores puede producir 
# problemas de imprecisión de los estimadores (las varianzas de los estimadores 
# son mayores de lo que deberían ser). Así, los intervalos de confianza son muy anchos, 
# hay dificultad para interpretar los coeficientes y se tiende a no rechazar las hipótesis 
# nula de significación.

vif(regresion1) 
sqrt(vif(regresion1)) > 2
# Observamos que la mayoría de las variables dan problemas de multicolinealidad
# Posiblemente a que muchas de las estadísticas tienen que ver con cada partido que se juega.

# Outliers ----

# valores atipicos

outlierTest(regresion1)
# rstudent unadjusted p-value Bonferroni p
# 326 4.586546         5.8334e-06    0.0028059
# 114 3.992498         7.6212e-05    0.0366580
# 2 valores atipicos

#  Valores Extremos

hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main = "Index Plot of Hat Values")
  abline(h = c(2,3)*p/n, col = "red", lty = 2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(regresion1)

# [1]  20 141 146 164 187 223 228

# Valores influyentes

# Cooks Distance D
# identify D values > 4/(n-k-1) 
cutoff <- 4/(nrow(nba_sin_categoricas) - length(regresion1$coefficients) - 2)
plot(regresion1, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

# 143, 166, 326

# Influence Plot
influencePlot(regresion1, id.method = "identify", main = "Influence Plot", 
              sub = "Circle size is proportial to Cook's Distance" )

# 114, 326, 166, 143

# Outliers totales : 20, 114, 141, 143, 146, 164, 166, 187, 223, 228, 326


# Modelo de predicción con todas las variables en una muestra n = 10 -----

set.seed(1234)
n <- 10
muestra <- sample(1:nrow(nba_sin_categoricas), size = n, replace = FALSE)
datos_de_la_muestra <- nba_sin_categoricas[muestra,]
datos_de_la_muestra

prediccion_de_la_muestra <- predict(regresion1, datos_de_la_muestra, se.fit = TRUE)
prediccion_de_la_muestra
# 1        2        3        4        5        6        7        8        9       10 
# 6679178 11160824  6817193  4619128  4143566  1078683 15613034 20081873 12042484  1045636 

# Modelo 2 de predicción mediante selección de variables -----

# Métodos de Selección: Both forward Stepwise and Backward Stepwise

stepAIC(regresion1, direction = "both")

regresion2 <- lm(formula = Salary ~ Age + NBA_DraftNumber + G + MP + PER + 
     `3PAr` + `ORB%` + `TRB%` + `USG%` + WS + OBPM, data = nba_sin_categoricas)

summary(regresion2)
nba_sin_categoricas2 <- nba_sin_categoricas %>% dplyr::select(Salary, Age, NBA_DraftNumber, G,  
                                                              MP, PER, `3PAr`,
                                                              `ORB%`, `TRB%`, 
                                                              `USG%`, WS, OBPM)
set.seed(1234)
n <- 10
muestra <- sample(1:nrow(nba_sin_categoricas2), size = n, replace = FALSE)
datos_de_la_muestra <- nba_sin_categoricas2[muestra,]
datos_de_la_muestra

prediccion_de_la_muestra2 <- predict(regresion2, datos_de_la_muestra, se.fit = TRUE)
prediccion_de_la_muestra2

#  1          2          3          4          5          6          7          8          9 
# 5812043.7 11205194.1  6446277.3  4114492.8  4110509.4  1253707.1 15201985.8 20748503.2 12199777.0 
# 10 
# 601566.5 

######## Modelo 3 de predicción mediante eliminacion de outliers ########
# con las varaibles del modelo 2

### Outliers 

# valores atipicos

outlierTest(regresion2)
# rstudent unadjusted p-value Bonferroni p
# 326 4.418019         1.2388e-05    0.0059588
# 114 4.097004         4.9325e-05    0.0237250
# 2 valores atipicos

#  Valores Extremos

hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main = "Index Plot of Hat Values")
  abline(h = c(2,3)*p/n, col = "red", lty = 2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(regresion2)

# [1]  20 141 146 223

# Valores influyentes

# Cooks Distance D
# identify D values > 4/(n-k-1) 
cutoff <- 4/(nrow(nba_sin_categoricas2) - length(regresion2$coefficients) - 2)
plot(regresion2, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

# 143, 227, 326

# Influence Plot
influencePlot(regresion1, id.method = "identify", main = "Influence Plot", 
              sub = "Circle size is proportial to Cook's Distance" )

# 114, 326, 166, 143

# Outliers totales : 20, 114, 141,143, 227, 146, 166, 223, 326

nba_sin_categoricas2 <- nba_sin_categoricas2[c(-20, -114, -141, -143, -227, -146, 
                                               -166, -223, -326),]


set.seed(1234)
n <- 10
muestra <- sample(1:nrow(nba_sin_categoricas2), size = n, replace = FALSE)
datos_de_la_muestra <- nba_sin_categoricas2[muestra,]
datos_de_la_muestra
prediccion_de_la_muestra2 <- predict(regresion2, datos_de_la_muestra, se.fit = TRUE)
prediccion_de_la_muestra2
# 1        2        3        4        5        6        7        8        9       10 
# 5017559  5498590  1536045  8316510 -1538961  2477695 11584319 -2785203  6075721  6446277 


###### Modelo 4 de prediccion. Con jugadores con mas de 2000 minutos jugados y mas de 50 partidos jugados ########

hist(nba$G)
hist(nba$MP)

nba_sin_categoricas <- nba_con_jugadores %>% filter(G > 50 | MP > 2000 ) %>% dplyr::select(Salary,Age,NBA_DraftNumber, 
                                                           G:VORP) 

regresion3 <- lm(Salary~., data = nba_sin_categoricas)

stepAIC(regresion3, direction = "both")

regresion3 <- lm(formula = Salary ~ Age + NBA_DraftNumber + G + MP + `TS%` + 
                   `3PAr` + FTr + `AST%` + `STL%` + `TOV%` + OWS + `WS/48` + 
                   OBPM + BPM + VORP, data = nba_sin_categoricas)

# outliers
outlierTest(regresion3)

# rstudent unadjusted p-value Bonferroni p
# 141 -3.315517          0.0010405      0.30695

#  Valores Extremos

hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main = "Index Plot of Hat Values")
  abline(h = c(2,3)*p/n, col = "red", lty = 2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(regresion3)

# [1]  55  78 116 174 209 215

# Valores influyentes

# Cooks Distance D
# identify D values > 4/(n-k-1) 
cutoff <- 4/(nrow(nba_sin_categoricas) - length(regresion3$coefficients) - 2)
plot(regresion3, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

# 42, 141, 193

# Influence Plot
influencePlot(regresion1, id.method = "identify", main = "Influence Plot", 
              sub = "Circle size is proportial to Cook's Distance" )

# 114, 326, 166, 143

# Outliers totales : 42 55 78 114 116 141 143 166 174 193 209 215 326

nba_sin_categoricas3 <- nba_sin_categoricas[c(-42,-55,-78,-114, -116, -141, -143,
                                              -166, -174,-193,-209,-215,-326),]

regresion3 <- lm(formula = Salary ~. , data = nba_sin_categoricas3)

stepAIC(regresion3, direction = "both")

regresion3 <- lm(formula = Salary ~ Age + NBA_DraftNumber + G + MP + `TS%` + 
                   `3PAr` + FTr + `AST%` + `STL%` + `TOV%` + OBPM + BPM + VORP, 
                 data = nba_sin_categoricas3)


summary(regresion3)


set.seed(1234)
n <- 10
muestra <- sample(1:nrow(nba_sin_categoricas3), size = n, replace = FALSE)
datos_de_la_muestra <- nba_sin_categoricas3[muestra,]
datos_de_la_muestra
prediccion_de_la_muestra3 <- predict(regresion3, datos_de_la_muestra, se.fit = TRUE)
prediccion_de_la_muestra3



