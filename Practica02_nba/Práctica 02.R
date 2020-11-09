library(here) # Comentar
library(tidyverse)
library(janitor) # Clean names
library(skimr) # Beautiful Summarize
library(magrittr) # Pipe operators
library(corrplot) # Correlations
library(ggcorrplot)  # Correlations
library(PerformanceAnalytics) # Correlations
library(leaps) # Model selection
library(rsample)  # data splitting 
library(glmnet)   # implementing regularized regression approaches
library(dplyr)  
library(ggplot2)


nba <-  read.csv("./data/nba.csv")
head(nba)
colnames(nba)


# Variables Names

nba %<>% clean_names()
colnames(raw_data)

# delete duplicate
# Remove duplicate rows of the dataframe
raw_data %<>% distinct(player,.keep_all = TRUE)

# delete NA's
raw_data %<>% drop_na()

# Summarise
skim(raw_data)


raw_data %>% 
  select_at(vars(-c("player","nba_country","tm"))) %>% 
  tidyr::gather("id", "value", 2:25) %>% 
  ggplot(., aes(y = log(salary), x = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~id,ncol = 2,scales = "free_x")


##### EDA ######

log_data <- raw_data %>% mutate(salary = log(salary))

skim(log_data)

# Excluded vars (factor)

vars <- c("player","nba_country","tm")

# Correlations
ggcorrplot(cor(log_data %>% 
                 select_at(vars(-vars)), 
               use = "complete.obs"),
           hc.order = TRUE,
           type = "lower",  lab = TRUE)

# Modelo lineal

nba_sin_categoricas <- log_data %>% dplyr::select(salary,age,nba_draft_number, 
                                                           g:vorp) 

regresion1 <- lm(salary~., data = nba_sin_categoricas)

summary(regresion1)

# calculo de los valores VIF

valores_vif <- car::vif(regresion1)
valores_vif

knitr::kable(vif_values)

# El VIF mide cuánto aumenta la varianza de un coeficiente de regresión estimado 
# si sus predictores están correlacionados. Más variación son malas noticias; 
# buscamos estimaciones precisas. Si la varianza de los coeficientes aumenta,
# nuestro modelo no será tan confiable.

# VIF
# Problemas de multicolinealidad. Genera cambios muy grandes en los estimadores. 
# Si estan sobre 4,5,6 habría que eliminarlos. 

# En este caso presentan problemas de colinealidad: mp, per, orb, drb, trb, ows,
# dws, ws, ws_48, obpm, dbpm, bpm, vorp


# Selección del modelo

nba_final <- nba_sin_categoricas

set.seed(1234)
filas_data <- nrow(nba_final)
num_data_test <- 20
train <-  sample(filas_data ,filas_data - num_data_test)


nba_train <- nba_sin_categoricas[train,] # data de entrenamiento
nba_test  <-  nba_sin_categoricas[-train,] 

seleccion_modelo <- regsubsets(salary~. , data = data_train, method = "seqrep", nvmax = 20)

resumen_seleccion_modelo <- summary(seleccion_modelo)

data.frame(
  Adj.R2 = (resumen_seleccion_modelo$adjr2),
  CP = (resumen_seleccion_modelo$cp),
  BIC = (resumen_seleccion_modelo$bic)
)

# BIC nos muestra los mejores modelos según el número de variables. 
# Cuando cambiamos el `set.seed` salen resultados diferentes para una misma muestra. 
# Esto hay que arreglarlo.

data.frame(
  Adj.R2 = which.max(resumen_seleccion_modelo$adjr2),
  CP = which.min(resumen_seleccion_modelo$cp),
  BIC = which.min(resumen_seleccion_modelo$bic)
)

# En este caso cogeremos el modelo correspondiente a un bajo BIC

coef(seleccion_modelo,which.min(resumen_seleccion_modelo$bic))

# (Intercept)              age nba_draft_number               mp              drb 
# 11.7152787355     0.0997178311    -0.0229478008     0.0007843202     0.0251805799 

# ----------------------------------------------
library(rsample)  # data splitting 
library(glmnet)   # implementing regularized regression approaches
library(dplyr)  
library(ggplot2)
# --------------------------------------------------

# Creamos matrices de los modelos de funciones y vectores de respuesta de entrenamiento y de prueba.
# usamos model.matrix (...) [, -1] para descartar la intersección.

nba_train_x <- model.matrix(salary ~ ., nba_train)[, -1]
nba_train_y <- nba_train$salary

nba_test_x <- model.matrix(salary ~ ., nba_test)[, -1]
nba_test_y <- nba_test$salary

# Para realizar un regresión cresta podemos usar la función glmnet :: glmnet. 
# El parámetro alpha le dice a glmnet que realice una regersión cresta (alpha = 0), 
# lasso (alpha = 1) o elastic net ($ 0  leq alpha  leq 1 $). glmnet está haciendo 
# dos cosas que debes tener en cuenta:  

# Mediante Ridge

nba_ridge <- glmnet(
  x = nba_train_x,
  y = nba_train_y,
  alpha = 0 # Ridge
)

plot(nba_ridge, xvar = "lambda")
  
nba_ridge$lambda %>% head()  
  
# Mediante Lasso

nba_lasso <- glmnet(
  x = nba_train_x,
  y = nba_train_y,
  alpha = 1
)

plot(nba_lasso, xvar = "lambda")
  

# Mediante Cross-Validation

# Apply CV Ridge regression to ames data
ames_lasso_cv <- cv.glmnet(
  x = ames_train_x,
  y = ames_train_y,
  alpha = 1
)
# plot results
plot(ames_lasso_cv)


# Elastic - Net

lasso_nba    <- glmnet(nba_train_x, nba_train_y, alpha = 1.0) 
elastic1_nba <- glmnet(nba_train_x, nba_train_y, alpha = 0.2) 
elastic2_nba <- glmnet(nba_train_x, nba_train_y, alpha = 0.8) 
ridge_nba    <- glmnet(nba_train_x, nba_train_y, alpha = 0.0)

par(mfrow = c(2, 2), mar = c(6, 4, 6, 2) + 0.1)
plot(lasso_nba, xvar = "lambda", main = "Lasso (Alpha = 1)\n\n\n")
plot(elastic1_nba, xvar = "lambda", main = "Elastic Net (Alpha = .25)\n\n\n")
plot(elastic2_nba, xvar = "lambda", main = "Elastic Net (Alpha = .75)\n\n\n")
plot(ridge_nba, xvar = "lambda", main = "Ridge (Alpha = 0)\n\n\n")



nba_id <- sample(1:10, size = length(nba_train_y), replace = TRUE)

nba_grid <- tibble::tibble(
  alpha      = seq(0, 1, by = .1),
  mse_min    = NA,
  mse_1se    = NA,
  lambda_min = NA,
  lambda_1se = NA
)
nba_grid


for(i in seq_along(nba_grid$alpha)) {
  
  
  fit <- cv.glmnet(nba_train_x, nba_train_y, alpha = nba_grid$alpha[i], foldid = nba_id)
  
  
  nba_grid$mse_min[i]    <- fit$cvm[fit$lambda == fit$lambda.min]
  nba_grid$mse_1se[i]    <- fit$cvm[fit$lambda == fit$lambda.1se]
  nba_grid$lambda_min[i] <- fit$lambda.min
  nba_grid$lambda_1se[i] <- fit$lambda.1se
}

nba_grid

nba_grid %>%
  mutate(se = mse_1se - mse_min) %>%
  ggplot(aes(alpha, mse_min)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymax = mse_min + se, ymin = mse_min - se), alpha = .25) +
  ggtitle("MSE ± one standard error")




# mejor modelo
cv_net   <- cv.glmnet(nba_train_x, nba_train_y, alpha = 0.9)
min(cv_net$cvm)

# predicción
pred <- predict(cv_net, s = cv_net$lambda.min, nba_test_x)
mean((nba_test_y - pred)^2)


#############

variables <- c('mp', 'per', 'orb', 'drb', 'trb', 'ows', 'dws', 'ws', 'ws_48', 'obpm', 'dbpm', 'bpm', 'vorp')

nba_final2 <- nba_sin_categoricas %>% select_at(vars(-variables))

set.seed(1234)
filas_data <- nrow(nba_final2) # número de filas 
num_data_test <- 10
train <-  sample(filas_data ,filas_data - num_data_test)


nba_train <- nba_sin_categoricas[train,] # data de entrenamiento
nba_test  <-  nba_sin_categoricas[-train,] # data para el test

nba_train_x <- model.matrix(salary ~ ., nba_train)[, -1]
nba_train_y <- nba_train$salary

nba_test_x <- model.matrix(salary ~ ., nba_test)[, -1]
nba_test_y <- nba_test$salary

lasso_nba    <- glmnet(nba_train_x, nba_train_y, alpha = 1.0) 
elastic1_nba <- glmnet(nba_train_x, nba_train_y, alpha = 0.2) 
elastic2_nba <- glmnet(nba_train_x, nba_train_y, alpha = 0.8) 
ridge_nba    <- glmnet(nba_train_x, nba_train_y, alpha = 0.0)

par(mfrow = c(2, 2), mar = c(6, 4, 6, 2) + 0.1)
plot(lasso_nba, xvar = "lambda", main = "Lasso (Alpha = 1)\n\n\n")
plot(elastic1_nba, xvar = "lambda", main = "Elastic Net (Alpha = .25)\n\n\n")
plot(elastic2_nba, xvar = "lambda", main = "Elastic Net (Alpha = .75)\n\n\n")
plot(ridge_nba, xvar = "lambda", main = "Ridge (Alpha = 0)\n\n\n")

nba_id <- sample(1:10, size = length(nba_train_y), replace = TRUE)

nba_grid <- tibble::tibble(
  alpha      = seq(0, 1, by = .1),
  mse_min    = NA,
  mse_1se    = NA,
  lambda_min = NA,
  lambda_1se = NA
)
nba_grid

for(i in seq_along(nba_grid$alpha)) {
  
  
  fit <- cv.glmnet(nba_train_x, nba_train_y, alpha = nba_grid$alpha[i], foldid = nba_id)
  
  
  nba_grid$mse_min[i]    <- fit$cvm[fit$lambda == fit$lambda.min]
  nba_grid$mse_1se[i]    <- fit$cvm[fit$lambda == fit$lambda.1se]
  nba_grid$lambda_min[i] <- fit$lambda.min
  nba_grid$lambda_1se[i] <- fit$lambda.1se
}

nba_grid







