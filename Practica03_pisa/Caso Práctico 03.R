
library(readr)
library(tidyverse)
library(broom) # modelos en df
library(flextable) # Tablas formateadas
library(mgcv) # estimar gam
library(reshape2) # melt
library(splines)
library(skimr)
library(gamlss)
pisa <- read.csv('./data/pisasci2006.csv')
attach(pisa)

# El objetivo es modelizar la relación entre la puntuación media (OSS) y el resto 
# de variables, utilizando modelos de splines y GAM. Se debe realizar CV cuando se pueda.

# Examine el data frame de pisa
skim(pisa)


# lineal fit

modelo_lm <- lm(Overall ~ Interest + Support + Income + Health + Edu + HDI, data = pisa)
summary(modelo_lm)


width(flextable(tidy(modelo_lm)), width = 1.5)

width(flextable(glance(modelo_lm)), width = 1.5)

termplot(modelo_lm, partial.resid = TRUE, se = TRUE)


# Mapping de las varaibles explicativas 

# Interest

interestplot <- ggplot(data = pisa, mapping = aes(x = Interest, y = Overall)) +
  layer(geom = "point",stat = "identity",position = "identity") +
  theme_bw() + theme(legend.key = element_blank())
interestplot

attr(bs(pisa$Interest, df = 6), 'knots')

knotsint <- c(501, 522, 565)
pisa$X1int <- pmax(0, pisa$Interest - knotsint[1])
pisa$X2int <- pmax(0, pisa$Interest - knotsint[2])
pisa$X3int <- pmax(0, pisa$Interest - knotsint[3])

SupportLint <- range(pisa$Support, na.rm = TRUE)
interest.grid <- seq(from = SupportLint[1], to = SupportLint[2])
fitint <- lm(Overall~bs(Interest, knots=c(501, 522, 565)), data = pisa, na.action = na.omit)
predint <- predict(fitint, newdata = list(Interest = interest.grid), se = TRUE)


plot(Interest, Overall, col = 'gray')
lines(interest.grid, predint$fit, lwd = 2)
lines(interest.grid, predint$fit + 2*predint$se, lty = 'dashed')
lines(interest.grid, predint$fit - 2*predint$se, lty = 'dashed')

# Support

supportplot <- ggplot(data = pisa, mapping = aes(x = Support, y = Overall)) +
  layer(geom = "point",stat = "identity",position = "identity") +
  theme_bw() + theme(legend.key = element_blank())
supportplot

attr(bs(pisa$Support, df = 6), 'knots')

knotssup <- c(494, 512, 529)
pisa$X1sup <- pmax(0, pisa$Interest - knotssup[1])
pisa$X2sup <- pmax(0, pisa$Interest - knotssup[2])
pisa$X3sup <- pmax(0, pisa$Interest - knotssup[3])

SupportLims <- range(pisa$Support, na.rm = TRUE)
Support.grid <- seq(from = SupportLims[1], to = SupportLims[2])
fit <- lm(Overall~bs(Support, knots=c(494, 512, 529)), data = pisa)
pred <- predict(fit, newdata = list(Support = Support.grid), se = TRUE)


plot(Support, Overall, col = 'gray')
lines(Support.grid, pred$fit, lwd = 2)
lines(Support.grid, pred$fit + 2*pred$se, lty = 'dashed')
lines(Support.grid, pred$fit - 2*pred$se, lty = 'dashed')


# Income

incomeplot <- ggplot(data = pisa, mapping = aes(x = Income, y = Overall)) +
  layer(geom = "point",stat = "identity",position = "identity") +
  theme_bw() + theme(legend.key = element_blank())
incomeplot

attr(bs(pisa$Income, df = 6), 'knots')

knotsincome <- c(0.658, 0.765, 0.833)
pisa$X1inc <- pmax(0, pisa$Income - knotsincome[1])
pisa$X2inc <- pmax(0, pisa$Income - knotsincome[2])
pisa$X3inc <- pmax(0, pisa$Income - knotsincome[3])


IncomeLims <- range(Income, na.rm = TRUE)
income.grid <- seq(from = IncomeLims[1], to = IncomeLims[2])
fitincome <- lm(Overall~bs(Income, knots=c(0.658, 0.765, 0.833)), data = pisa)
predincome <- predict(fitincome, newdata = list(Income = income.grid), se = TRUE)

plot(Income, Overall, col = 'gray')
lines(income.grid, predincome$fit, lwd = 2)
lines(income.grid, predincome$fit + 2*predincome$se, lty = 'dashed')
lines(income.grid, predincome$fit - 2*predincome$se, lty = 'dashed')

# Health

healthplot <- ggplot(data = pisa, mapping = aes(x = Health, y = Overall)) +
  layer(geom = "point",stat = "identity",position = "identity") +
  theme_bw() + theme(legend.key = element_blank())
healthplot

attr(bs(pisa$Health, df = 6), 'knots')

knotshealth <- c(0.838, 0.893, 0.942)
pisa$X1health <- pmax(0, pisa$Health - knotshealth[1])
pisa$X2health <- pmax(0, pisa$Health - knotshealth[2])
pisa$X3health <- pmax(0, pisa$Health - knotshealth[3])



health.grid <- seq(from = healthLims[1], to = healthLims[2], 0.01)
fithealth <- lm(Overall ~ bs(Health, knots = c(0.838, 0.893, 0.942)), data = pisa)
predhealth <- predict(fithealth, newdata = list(Health = health.grid), se = TRUE)

plot(Health, Overall, col = 'gray')
lines(health.grid, predhealth$fit, lwd = 2)
lines(health.grid, predhealth$fit + 2*predhealth$se, lty = 'dashed')
lines(health.grid, predhealth$fit - 2*predhealth$se, lty = 'dashed')

# Edu

eduplot <- ggplot(data = pisa, mapping = aes(x = Edu, y = Overall)) +
  layer(geom = "point",stat = "identity",position = "identity") +
  theme_bw() + theme(legend.key = element_blank())
eduplot

attr(bs(pisa$Edu, df = 6), 'knots')

knotseduc <- c(0.718, 0.812, 0.878)
pisa$X1Edu <- pmax(0, pisa$Edu - knotseduc[1])
pisa$X2Edu <- pmax(0, pisa$Edu - knotseduc[2])
pisa$X3Edu <- pmax(0, pisa$Edu - knotseduc[3])

eduLims <- range(Edu, na.rm = TRUE)
edu.grid <- seq(from = eduLims[1], to = eduLims[2], 0.01)
fitedu <- lm(Overall ~ bs(Edu, knots = c(0.718, 0.812, 0.878)), data = pisa)
prededu <- predict(fitedu, newdata = list(Edu = edu.grid), se = TRUE)

plot(Edu, Overall, col = 'gray')
lines(edu.grid, prededu$fit, lwd = 2)
lines(edu.grid, prededu$fit + 2*prededu$se, lty = 'dashed')
lines(edu.grid, prededu$fit - 2*prededu$se, lty = 'dashed')

# HDI

hdiplot <- ggplot(data = pisa, mapping = aes(x = HDI, y = Overall)) +
  layer(geom = "point",stat = "identity",position = "identity") +
  theme_bw() + theme(legend.key = element_blank())
hdiplot

attr(bs(pisa$HDI, df = 6), 'knots')

knotsHDI <- c(0.7485, 0.8170, 0.8770)
pisa$X1HDI <- pmax(0, pisa$HDI - knotsHDI[1])
pisa$X2HDI <- pmax(0, pisa$HDI - knotsHDI[2])
pisa$X3HDI <- pmax(0, pisa$HDI - knotsHDI[3])


hdiLims <- range(HDI, na.rm = TRUE)
hdi.grid <- seq(from = hdiLims[1], to = hdiLims[2], 0.01)
fithdi <- lm(Overall ~ bs(HDI, knots = c(0.7485, 0.8170, 0.8770)), data = pisa)
predhdi <- predict(fithdi, newdata = list(HDI = hdi.grid), se = TRUE)

plot(HDI, Overall, col = 'gray')
lines(hdi.grid, predhdi$fit, lwd = 2)
lines(hdi.grid, predhdi$fit + 2*predhdi$se, lty = 'dashed')
lines(hdi.grid, predhdi$fit - 2*predhdi$se, lty = 'dashed')

# GAM

modelo_gam1 <- gam(Overall ~ s(Interest) + s(Support) + s(Income) + s(Health) + 
                    s(Edu) + HDI, data = pisa, na.action = na.exclude)

summary(modelo_gam1)

par(mfrow = c(2, 3))
plot(modelo_gam1, se = TRUE, col = 'blue')

gam.check(modelo_gam1)

# Elimino los splines a Health

modelo_gam2 <- gam(Overall ~ s(Interest) + s(Support) + s(Income) + Health + 
                     s(Edu) + HDI, data = pisa, na.action = na.exclude)

summary(modelo_gam2)

par(mfrow = c(2, 2))
plot(modelo_gam2, se = TRUE, col = 'blue')

gam.check(modelo_gam2)

# Fit with k basis functions
gam_int_k50 <- gam(Overall ~ s(Interest, k = 50), data = pisa)

gam_sup_k50 <- gam(Overall ~ s(Support, k = 50), data = pisa) # Fuera support. lineal

gam_inc_k20 <- gam(Overall ~ s(Income, k = 20), data = pisa)

gam_health_k50 <- gam(Overall ~ s(Health, k = 50), data = pisa) # Fuera health. lineal

gam_edu_k50 <- gam(Overall ~ s(Edu, k = 50), data = pisa)


# Visualize the GAMs

plot(gam_int_k50, residuals = TRUE, pch = 1)

plot(gam_sup_k50, residuals = TRUE, pch = 1)

plot(gam_inc_k20, residuals = TRUE, pch = 1)

plot(gam_health_k50, residuals = TRUE, pch = 1)

plot(gam_edu_k50, residuals = TRUE, pch = 1)

# resultados

plot(modelo_gam1, residuals = TRUE, pch = 1)




## Modelo CV
cv_modelo_1 <- gamlssCV(Overall ~ Interest + Support + Income + Health + 
                          Edu + HDI, data = na.omit(pisa), K.fold = 10, parallel = "multicore", ncpus = 4, set.seed = 1234)

CV(cv_modelo_1)
