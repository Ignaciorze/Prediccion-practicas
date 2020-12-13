
library(CausalImpact)
library(quantmod)

bmw <- getSymbols("BMW.DE" ,from = "2014-01-01",to = "2020-11-25", auto.assign = FALSE)
volkswagen <- getSymbols("VOW.DE" ,from = "2014-01-01",to = "2020-11-25", auto.assign = FALSE)

#Define workdata

xbmw = Ad(bmw)
xvolkswagen = Ad(volkswagen)


xbmw = na.exclude(xbmw)
xvow = na.exclude(xvolkswagen)
price_pair = merge(xbmw,xvow)


plot(price_pair$BMW.DE.Adjusted)


pre_period <- as.Date(c("2014-01-01", "2019-9-25"))
post_period <- as.Date(c("2019-9-26", "2020-11-25"))

marketing_causal <- CausalImpact(xbmw, 
                                 pre.period = pre_period, 
                                 post.period = post_period)



summary(marketing_causal)



plot(marketing_causal)

plot(marketing_causal, "original")

summary(marketing_causal, "report")
plot(price_pair$VOW.DE.Adjusted)

pre_period <- as.Date(c("2014-01-01", "2019-9-25"))
post_period <- as.Date(c("2019-9-26", "2020-11-25"))

marketing_causal <- CausalImpact(xvow, 
                                 pre.period = pre_period, 
                                 post.period = post_period)


summary(marketing_causal)


plot(marketing_causal)

plot(marketing_causal, "original")

summary(marketing_causal, "report")






















