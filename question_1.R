# question 1
library(haven)
library(tidyverse)
library(tseries)
library(stats)
library(forecast)
library(lmtest)

first_series <- read_dta("C:\\Users\\Lui Yu Sen\\Google Drive\\NTU_study materials\\Economics\\HE3021 Intermediate Econometrics\\Week 8\\HE3021-Week-8-Tutorial-6\\rawdata\\first_Series.dta")
first_series <- mutate(first_series, time = as.Date(first_series$time, origin = "2000-01-01"))
adftest <- adf.test(ts(first_series$y))
kpss.test(ts(first_series$y))
plot(first_series$time, first_series$y, type = "l")
pp <- pp.test(first_series$y) # this one. Still have to find out how adf works
# pp and adf tests reject non-stationarity, 0.01 p-value, too low to print
# also ACF decays exponentially
# adftest_pvalue <- c()
# for (n in 2:30){
#     adftest_lagn <- adf.test(ts(first_series$y), k = n)
#     adftest_pvalue <- c(adftest_pvalue, adftest_lagn$p.value)
#     rm(adftest_lagn)
# }
# rm(n)
pacf(first_series$y) # sharp spike, identify AR model
acf(first_series$y) # decaying exponentially
Arima(first_series$y, order = c(1,0,0), include.mean = TRUE)
AIC(Arima(first_series$y, order = c(1,0,0), include.mean = TRUE)) # ar(1)
model_ar2 <- auto.arima(first_series$y) # ar(2) has smallest AIC
residuals <- Arima(first_series$y, order = c(1,0,0), include.mean = TRUE)$residuals
residuals_frame <- data.frame(cbind(residuals[3:5000], residuals[2:4999], residuals[1:4998]))
colnames(residuals_frame) <- c("t","t1","t2")
lmtest::bgtest(t ~ t1 + t2, data = residuals_frame, order = 2)
residuals_lm <- lm(t ~ t1 + t2 - 1, data = residuals_frame) # p-value 0.3232, cannot reject null hyp that all coeff 0, so no serial correlation for 2 lags
# is white noise yay

