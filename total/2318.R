setwd('E://bdt//5006//proj/')
par(mfrow=c(2,2))
library(tseries)

# =================================================
vm = read.csv('./2318.HK.csv')[,6]
length(vm)
plot(vm,main="",ylab="price",xlab="",type="l", col='red')

adf.test(vm)
# Dickey-Fuller = -0.98727, Lag order = 13, p-value = 0.9408
# non stationary

log_rtn = diff(log(vm))
plot(log_rtn,main="",ylab="log_rtn",xlab="",type="l", col='red')

adf.test(log_rtn)
# Dickey-Fuller = -13.744, Lag order = 13, p-value = 0.01
# stationaty

Box.test(log_rtn,12,type = 'Ljung')
# X-squared = 31.819, df = 12, p-value = 0.001476
# reject white noise, there is correlation in log_rtn

# check acf and pacf
a = acf(log_rtn,12,main="")
a

pa = pacf(log_rtn,12,main="")
pa

# ==========
# ar11 model
# ==========
m1 = arima(log_rtn,order = c(11,0,0))
m1
# sigma^2 estimated as 0.0003879:  log likelihood = 6183.28,  aic = -12340.56

# fix parameters
m1 = arima(log_rtn,order = c(11,0,0),fixed = c(0,0,0,NA,NA,0,0,0,0,0,NA,0))
m1
# sigma^2 estimated as 0.00039:  log likelihood = 6176.77,  aic = -12345.54

# model checking
Box.test(m1$residuals,12,type = "Ljung")
# X-squared = 14.343, df = 12, p-value = 0.2793

pv=1-pchisq(14.343,9) #Compute p-value using 9 degrees of freedom
pv
# pv = 0.11 > 0.05, we think it's correct


# ==========
# ar5 model
# ==========
m2 = arima(log_rtn,order = c(5,0,0))
m2
# sigma^2 estimated as 0.0003898:  log likelihood = 6177.24,  aic = -12340.49

# fix parameter
m2 = arima(log_rtn,order = c(5,0,0),fixed = c(NA,NA,0,NA,NA,0))
m2
# sigma^2 estimated as 0.00039:  log likelihood = 6176.6,  aic = -12343.19


# model checking
Box.test(m2$residuals,12,type = "Ljung")
# X-squared = 13.867, df = 12, p-value = 0.3093

pv = 1-pchisq(13.867,8)
pv
# pv = 0.08 > 0.05, we think it's correct

# =====================
# r_t = 0.05*r_{t-4} - 0.0432*r_{t-5} - 0.0566*r_{t-11}
# forecast using ar(11,0,1)
# =====================
m = arima(log(vm),order = c(11,1,0), fixed = c(0,0,0,NA,NA,0,0,0,0,0,NA))
m

fore = predict(m,10)

U=fore$pred +1.96 * fore$se
L=fore$pred - 1.96 * fore$se

E1=exp(fore$pred+fore$se*fore$se/2)
U1 = exp(U)
L1 = exp(L)

E1
# 38.91786 38.91121 38.92941 38.77505 38.84460 38.90254 38.86277 38.86472 38.89102 38.85076
U1
# 40.44581 41.08443 41.60430 41.86317 42.35145 42.75927 43.03411 43.33445 43.64681 43.86802
L1
# 37.43303 36.82422 36.38390 35.85875 35.55730 35.30935 34.99831 34.74551 34.52984 34.27112


plot(1:30,append(vm[2447:2466],E1),ylim=c(0,80),type="o",ylab="",xlab="",main="Forecasting")
lines(20:30, append(vm[2466],E1),type="o",col="red")
lines(20:30, append(vm[2466],U1),type="l",col="blue")
lines(20:30, append(vm[2466],L1),type="l",col="blue")
legend(x="topleft",c("prediction"),lty=c(1,1),pch=c(1,1),col=c("red"))



library(TSA)
eacf(log_rtn)

for (p in c(0,1,2)){
  for (q in c(0,1,2)){
    mm = arima(log_rtn,order = c(p,0,q))
    print(1-pchisq(Box.test(mm$residuals,12,type = "Ljung")$statistic,12-p-q))
    print(BIC(mm))
  }
}

# =====================
# GARCH MODEL
# =====================

Box.test(log_rtn^2,12,type="Ljung")
# X-squared = 378.92, df = 12, p-value < 2.2e-16


# =================
# garch(1,1)
# =================
library(fGarch)
m=garchFit(log_rtn~garch(1,1),data=log_rtn,trace=F,include.mean = FALSE)
summary(m)

# model checking
# Obtain standardized residuals.
stresi=residuals(m,standardize=T)

Box.test(stresi,12,type="Ljung")
# p-value = 0.006136 , is not correct

Box.test(stresi^2,12,type="Ljung")
# p-value = 0.912 > 0.05, so we think garch part is correct 



# =================
# ar1 + garch(1,1)
# =================

m=garchFit(log_rtn~arma(5,0)+garch(1,1),data=log_rtn,trace=F,include.mean = FALSE)
summary(m)

# model checking
# Obtain standardized residuals.
stresi=residuals(m,standardize=T)

Box.test(stresi,12,type="Ljung")
# p-value = 0.2392 > 0.05, so we think arma part is correct 

Box.test(stresi^2,12,type="Ljung")
# p-value = 0.9149 > 0.05, so we think garch part is correct 

#AIC       BIC       SIC      HQIC 
#-5.206021 -5.196593 -5.206026 -5.202596 


# =================
# ma1 + garch(1,1)
# =================
m=garchFit(log_rtn~arma(0,1)+garch(1,1),data=log_rtn,trace=F,include.mean = FALSE)
summary(m)

# model checking
# Obtain standardized residuals.
stresi=residuals(m,standardize=T)

Box.test(stresi,12,type="Ljung")
# p-value = 0.2466 > 0.05, so we think arma part is correct 

Box.test(stresi^2,12,type="Ljung")
# p-value = 0.9119 > 0.05, so we think garch part is correct 

# AIC       BIC       SIC      HQIC 
#-5.205979 -5.196552 -5.205985 -5.202554 


library(rugarch)

# ===========
# igarch(1,1)
# ===========

spec1=ugarchspec(variance.model=list(model="iGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(0,0),include.mean = TRUE),
                 distribution.model = "norm")
mm=ugarchfit(spec=spec1,data=log_rtn)
mm

# Obtain standardized residuals.
stresi = residuals(mm,standardize=T)

Box.test(stresi,10,type="Ljung")
# p-value = 0.006445 not correct 

Box.test(stresi^2,10,type="Ljung")
# p-value = 0.8412 > 0.05, so we think garch part is correct 


# ========================
# arma(1,0) + igarch(1,1)
# ========================

spec1=ugarchspec(variance.model=list(model="iGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(1,0),include.mean = TRUE),
                 distribution.model = "norm",
                 fixed.pars = list(mu=0,omega=0))
mm=ugarchfit(spec=spec1,data=log_rtn)
mm

# Obtain standardized residuals.
stresi = residuals(mm,standardize=T)

Box.test(stresi,12,type="Ljung")
# p-value = 0.2024  correct 

Box.test(stresi^2,12,type="Ljung")
# p-value = 0.7412 > 0.05, so we think garch part is correct 

# Akaike       -5.1893
# Bayes        -5.1846
# Shibata      -5.1893
# Hannan-Quinn -5.1876


# ========================
# arma(0,1) + igarch(1,1)
# ========================

spec1=ugarchspec(variance.model=list(model="iGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(0,1),include.mean = TRUE),
                 distribution.model = "norm",
                 fixed.pars = list(mu=0,omega=0))
mm=ugarchfit(spec=spec1,data=log_rtn)
mm

# Obtain standardized residuals.
stresi = residuals(mm,standardize=T)

Box.test(stresi,12,type="Ljung")
# p-value = 0.2039  correct 

Box.test(stresi^2,12,type="Ljung")
# p-value = 0.7397 > 0.05, so we think garch part is correct 

# Akaike       -5.1893
# Bayes        -5.1845
# Shibata      -5.1893
# Hannan-Quinn -5.1875


# ========================
# arma(1,0) + egarch(1,1)
# ========================

spec=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(1,1)),
                mean.model=list(armaOrder=c(1,0),include.mean = TRUE),
                distribution.model = "norm",
                fixed.pars = list(mu=0))
mm=ugarchfit(spec=spec,data=log_rtn)
mm

# Obtain standardized residuals.
stresi = residuals(mm,standardize=T)

Box.test(stresi,12,type="Ljung")
# p-value = 0.2119  correct 

Box.test(stresi^2,12,type="Ljung")
# p-value = 0.8028 > 0.05, so we think garch part is correct 

# Akaike       -5.2063
# Bayes        -5.1945
# Shibata      -5.2063
# Hannan-Quinn -5.2020



# ========================
# arma(0,1) + egarch(1,1)
# ========================

spec=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(1,1)),
                mean.model=list(armaOrder=c(0,1),include.mean = TRUE),
                distribution.model = "norm",
                fixed.pars = list(mu=0))
mm=ugarchfit(spec=spec,data=log_rtn)
mm

# Obtain standardized residuals.
stresi = residuals(mm,standardize=T)

Box.test(stresi,12,type="Ljung")
# p-value = 0.2187  correct 

Box.test(stresi^2,12,type="Ljung")
# p-value = 0.7979 > 0.05, so we think garch part is correct 

# Akaike       -5.2062
# Bayes        -5.1944
# Shibata      -5.2062
# Hannan-Quinn -5.2019



# ==============================
# choose arma(1,0) + igarch(1,1)
# ==============================

spec1=ugarchspec(variance.model=list(model="iGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(1,0),include.mean = TRUE),
                 distribution.model = "norm",
                 fixed.pars = list(mu=0,omega=0))
mm=ugarchfit(spec=spec1,data=log_rtn)
mm

fore = ugarchboot(mm, method=c("Partial", "Full")[1],n.ahead = 4, n.bootpred = 4)
show(fore)







# ===========
# igarch(1,1)
# ===========

spec1=ugarchspec(variance.model=list(model="iGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(0,0),include.mean = TRUE),
                 distribution.model = "std")
mm=ugarchfit(spec=spec1,data=log_rtn)
mm

# Obtain standardized residuals.
stresi = residuals(mm,standardize=T)

Box.test(stresi,10,type="Ljung")
# p-value = 0.006445 not correct 

Box.test(stresi^2,10,type="Ljung")
# p-value = 0.8412 > 0.05, so we think garch part is correct 


# ========================
# arma(1,0) + igarch(1,1)
# ========================

spec1=ugarchspec(variance.model=list(model="iGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(1,0),include.mean = TRUE),
                 distribution.model = "std",
                 fixed.pars = list(mu=0,omega=0))
mm=ugarchfit(spec=spec1,data=log_rtn)
mm

# Obtain standardized residuals.
stresi = residuals(mm,standardize=T)

Box.test(stresi,12,type="Ljung")
# p-value = 0.2024  correct 

Box.test(stresi^2,12,type="Ljung")
# p-value = 0.7412 > 0.05, so we think garch part is correct 

# Akaike       -5.1893
# Bayes        -5.1846
# Shibata      -5.1893
# Hannan-Quinn -5.1876


# ========================
# arma(0,1) + igarch(1,1)
# ========================

spec1=ugarchspec(variance.model=list(model="iGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(0,1),include.mean = TRUE),
                 distribution.model = "norm",
                 fixed.pars = list(mu=0,omega=0))
mm=ugarchfit(spec=spec1,data=log_rtn)
mm

# Obtain standardized residuals.
stresi = residuals(mm,standardize=T)

Box.test(stresi,12,type="Ljung")
# p-value = 0.2039  correct 

Box.test(stresi^2,12,type="Ljung")
# p-value = 0.7397 > 0.05, so we think garch part is correct 

# Akaike       -5.1893
# Bayes        -5.1845
# Shibata      -5.1893
# Hannan-Quinn -5.1875
