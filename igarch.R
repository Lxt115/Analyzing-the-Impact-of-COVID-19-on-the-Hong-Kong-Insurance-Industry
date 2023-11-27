setwd('E://bdt//5006//proj/')
par(mfrow=c(2,2))

vm = read.csv('./2318.HK.csv')[,6]
length(vm)
plot(vm,main="",ylab="price",xlab="",type="l", col='red')

adf.test(vm)
# Dickey-Fuller = -0.98727, Lag order = 13, p-value = 0.9408
# non stationary

log_rtn = diff(log(vm))
plot(log_rtn,main="",ylab="log_rtn",xlab="",type="l", col='red')

adf.test(log_rtn)

library(rugarch)


spec1=ugarchspec(variance.model=list(model="iGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(0,1),include.mean = TRUE),
                 distribution.model = "std",
                 fixed.pars = list(mu=0,omega=0))
g1i=ugarchfit(spec=spec1,data=log_rtn)

spec2=ugarchspec(variance.model=list(model="iGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(5,0),include.mean = TRUE),
                 distribution.model = "std",
                 fixed.pars = list(mu=0,ar2=0,ar3=0,ar4=0,ar5=0,omega=0))
g2i=ugarchfit(spec=spec2,data=log_rtn)

spec3=ugarchspec(variance.model=list(model="iGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(11,0),include.mean = TRUE),
                 distribution.model = "std",
                 fixed.pars = list(mu=0,ar2=0,ar3=0,ar4=0,ar5=0,ar7=0,ar8=0,ar9=0,ar10=0,omega=0))
g3i=ugarchfit(spec=spec3,data=log_rtn)

spec4=ugarchspec(variance.model=list(model="iGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(1,1),include.mean = TRUE),
                 distribution.model = "std",
                 fixed.pars = list(mu=0, ar1=0, ma1 = 0,omega=0))
g4i=ugarchfit(spec=spec4,data=log_rtn)

ics = cbind(infocriteria(g1i),infocriteria(g2i),infocriteria(g3i),infocriteria(g4i))

x_label <- c("AIC", "BIC", "SIC", "HQIC")

par(mar = c(5, 9, 4, 9.5))


# Plot the chart with swapped x-axis and y-axis
plot(ics[,1], type = "b", pch = 16, col = "burlywood", xlab = "Criterion", ylab = "value", 
     ylim = c(min(ics), max(ics)), main = "Information Criteria of iGARCH(std)", xaxt = "n")
lines(ics[,2], type = "b", pch = 16, col = "firebrick")
lines(ics[,3], type = "b", pch = 16, col = "mediumseagreen")
lines(ics[,4], type = "b", pch = 16, col = "powderblue")

axis(1, at = 1:length(ics[1,]), labels = x_label, tick = FALSE)


# Add a legend outside the plot
legend(x="topright", legend = c("1", "2", "3", "4"), col = c("burlywood", "firebrick", "mediumseagreen", "powderblue")
       , pch = 16, xpd = TRUE, horiz = FALSE)

fore = ugarchboot(g3i, method=c("Partial", "Full")[1],n.ahead = 10, n.bootpred = 20)
show(fore)
U1 = c(0.020376,
       0.026885,
       0.018217,
       0.056581,
       0.085011,
       0.089280,
       0.030401,
       0.106658,
       0.045400,
       0.034229)
L1 = c(-0.048850,
       -0.031734,
       -0.053965,
       -0.058123,
       -0.038120,
       -0.054555,
       -0.043320,
       -0.043656,
       -0.082537,
       -0.039811)
E1 = fore@fseries[17,]
plot(1:60,append(log_rtn[2416:2465],E1),ylim=c(-0.1,0.1),type="o",ylab="",xlab="",main="Forecasting")
lines(50:60, append(log_rtn[2465],E1),type="o",col="red")
lines(50:60, append(log_rtn[2465],U1),type="l",col="blue")
lines(50:60, append(log_rtn[2465],L1),type="l",col="blue")
legend(x="topleft",c("prediction"),lty=c(1,1),pch=c(1,1),col=c("red"))

# Obtain standardized residuals.
stresi = residuals(g3i,standardize=T)
Box.test(stresi,12,type="Ljung")
Box.test(stresi^2,12,type="Ljung")

plot(as.vector(stresi),main="standard residuals",ylab="",xlab="",type="l")
plot(as.vector(stresi)^2,main="squared standard residuals",ylab="",xlab="",type="l")
