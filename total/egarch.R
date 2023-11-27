setwd('E://bdt//5006//proj/')
par(mfrow=c(2,1))

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


spec1=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(0,1),include.mean = TRUE),
                 distribution.model = "std",
                 fixed.pars = list(mu=0))
g1e=ugarchfit(spec=spec1,data=log_rtn)

spec2=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(5,0),include.mean = TRUE),
                 distribution.model = "std",
                 fixed.pars = list(mu=0))
g2e=ugarchfit(spec=spec2,data=log_rtn)

spec3=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(11,0),include.mean = TRUE),
                 distribution.model = "std",
                 fixed.pars = list(mu=0))
g3e=ugarchfit(spec=spec3,data=log_rtn)

spec4=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(1,1),include.mean = TRUE),
                 distribution.model = "std",
                 fixed.pars = list(mu=0))
g4e=ugarchfit(spec=spec4,data=log_rtn)

ics = cbind(infocriteria(g1e),infocriteria(g2e),infocriteria(g3e),infocriteria(g4e))


x_label <- c("AIC", "BIC", "SIC", "HQIC")

par(mar = c(5, 9, 4, 9.5))

# Plot the chart with swapped x-axis and y-axis
plot(ics[,1], type = "b", pch = 16, col = "burlywood", xlab = "Criterion", ylab = "value", 
     ylim = c(min(ics), max(ics)), main = "Information Criteria of eGARCH(std)", xaxt = "n")
lines(ics[,2], type = "b", pch = 16, col = "firebrick")
lines(ics[,3], type = "b", pch = 16, col = "mediumseagreen")
lines(ics[,4], type = "b", pch = 16, col = "powderblue")

axis(1, at = 1:length(ics[1,]), labels = x_label, tick = FALSE)

# Set the coordinates for the legend outside the plot
legend_x <- length(ics[,1]) + 0.3
legend_y <- mean(range(ics)) +0.08

# Add a legend outside the plot
legend(x = "topright",legend = c("1", "2", "3", "4"), col = c("burlywood", "firebrick", "mediumseagreen", "powderblue")
       , pch = 16, xpd = TRUE, horiz = FALSE)
legend(x = "topright", legend = "Data Points", pch = 16, col = "blue")
