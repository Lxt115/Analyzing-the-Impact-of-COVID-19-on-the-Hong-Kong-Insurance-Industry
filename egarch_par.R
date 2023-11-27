setwd('E://bdt//5006//proj/')
par(mfrow=c(2,1))

library(rugarch)


spec1=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(0,1),include.mean = TRUE),
                 distribution.model = "std",
                 fixed.pars = list(mu=0))
g1e=ugarchfit(spec=spec1,data=log_rtn)

spec2=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(1,2)),
                 mean.model=list(armaOrder=c(0,1),include.mean = TRUE),
                 distribution.model = "std",
                 fixed.pars = list(mu=0))
g2e=ugarchfit(spec=spec2,data=log_rtn)

spec3=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(2,1)),
                 mean.model=list(armaOrder=c(0,1),include.mean = TRUE),
                 distribution.model = "std",
                 fixed.pars = list(mu=0))
g3e=ugarchfit(spec=spec3,data=log_rtn)

spec4=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(2,2)),
                 mean.model=list(armaOrder=c(0,1),include.mean = TRUE),
                 distribution.model = "std",
                 fixed.pars = list(mu=0))
g4e=ugarchfit(spec=spec4,data=log_rtn)

spec5=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(0,1),include.mean = TRUE),
                 distribution.model = "norm",
                 fixed.pars = list(mu=0))
g5e=ugarchfit(spec=spec5,data=log_rtn)

spec6=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(1,2)),
                 mean.model=list(armaOrder=c(0,1),include.mean = TRUE),
                 distribution.model = "norm",
                 fixed.pars = list(mu=0))
g6e=ugarchfit(spec=spec6,data=log_rtn)

spec7=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(2,1)),
                 mean.model=list(armaOrder=c(0,1),include.mean = TRUE),
                 distribution.model = "norm",
                 fixed.pars = list(mu=0))
g7e=ugarchfit(spec=spec7,data=log_rtn)

spec8=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(2,2)),
                 mean.model=list(armaOrder=c(0,1),include.mean = TRUE),
                 distribution.model = "norm",
                 fixed.pars = list(mu=0))
g8e=ugarchfit(spec=spec8,data=log_rtn)

ics = cbind(infocriteria(g1e),infocriteria(g2e),infocriteria(g3e),infocriteria(g4e),
            infocriteria(g5e),infocriteria(g6e),infocriteria(g7e),infocriteria(g8e))


x_label <- c("AIC", "BIC", "SIC", "HQIC")

par(mar = c(5, 9, 4, 9.5))

# Plot the chart with swapped x-axis and y-axis
plot(ics[,1], type = "b", pch = 16, col = "burlywood", xlab = "Criterion", ylab = "value", 
     ylim = c(min(ics), max(ics)), main = "Information Criteria of eGARCH(std)", xaxt = "n")
lines(ics[,2], type = "b", pch = 16, col = "firebrick")
lines(ics[,3], type = "b", pch = 16, col = "mediumseagreen")
lines(ics[,4], type = "b", pch = 16, col = "powderblue")
lines(ics[,5], type = "b", pch = 16, col = "palegreen")
lines(ics[,6], type = "b", pch = 16, col = "pink")
lines(ics[,7], type = "b", pch = 16, col = "orchid")
lines(ics[,8], type = "b", pch = 16, col = "lightsalmon")

axis(1, at = 1:length(ics[,1]), labels = x_label, tick = FALSE)

# Add a legend outside the plot
legend(x="topright",inset = c(-0.27,0), legend = c("(1,1) std", "(1,2) std", "(2,1) std", "(2,2) std",
                             "(1,1) norm", "(1,2) norm", "(2,1) norm", "(2,2) norm")
       , col = c("burlywood", "firebrick", "mediumseagreen", "powderblue",
                 "palegreen","pink","lightsalmon","royalblue")
       , pch = 16, xpd = TRUE, horiz = FALSE)
