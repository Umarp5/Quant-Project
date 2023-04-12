set.seed(1)
install.packages("readxl")
install.packages("car")
install.packages("prais")
install.packages("sandwich")
install.packages("lmtest")
install.packages("cardata")
#
library(car)
library(readxl)
library(car)
library(prais)
library(sandwich)
library(lmtest)
library(carData)
###
library(car)
###
reg1=lm(log(tp)~ga, data = FPLDataoriginal)
summary(reg1)
###
reg2=lm(log(tp)~Minutes+ga+Creativity+Threat, data = FPLDataoriginal)
summary(reg2)
##
reg3=lm(log(tp)~Minutes+ga+Creativity, data = FPLDataoriginal)
summary(reg3)
##
#obtain vif
reg3vif=vif(reg3)
##plot vif
barplot(reg3vif, main = "VIF Values model (1)", horiz =TRUE, col = "blue", 
        xlim=c(0,7), density = 20, las=2)
abline(v = 5, lwd = 3, lty = 2, col="red")
#
#autocorrelation
errors3=residuals(reg3)
# plot errors
par(mfrow=c(1,2))
plot(errors3, xlab="Fitted Values", ylab="Residuals", pch="*", cex=2, col="blue")
plot(errors3[-41]~errors4[-1], xlab="Residuals (lag 1)", ylab="Residuals", 
     pch="*", cex=2, col="red")
##
# perform DW test
durbinWatsonTest(reg3)
# correct for potential autocorelation with NW appraoch
NWVcov=NeweyWest(reg3, lag=1, prewhite=FALSE, adjust=TRUE)
coeftest(reg3, vcov=NWVcov)
##
##obtain fitted values
fitted4=fitted(reg3)
# perform visual diagnostics
par(mfrow=c(1,1))
plot(errors3~fitted4, xlab="Fitted Values", ylab="Residuals", pch="*", cex=2,
     col="blue")
         