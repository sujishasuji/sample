#DATA

library(readxl)
d <- read_excel("~/suji/SCC/FINAL  15 DATA SETS.xlsx", 
 sheet = "Sheet2")
library(tseries)
library(rugarch)
library(fGarch)
library(aTSA)
library(car)
library(FinTS)

#DESCRIPTIVE STATISTICS
summary(d)
skewness(d[-1])
kurtosis(d[-1])

#Time plot
xt=ts(d,frequency = 52,start=c(2016,1));xt
P=plot.ts(xt[,c(2:11)],main="Plot of closing prices in stock market during covid 19",xlab="timepoints",ylab="weekly closing prices")
xt[,c(2:10)]

#or
par(mfrow=c(2,5))
i={}
for (i in 2:16) {
  xt=ts(d[i],frequency = 52,start=c(2016,1))
  p=plot.ts(xt,main= colnames(d)[i],xlab="time points",ylab="weekly closing prices")
}

# log return
n=nrow(d);n 
logreturn={} 
for(i in 2:16)
  a=d[,i]
  for(j in 1:(n-1))
    logreturn[i]=log(a[j+1])-log(a[j])
summary(logreturn)


plot(logreturn,main="Plot of logreturn series", type="l",xlab="timepoints",ylab="Silver price") 
par(mfrow=c(2,2)) 
acf(logreturn,main="ACF of Logreturns") 
pacf(logreturn,main="PACF of Logreturns") 
acf(logreturn^2,main="ACF of Squared Logreturns") 
pacf(logreturn^2,main="PACF of Squared Logreturns") 
k=kurtosis(logreturn);k 
cat("\n Excess kurtosis value for log return series is \n") 
print(k) 
skk=skewness(logreturn);skk

#central bank

x1=A$CENTRAL
xt=ts(x1,frequency=52,start=c(2016,1))
plot.ts(xt)
P=plot.ts(xt,main="central bank",xlab="timepoints",ylab="weekly closing prices");P


x1=A$CENTRAL
x1=as.vector(x1)
n=length(x1)
logreturn={}
for(i in 1:(n-1))
  logreturn[i]=log(x1[i+1])-log(x1[i])
logreturn

library(MTS)
archTest(logreturn)

summary(logreturn)
plot(x1,main="Plot of central bank closing price",type="l", xlab="timepoints", ylab="closing price")
plot(logreturn,main="Plot of logreturn series", type="l",xlab="timepoints",ylab="closingprice")
par(mfrow=c(2,2))
acf(logreturn,main="ACF of Logreturns")
pacf(logreturn,main="PACF of Logreturns")
acf(logreturn^2,main="ACF of Squared Logreturns")
pacf(logreturn^2,main="PACF of Squared Logreturns")
k=kurtosis(logreturn);k
cat("\n Excess kurtosis value for log return series is \n")
print(k) 
#GARCH Model

garch11.spec = ugarchspec(variance.model = list(garchOrder=c(1,1)),  
                          mean.model = list(armaOrder=c(0,0)))
MSFT.garch11.fit = ugarchfit(spec=garch11.spec, data=logreturn,out.sample=20)
MSFT.garch11.fit
forc1= ugarchforecast(MSFT.garch11.fit, n.ahead=20)
fpm(forc1)
a=residuals(MSFT.garch11.fit,standardize=TRUE)
plot(residuals(MSFT.garch11.fit),type="l") 


