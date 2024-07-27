#DATA

library(readxl)
d <- read_excel("~/suji/SCC/FINAL  15 DATA SETS.xlsx", 
 sheet = "Sheet2")

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
hii
