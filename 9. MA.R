# código 9: 

# SIMULANDO UN MA(1)
#rm(list=ls(all=TRUE))

u<- rnorm(300, mean = 0, sd = 1)
y<-matrix(ncol=1,nrow=300)
y[1]<-u[1]
for (i in 2:300) y[i] <- u[i]+0.9*u[i-1]
plot(y,type="l")

# USANDO FUNCIONES

ma1 <- arima.sim(list(order = c(0,0,1), ma = 0.9), n = 300)
par(mfrow=c(2,1))
par(mfrow=c(3,1),oma=c(2,2,2,2))
ts.plot(ma1)
acf(ma1,lag=20)
acf(ma1,type="partial",lag=20)