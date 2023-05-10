# código 10: 

# SIMULANDO UN AR(1)
#rm(list=ls(all=TRUE))

u<- rnorm(1000, mean = 0, sd = 10)
y<-matrix(ncol=1,nrow=1000)
y[1]<-u[1]
for (i in 2:1000) y[i] <- 0.7*y[i-1]+u[i]
plot(y,type="l")


# USANDO FUNCIONES

ar1 <- arima.sim(list(order = c(1,0,0), ar = 0.5), n = 300)
par(mfrow=c(2,1))
acf(ar1,lag=20)
acf(ar1,type="partial",lag=20)


# AR(1) VS AR(2)
AR1 <- arima.sim(list(order = c(1,0,0), ar = 0.8), n = 300)
AR2 <- arima.sim(list(order = c(2,0,0), ar =  c(0.25, 0.5)), n = 300)
par(mfrow=c(2,1))
acf(AR1,main='ACF de un Proceso AR(1)')
acf(AR2,main='ACF de un Proceso AR(2)')


