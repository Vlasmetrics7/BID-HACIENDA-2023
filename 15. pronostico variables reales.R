# CODIGO 15: Pronostiquemos las variables que ya hemos visto

library(quantmod)

# EJEMPLO 1

# inflaci?n de Estados Unidos
# Growth rate previous period, Not Seasonally Adjusted

getSymbols('CPALTT01USQ657N',src = 'FRED')
cpi.changes <- CPALTT01USQ657N
plot(cpi.changes,main='CPI',ylab='%',xlab='Time')

par(mfrow=c(2,1))
acf(cpi.changes,lag=20)
acf(cpi.changes,type="partial",lag=20)

# AR(3)
cpi<-ts(cpi.changes,frequency = 4,start = c(1960,1))
preAR = predict(arima(cpi, order=c(3,0,0)), 4)
forecast <- ts(preAR$pred, frequency = 4,start = c(2023,2))

ts.plot(cpi, forecast , col=1:2,main="Pronosticando un AR(1)")
Upp = preAR$pred+1.96*preAR$se; Low = preAR$pred-1.96*preAR$se
xx = c(time(Upp), rev(time(Upp))); yy = c(Low, rev(Upp))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
lines(preAR$pred, type="p", col=2)


# EJEMPLO 3
# Consumo: Personal Consumption Expenditures (PCEC)

getSymbols('PCEC',src = 'FRED')
plot(PCEC,main='Personal Consumption Expenditures',ylab='$USD Billions',xlab='Time')

par(mfrow=c(2,1))
acf(PCEC,lag=20)
acf(PCEC,type="partial",lag=20)

# Tomemos justamente esta serie para ejemplificar qu? 
# pasa al diferenciar la serie

# Diferenciando la serie

DPCEC<-diff(PCEC,1)  # Calculamos primera diferencia

par(mfrow=c(2,1))
plot(PCEC,main='Personal Consumption Expenditures',ylab='$USD Billions',xlab='Time')
plot(DPCEC,main='First Difference',ylab='',xlab='Time')

par(mfrow=c(2,2))
acf(PCEC,lag=20,main='Correlograma PCEC')
acf(DPCEC[2:length(DPCEC)],lag=20,main='Correlograma PCEC en 1as diferencias')
acf(PCEC,type="partial",lag=20,main="")
acf(DPCEC[2:length(DPCEC)],type="partial",lag=20,main="")

PCEC<-ts(PCEC,frequency = 4,start = c(1947,1))
preAR = predict(arima(PCEC, order=c(3,1,0)), 4)
forecast <- ts(preAR$pred, frequency = 4,start = c(2023,2))

ts.plot(PCEC, forecast , col=1:2,main="Pronosticando un AR(1)")
Upp = preAR$pred+1.96*preAR$se; Low = preAR$pred-1.96*preAR$se
xx = c(time(Upp), rev(time(Upp))); yy = c(Low, rev(Upp))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
lines(preAR$pred, type="p", col=2)

























