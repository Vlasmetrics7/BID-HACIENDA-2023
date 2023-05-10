# CODIGO 12: Estimando modelos

#install.packages("TSA")
#install.packages("astsa")
library(TSA)
library(astsa)
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

# Entendamos de manera general el comando
# ?arima

mod1=arima(cpi.changes,order=c(3,0,0),include.mean = TRUE,
           method = c("ML"))

mod2=arima(cpi.changes,order=c(6,0,0),include.mean = TRUE,
           method = c("ML"))

library(stargazer)

stargazer(mod1, mod2, column.labels = c("AR(3)", "AR(6)"),
          type="text")

# usando el otro paquete
# ver estimación solo en una tabla
sarima(cpi.changes, 3,0,0,MODEL = TRUE,details = FALSE)$ttable 
# ver mucha más información Y diagnóstico de residuales
sarima(cpi.changes, 3,0,0,MODEL = TRUE,details = TRUE)

# EJEMPLO 3
# Consumo: Personal Consumption Expenditures (PCEC)

getSymbols('PCEC',src = 'FRED')
plot(PCEC,main='Personal Consumption Expenditures',ylab='$USD Billions',xlab='Time')

# Diferenciando la serie

DPCEC<-diff(PCEC,1)  # Calculamos primera diferencia

par(mfrow=c(2,2))
acf(PCEC,lag=20,main='Correlograma PCEC')
acf(DPCEC[2:length(DPCEC)],lag=20,main='Correlograma PCEC en 1as diferencias')
acf(PCEC,type="partial",lag=20,main="")
acf(DPCEC[2:length(DPCEC)],type="partial",lag=20,main="")

mod3=arima(PCEC,order=c(2,1,0),include.mean = FALSE,
           method = c("ML"))

mod4=arima(DPCEC,order=c(2,0,0),include.mean = FALSE,
           method = c("ML"))

stargazer(mod3, mod4, column.labels = c("ARIMA(2,1,0)", "ARMA(2,0) de 1as diff"),
          type="text")




