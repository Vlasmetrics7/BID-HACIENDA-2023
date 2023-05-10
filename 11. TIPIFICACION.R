# CODIGO 11: Seleccionando posibles modelos

library(quantmod)

# EJEMPLO 1

# inflación de Estados Unidos
# Growth rate previous period, Not Seasonally Adjusted

getSymbols('CPALTT01USQ657N',src = 'FRED')
cpi.changes <- CPALTT01USQ657N
plot(cpi.changes,main='CPI',ylab='%',xlab='Time')

par(mfrow=c(2,1))
acf(cpi.changes,lag=20)
acf(cpi.changes,type="partial",lag=20)


# EJEMPLO 2
# Tasa de desempleo
#  Percent, Seasonally Adjusted

getSymbols('UNRATE',src = 'FRED')
plot(UNRATE,main='Unemployment Rate',ylab='%',xlab='Time')

par(mfrow=c(2,1))
acf(UNRATE,lag=20)
acf(UNRATE,type="partial",lag=20)

# EJEMPLO 3
# Consumo: Personal Consumption Expenditures (PCEC)

getSymbols('PCEC',src = 'FRED')
plot(PCEC,main='Personal Consumption Expenditures',ylab='$USD Billions',xlab='Time')

par(mfrow=c(2,1))
acf(PCEC,lag=20)
acf(PCEC,type="partial",lag=20)

# Tomemos justamente esta serie para ejemplificar qué 
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


# Ejemplo 4

library(readxl)
library(tidyverse)

BaseCNS <- read_csv("Incidencia.csv")

homicidios<- ts(BaseCNS$tasa_homic,frequency=12,start=c(1997,1))

# Descomponemos serie
plot(decompose(homicidios, "additive"))

Ruido<- decompose(homicidios)$random

par(mfrow=c(2,2))
acf(homicidios,lag=20,main='Homicidios')
acf(na.omit(Ruido),lag=20,main='Ruido tras descomponer serie')
acf(homicidios,type="partial",lag=20,main="")
acf(na.omit(Ruido),type="partial",lag=20,main="")



