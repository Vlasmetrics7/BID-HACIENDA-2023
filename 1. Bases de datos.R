# CODIGO 1: BASES DE DATOS MACROECONÓMICAS

#install.packages('quantmod')
library(quantmod)

# EJEMPLO 1

# inflación de Estados Unidos

# Mide qué tan rápido están subiendo los precios siguiendo los precios de una canasta fija de
# bienes y servicios a lo largo del tiempo

# la extraemos de la Federal Reserve Bank of St. Louis’s
#?getSymbols
getSymbols('CPALTT01USQ657N',src = 'FRED')

# Observamos las primeras entradas de la variable que acabamos de extraer 
cpi.changes <- CPALTT01USQ657N
head(cpi.changes)

summary(cpi.changes)

plot(cpi.changes,main='CPI',ylab='%',xlab='Time')


# EJEMPLO 2
# Tasa de desempleo: Mide la fracción de la fuerza laboral que está sin trabajo. 

getSymbols('UNRATE',src = 'FRED')
summary(UNRATE)
plot(UNRATE,main='Unemployment Rate',ylab='%',xlab='Time')


# EJEMPLO 3
# Consumo vía FRED: Personal Consumption Expenditures (PCEC)
# Los bienes y servicios que compran los hogares

getSymbols('PCEC',src = 'FRED')
summary(PCEC)
plot(PCEC,main='Personal Consumption Expenditures',ylab='$USD Billions',xlab='Time')


# Como sabemos existen 
# 1) Personal Consumption Expenditures: Durable Goods
# 2) Personal Consumption Expenditures: Nondurable Goods
# 3) Personal Consumption Expenditures: Services

# Nos preguntamos 
# ¿Cómo se comporta el consumo de estas tres categorías a lo largo del tiempo?
getSymbols('PCDG',src = 'FRED')
getSymbols('PCND',src = 'FRED')
getSymbols('PCESV',src = 'FRED')

consumptionData <- merge(PCDG,PCND,PCESV,all = T)
head(consumptionData)

consumptionData <- na.omit(consumptionData)
totalConsumption <- apply(consumptionData,1,sum)
head(totalConsumption)


consumptionData$fractionDurable <- consumptionData$PCDG/totalConsumption
consumptionData$fractionNondurable <- consumptionData$PCND/totalConsumption
consumptionData$fractionServices <- 1 - consumptionData$fractionDurable - consumptionData$fractionNondurable
plot(consumptionData$fractionDurable,main='Consumption Composition',ylim=c(0,1))
lines(consumptionData$fractionNondurable,col = 2)
lines(consumptionData$fractionServices,col = 3)
legend("top",legend = c("Durable","Nondurable","Services"),col = 1:3,lty = 1)


# EJEMPLO 4
# DEFLACTOR DEL PIB: (PIB nominal / PIB real) refleja lo que está sucediendo con 
# el nivel general de precios en la economía. 
# FRED Code: Gross Domestic Product: Implicit Price Deflator (GDPDEF)

# inflación de Estados Unidos (lo hicimos antes)
getSymbols('CPALTT01USQ657N',src = 'FRED')
cpi.changes <- CPALTT01USQ657N

getSymbols('GDPDEF',src ='FRED')

gdpDeflatorChange <- Delt(GDPDEF,type = 'arithmetic')
plot(100*gdpDeflatorChange,main='GDP Deflator')
lines(cpi.changes,col = 2)
legend('topleft',legend = c('Deflator','CPI'),col = 1:2,lty = 1)





