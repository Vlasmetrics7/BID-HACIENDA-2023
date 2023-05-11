


#install.packages("strucchange")
library(strucchange)
library(quantmod)

# EJEMPLO 1

# inflación de Estados Unidos
# Growth rate previous period, Not Seasonally Adjusted

getSymbols('CPALTT01USQ657N',src = 'FRED')
cpi.changes <- CPALTT01USQ657N
cpi<-ts(cpi.changes,frequency = 4,start = c(1960,1))
plot(cpi,main='CPI',ylab='%',xlab='Time')

###############################
# Identificando rompimientos estructurales con 
# la metodología de Bai-Perron
###############################

# Rompimiento en la media del proceso

TSdata_brk <- breakpoints(cpi ~ 1, h = 0.15)
TSdata_brk
nbreaks = length(TSdata_brk$breakpoints)  #Numero de breaks

plot(cpi,ylim=c(min(cpi),max(cpi)),ylab="",xlab="",lwd = 3,main = "Distintas fases de la inflación (niveles)") 
abline(v=breakdates(TSdata_brk), col="gray70", lty=3, lwd=2, untf=TRUE)
lines(fitted(TSdata_brk, breaks = length(TSdata_brk$breakpoints)), col = "red",lty = 1, lwd = 2)
lines(confint(TSdata_brk, breaks = length(TSdata_brk$breakpoints)),lwd=2)


# Rompimiento en la tendencia del proceso
tt <- 1:length(cpi) 
TSdata_brk <- breakpoints(cpi ~ 1+tt, h = 0.13)
TSdata_brk
nbreaks = length(TSdata_brk$breakpoints)  #Numero de breaks

plot(cpi,ylim=c(min(cpi),max(cpi)),ylab="",xlab="",lwd = 3,main = "Distintas fases de la inflación (tendencia)") 
abline(v=breakdates(TSdata_brk), col="gray70", lty=3, lwd=2, untf=TRUE)
lines(fitted(TSdata_brk, breaks = length(TSdata_brk$breakpoints)), col = "red",lty = 1, lwd = 2)
lines(confint(TSdata_brk, breaks = length(TSdata_brk$breakpoints)),lwd=2)

##################################################

# Ejemplo de homicidios

library(readxl)
library(tidyverse)

BaseCNS <- read_csv("Incidencia.csv")

homicidios<- ts(BaseCNS$tasa_homic,frequency=12,start=c(1997,1))


# Rompimiento en la media del proceso

TSdata_brk <- breakpoints(homicidios ~ 1, h = 0.15)
TSdata_brk
nbreaks = length(TSdata_brk$breakpoints)  #Numero de breaks

plot(homicidios,ylim=c(min(homicidios),max(homicidios)),ylab="",xlab="",lwd = 3,main = "Distintas fases de la inflación (niveles)") 
abline(v=breakdates(TSdata_brk), col="gray70", lty=3, lwd=2, untf=TRUE)
lines(fitted(TSdata_brk, breaks = length(TSdata_brk$breakpoints)), col = "red",lty = 1, lwd = 2)
lines(confint(TSdata_brk, breaks = length(TSdata_brk$breakpoints)),lwd=2)

# Rompimiento en la tendencia del proceso

tt <- 1:length(homicidios) 
TSdata_brk <- breakpoints(homicidios ~ 1+tt, h = 0.12)
TSdata_brk
nbreaks = length(TSdata_brk$breakpoints)  #Numero de breaks

plot(homicidios,ylim=c(min(homicidios),max(homicidios)),ylab="",xlab="",lwd = 3,main = "Distintas fases de la inflación (niveles)") 
abline(v=breakdates(TSdata_brk), col="gray70", lty=3, lwd=2, untf=TRUE)
lines(fitted(TSdata_brk, breaks = length(TSdata_brk$breakpoints)), col = "red",lty = 1, lwd = 2)
lines(confint(TSdata_brk, breaks = length(TSdata_brk$breakpoints)),lwd=2)

