# CÓDIGO 13: X13ARIMASEATS
# X13 ARIMA SEATS

#install.packages("x13binary")
#install.packages("seasonal")
library(x13binary)
library(seasonal)
library('fpp3')
#install.packages("seasonalview")
library("seasonalview")
library(readxl)

BaseCNS <- read.csv("Incidencia.csv")

homicidios<- ts(BaseCNS$tasa_homic,frequency=12,start=c(1997,1))
homdescomp = decompose(homicidios, "additive")
plot(homdescomp)

####################################################################
#By default, seas calls the SEATS adjustment procedure
hom.seats <- seas(homicidios)

view(hom.seats)

hom.pred=seas(
  x = homicidios,
  transform.function = "log",
  regression.aictest = "td"
)

#################
# modelo general estimado
summary(hom.pred)
plot(hom.pred$data,main="AJUSTE VIA SEATS")
