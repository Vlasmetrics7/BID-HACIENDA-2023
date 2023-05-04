# CÓDIGO 6: descomposición automática de una serie temporal

#install.packages("readxl")

library(readxl)
library(tidyverse)

# usamos read_csv para leer la base de datos cin extensión csv
BaseCNS <- read_csv("Incidencia.csv")

# cargamos las columnas que nos interesan y les damos formatos de series 
# temporales. Las variables son mensuales por lo que su frecuencia es 12.
# (12 meses en un año)

incidencia <- ts(BaseCNS$tasa_incid,frequency=12,start=c(1997,1))
homicidios<- ts(BaseCNS$tasa_homic,frequency=12,start=c(1997,1))
extorsion <- ts(BaseCNS$tasa_extor,frequency=12,start=c(1997,1))
secuestro <- ts(BaseCNS$tasa_secue,frequency=12,start=c(1997,1))

# el comando stl descompone la serie de tiempo
plot(stl(homicidios, "periodic"))

# por ejemplo si solo quisieramos la tendencia de la serie
x<-stl(homicidios,"periodic")
plot(x$time.series[,2])




