
# Código 8. Filtro de Hodrick-Prescott para encontrar componente de ciclo

#install.packages("mFilter")
#install.packages("xts")

library(readxl)
library(mFilter)
library(xts)

# La serie temporal es PIB de México anual 1960-2021 (constant 2015 US$)

series <- read_excel("mex.xlsx")
mex <- ts(series, frequency = 1, start=c(1960,1))
plot(hpfilter(mex[,2],freq = 100))

hpf<-hpfilter(mex[,2],freq = 100)


plot(hpf$cycle)