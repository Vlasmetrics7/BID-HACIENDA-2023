
# Codigo 7: Descomposición de una serie temporal
# TIPO DE MODELO INCLUIRíA COMPONENTE TENDENCIAL Y ESTACIONAL

# VARIAS LIBRERIAS
# Un par de paquetes para cargar nuestros ejemplos sencillos

#install.packages("fpp3")
#install.packages("Ecdat")
#install.packages("forecast")

library(fpp3)
library(Ecdat)
library(forecast)

############### DATOS #################

data(ausbeer)
     # LOS DATOS SON TRIMESTRALES
data(AirPassengers)
     # LOS DATOS SON MENSUALES

par(mfrow=c(2,1))
plot(ausbeer)
plot(AirPassengers)

# podemos ver un poco el componente estacional
par(mfrow=c(2,1))
seasonplot(ausbeer)
seasonplot(AirPassengers)


#######################################
####### REMOVIENDO TENDECIAS ##########
#######################################

# SUAVIZANDO LAS SERIES CON PROMEDIO MÓVIL
# ORDER: Orden del smoother 
# Centre: If TRUE, then the moving average is centred for even orders.

trend_beer = ma(ts_beer, order = 4, centre = T)   
trend_air = ma(ts_air, order = 12, centre = T)

par(mfrow=c(2,2))
plot(as.ts(ts_beer)) 
lines(trend_beer)
plot(as.ts(ts_air)) 
lines(trend_air)
plot(as.ts(trend_beer))
plot(as.ts(trend_air))


#######################################
##### FUNCIONES AUTOMáTICAS ###########
#######################################
#graphics.off()

# USANDO DECOMPOSE( ) 

ts_beer = ts(ts_beer, frequency = 4)
ts_air = ts(ts_air, frequency = 12)

decompose_beer = decompose(ts_beer, "additive")
decompose_air = decompose(ts_air, "multiplicative")

plot(decompose_beer)
plot(decompose_air)

# GR?FICAS INDIVIDUALES
# plot(as.ts(decompose_beer$trend))
# plot(as.ts(decompose_air$trend))
# plot(as.ts(decompose_beer$seasonal))
# plot(as.ts(decompose_air$seasonal))
# plot(as.ts(decompose_beer$random))
# plot(as.ts(decompose_air$random))

# USANDO STL( )

# EN EL CÓDIGO ANTERIOR HABÍAMOS USADO STL

stl_beer = stl(ts_beer, "periodic")
plot(stl_beer)

# GR?FICAS INDIVIDUALES
# seasonal_stl_beer<- stl_beer$time.series[,1]
# plot(as.ts(seasonal_stl_beer))
# trend_stl_beer<- stl_beer$time.series[,2]
# plot(trend_stl_beer)
# random_stl_beer<- stl_beer$time.series[,3]
# plot(random_stl_beer)













