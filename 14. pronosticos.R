# CODIGO 12: Pronóstico

# PRONOSTICANDO ARMA

# AR 1

ARsim = arima.sim(list(order=c(1,0,0), ar=0.5), n=150)
preAR = predict(arima(ARsim, order=c(1,0,0)), 10)

ts.plot(ARsim, preAR$pred, col=1:2,main="Pronosticando un AR(1)")
Upp = preAR$pred+1.96*preAR$se; Low = preAR$pred-1.96*preAR$se
xx = c(time(Upp), rev(time(Upp))); yy = c(Low, rev(Upp))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
lines(preAR$pred, type="p", col=2)

# AR 5 PRONÓSTICO A 500 PERIODOS 
AR5sim = arima.sim(list(order=c(5,0,0), ar=c(1.06,-0.44,-0.33,0.87,-0.41)), n=500)
preAR = predict(arima(AR5sim, order=c(5,0,0)), 100)
ts.plot(AR5sim, preAR$pred, col=1:2,main="Pronosticando un AR(5)")
Upp = preAR$pred+1.96*preAR$se; Low = preAR$pred-1.96*preAR$se
xx = c(time(Upp), rev(time(Upp))); yy = c(Low, rev(Upp))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
lines(preAR$pred, type="p", col=2)

# GRAFICA DINÁMICA
set.seed(2018)
AR5sim = arima.sim(list(order=c(0,0,5), ar=c(1.06,-0.44,-0.33,0.87,-0.41)), n=250)
preAR<-0 
for (h in 1:50) {
  preAR = predict(arima(AR5sim, order=c(0,0,5)), h)
  ts.plot(AR5sim, preAR$pred, col=1:2,main="Pronosticando un AR(5)")
  Upp = preAR$pred+1.96*preAR$se; Low = preAR$pred-1.96*preAR$se
  xx = c(time(Upp), rev(time(Upp))); yy = c(Low, rev(Upp))
  polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
  lines(preAR$pred, type="p", col=2)
Sys.sleep(0.05)  
}

####################################################################

# MA 5 PRONÓSTICOS A 10 PERIODOS 

MA5sim = arima.sim(list(order=c(0,0,5), ma=c(1.06,-0.44,-0.33,0.87,-0.41)), n=50)
preMA = predict(arima(MA5sim, order=c(0,0,5)), 10)
ts.plot(MA5sim, preMA$pred, col=1:2,main="Pronosticando un MA(5)")
Upp = preMA$pred+1.96*preMA$se; Low = preMA$pred-1.96*preMA$se
xx = c(time(Upp), rev(time(Upp))); yy = c(Low, rev(Upp))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
lines(preMA$pred, type="p", col=2)



####################################################################

# ARMA(2,2) PRONÓSTICOS A 10 PERIODOS 

ARMA22sim = arima.sim(list(order=c(2,0,2), ar=c(0.25,0.5), ma=c(0.25,0.5)), n=100)
preARMA22 = predict(arima(ARMA22sim, order=c(2,0,2)), 10)
ts.plot(ARMA22sim, preARMA22$pred, col=1:2,main="Pronosticando un ARMA(2,2)")
Upp = preARMA22$pred+1.96*preARMA22$se; Low = preARMA22$pred-1.96*preARMA22$se
xx = c(time(Upp), rev(time(Upp))); yy = c(Low, rev(Upp))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
lines(preARMA22$pred, type="p", col=2)


####################################################################
# DINÁMICA A LARGO PLAZO
# ARMA(2,2) PRONÓSTICOS A 10 PERIODOS 
set.seed(2018)
ARMA22sim = arima.sim(list(order=c(2,0,2), ar=c(0.25,0.5), ma=c(0.25,0.5)), n=100)
preARMA22<-0;preMA2<-0;preAR2<-0;
for (h in 1:50){
  preARMA22 = predict(arima(ARMA22sim, order=c(2,0,2)), h)  
  preMA2 = predict(arima(ARMA22sim, order=c(2,0,0)), h)
  preAR2 = predict(arima(ARMA22sim, order=c(0,0,2)), h)
  ts.plot(ARMA22sim, preARMA22$pred,preAR2$pred,preMA2$pred, col=1:4,main="Pronostico a largo plazo")
  legend("topright",legend=c("ARMA22","AR2","MA2"),col=c("red","blue","green"), lty=1, cex=1)
  Sys.sleep(0.05)
}



###############################################
#############BACKCASTING #####################


set.seed(123456)
ARMA = arima.sim(list(order = c(1,0,1), ar =.95, ma=.5), n = 150)
ARMArev = rev(ARMA) # REVERTIMOS NUESTROS DATOS
pxr = predict(arima(ARMArev, order=c(1,0,1)), 30) # PREDECIMOS LOS DATOS REVERTIDOS
pxrp = rev(pxr$pred) # REORDENAMOS LAS PREDICCIONES (ESTO PARA GRAFICAR)
pxrse = rev(pxr$se) # REORDENAMOS LAS SEs
nx = ts(c(pxrp, ARMA), start=-29) # unimos los backcasts a nuestros datos
plot(nx, ylab="", main='Backcasting en un proceso ARMA(1,1)')
U = nx[1:30] + pxrse; L = nx[1:30] - pxrse
xx = c(-29:0, 0:-29); yy = c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(0.6, alpha = 0.2))
lines(-29:0, nx[1:30], col=2, type='o')




