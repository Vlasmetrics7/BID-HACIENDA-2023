# código 16: Rolling windows


# Generamos un proceso ARMA(1,1) de longitud T
T =150 # MUESTRA TOTAL 
T0=100 # ROLLING WINDOW

set.seed(2500); 

x1=arima.sim(model=list(ar1=0.5,ma1=0.2),n=T)

# Estimemos 3 posibles modelos candidatos:
# ARMA(1,1), ARMA(2,1), ARMA(0,1) usando solamente la muestra de tama?o T0 (rolling windows) 

# Tenemos que calcular el MSPE del pron?stico a 1-paso

e1=e2=e3=rep(NA,T)
Pred.Modelo1<-0
Pred.se.Modelo1<-0
for(Rwind in T0:(T-1)){
  range=c((Rwind-T0+1):Rwind)
  
  # ROLLING WINDOWS MODELO 1  
  Mod1  =arima(x1[range],order=c(1,0,1))
  Pred.Mod1   =as.numeric(predict(object=Mod1,n.ahead=1)$pred)
  Pred.se.Mod1=as.numeric(predict(object=Mod1,n.ahead=1)$se)
  e1[Rwind]=Pred.Mod1-x1[Rwind+1]
  Pred.Modelo1[Rwind]<-Pred.Mod1
  Pred.se.Modelo1[Rwind]<-Pred.se.Mod1
  
}

# GR?FICA ROLLING WINDOWS

Upp = Pred.Modelo1+1.96*Pred.se.Modelo1 
Low =  Pred.Modelo1-1.96*Pred.se.Modelo1

plot.ts(x1[2:150],main="Pronosticando mediante el m?todo de rolling windows ",
        xlab="",ylab="",)
lines(Pred.Modelo1[2:150],col="red",lwd=3,lty=4)
xx = c(time(Upp), rev(time(Upp))); yy = c(Low, rev(Upp))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))

