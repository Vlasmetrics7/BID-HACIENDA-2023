
# CODIGO 3: MODELO DE SOLOW SWAN

library(pwt9)
df <- pwt9.0
df$country <- as.character(df$country)
df$isocode <- as.character(df$isocode)

# Tenemos que definir las tasas de ahorro para poder filtrar a los países cuyas
# tasas sean negativas

# Recuerde que el ahorro es, en esencia, la producción total menos el consumo
df$Savings <- (df$rgdpna - df$rconna)/df$rgdpna

global_savings_avg <- tapply(df$Savings, df$isocode,function(x) mean(x, na.rm = T))
global_gdp_growth_avg <- tapply(df$rgdpna/df$pop,df$isocode, 
                                function(x) mean(diff(log(x)),na.rm = T))


positive_savings_countries <- names(global_savings_avg)[global_savings_avg >0]
global_gdp_growth_avg <- global_gdp_growth_avg[global_savings_avg > 0]
global_savings_avg <- global_savings_avg[global_savings_avg > 0]


# Ahora sí, veamos el modelo de Solow-Swan

global_pop_growth_avg <- tapply(df$pop, df$isocode,function(x) mean(diff(log(x)), na.rm = T))

global_pop_growth_avg <- global_pop_growth_avg[names(global_pop_growth_avg) %in%
                                                 positive_savings_countries]

plot(global_pop_growth_avg * 100, global_gdp_growth_avg * 100, 
     xlab = "Avg. Population Growth %", ylab = "Avg. GDP Growth per Capita",
     main = "1950-2014")
text(global_pop_growth_avg * 100, global_gdp_growth_avg *
       100, names(global_gdp_growth_avg), cex = 1,
     pos = 4)
points(100 * global_pop_growth_avg[names(global_pop_growth_avg) == "MEX"], 
       100 * global_gdp_growth_avg[names(global_pop_growth_avg) == "MEX"], 
       col = 4, pch = 19)


# MODELO DE REGRESIÓN LINEAL

mdl_pop <- lm(global_gdp_growth_avg ~ global_pop_growth_avg)
summary(mdl_pop)



mdl_pop_savings <- lm(global_gdp_growth_avg ~ global_pop_growth_avg + global_savings_avg)
summary(mdl_pop_savings)


# EMBELLEZCAMOS NUESTRAS GRÁFICAS DE REGRESIÓN
#install.packages("ggplot2")
library(ggplot2)
data<-  data.frame(global_gdp_growth_avg,global_pop_growth_avg,global_savings_avg)

reg_ss <- ggplot(data, aes(x = global_pop_growth_avg, y = global_gdp_growth_avg))

# veámos la primera regresión con intervalos de confianza al 99%
reg_ss +
  geom_point(colour = "red") +
  stat_smooth(method = lm, level = 0.99)

#VEÁMOS LOS RESIDUALES
data$predicted <- predict(mdl_pop)   # Guardamos los valores ajustados
data$residuals <- residuals(mdl_pop) # Guardamos los residuales


ggplot(data, aes(x = global_pop_growth_avg, y = global_gdp_growth_avg)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend =  global_pop_growth_avg, yend = predicted), alpha = .2) +
  geom_point(aes(color = residuals)) +  
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +  
  guides(scale = "none") +
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()




