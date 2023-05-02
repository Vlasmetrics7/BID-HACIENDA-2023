# CODIGO 1: Penn World Tables 

# incluyen cálculos de la participación del trabajo en los ingresos a lo largo 
# del tiempo para 182 países

# Ejemplo de crecimiento económico:
# Si la especificación Cobb-Douglas usual es correcta, deberíamos esperar que la 
# participación laboral en el ingreso total sea bastante constante y 
# suponga que el ingreso restante se destina al capital.

# install.packages('pwt9')
library(pwt9)
df <- pwt9.0
df$country <- as.character(df$country)
df$isocode <- as.character(df$isocode)
# labsh Share of labour compensation in GDP at
# current national prices.
mex_data <- df[df$isocode == "MEX", ]


plot(mex_data$year, 100 * mex_data$labsh, 
     ylim = c(0,100), typ = "l", ylab = "%", xlab = "", main = "MEX")
lines(mex_data$year, 100 * (1 - mex_data$labsh),col = 2)
legend("topright", legend = c("Labor Share of Income",
                              "Capital Share of Income"), lty = 1, col = 1:2,cex = 0.75)

# A diferencia del caso de Estados Unidos
usa_data <- df[df$isocode == "USA", ]
plot(usa_data$year, 100 * usa_data$labsh, ylim = c(0,100), typ = "l", 
     ylab = "%", xlab = "", main = "USA")
lines(usa_data$year, 100 * (1 - usa_data$labsh),col = 2)
legend("topright", legend = c("Labor Share of Income","Capital Share of Income"),
       lty = 1, col = 1:2,cex = 0.75)

# Comparativo México y EUA
c(mean(mex_data$labsh),mean(usa_data$labsh))

#Como podemos ver en el gráfico anterior, la participación laboral en los 
# ingresos está bastante cerca de alrededor de 60% del PIB de EE. UU. y 48% de México.
# Sin embargo, ¿qué tal será en el resto del mundo? 
# Calculemos la participación laboral promedio y la participación de capital 
# promedio a lo largo del tiempo en nuestra muestra de países.


# Run the line below to install on your
# install.packages('rworldmap')
library(rworldmap)

country_labor_share_mean <- tapply(df$labsh, df$isocode,function(x) mean(x, na.rm = T))
country_labor_share_mean

# En vez de tener un listado enorme de países con numeritos... hagamos las cosas algo más bellas

labor_share_mean <- data.frame(country = names(country_labor_share_mean),
                               labor_share = as.numeric(country_labor_share_mean),
                               capital_share = 1 - as.numeric(country_labor_share_mean))
labor_share_mean <- na.omit(labor_share_mean)

country_map <- joinCountryData2Map(labor_share_mean,
                                   joinCode = "ISO3", nameJoinColumn = "country")

# ?joinCountryData2Map
map_data_labor <- mapCountryData(country_map,
                                 nameColumnToPlot = "labor_share", mapTitle = "Labor Share of Income",
                                 colourPalette = "heat", addLegend = FALSE)
# ?mapCountryData
do.call(addMapLegend, c(map_data_labor, legendLabels = "all",
                        legendWidth = 0.5))

map_data_capital <- mapCountryData(country_map,
                                   nameColumnToPlot = "capital_share", mapTitle = "Capital Share of Income",
                                   colourPalette = "heat", addLegend = FALSE)
do.call(addMapLegend, c(map_data_capital, legendLabels = "all",
                        legendWidth = 0.5))



#######################################################################
#######################################################################
#######################################################################

# EJEMPLO 2: Crecimiento del PIB >>>  log(GDP)

country_gdp_mean <- tapply(df$rgdpna, df$isocode,
                           function(x) mean((log(x)), na.rm = T))
gdp_share_mean <- data.frame(country = names(country_gdp_mean),
                             gdp_growth = as.numeric(country_gdp_mean))
gdp_share_mean <- na.omit(gdp_share_mean)
country_map <- joinCountryData2Map(gdp_share_mean,
                                   joinCode = "ISO3", nameJoinColumn = "country")

# ?joinCountryData2Map
map_data_gdp <- mapCountryData(country_map, nameColumnToPlot = "gdp_growth",
                               mapTitle = "Avg. Ln. GDP", colourPalette = "heat",
                               addLegend = FALSE, numCats = 10, catMethod = "fixedWidth")
# ?mapCountryData
do.call(addMapLegend, c(map_data_gdp, legendLabels = "all",
                        legendWidth = 0.5))


