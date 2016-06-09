
#Sys.setenv(http_proxy = 'http://10.1.103.240:3128')
#Sys.setenv(https_proxy = 'http://10.1.103.240:3128')

#install.packages('RJSONIO')
#install.packages('WDI')

setwd("/home/sergio/Escritorio/TimeSeries/TP/git/uba_st/")
setwd("/run/media/jp/LIVE/DM/71_ST/proyectos/WorlBank/git/uba_st")


library(data.table)
library(ggplot2)

dat <- read.csv("dataset_all.csv")

dt_datos <- as.data.table(dat)

# saco los registros nulos
dt_datos <- dt_datos[NY.GDP.PCAP.KD != 'NA']
setkey(dt_datos,year)

fun_escalado <- function(x) {
  z=(x-mean(x,na.rm=TRUE))/sd(x,na.rm = TRUE)
  return(z) 
}


dt_datos_escalados <- dt_datos[,list(value=NY.GDP.PCAP.KD, escalado=fun_escalado(NY.GDP.PCAP.KD), year=year),by="iso2c"]
#head(dt_datos[order(iso2c)], 30)
#head(dt_datos_escalados[order(iso2c)], 40)


# grafico con todos los paises
#datos_grafico <- dt_datos_escalados[iso2c=="AR"]
datos_grafico <- dt_datos_escalados[iso2c=="AR"|iso2c=="BR"|iso2c=="CL"]

myplot1 <- ggplot(data = datos_grafico , aes(x = year, y = value, color = iso2c))+geom_line()
myplot2 <- ggplot(data = datos_grafico , aes(x = year, y = escalado, color = iso2c))+geom_line()

#install.packages("gridExtra")
library("gridExtra")
grid.arrange(myplot1, myplot2, ncol=2)


