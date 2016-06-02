#install.packages('WDI')
library(WDI)
library(data.table)
library(ggplot2)

WDIsearch("gni")

#Grab GNI per capita data for Chile, Hungary and Uruguay
#NY.GNP.PCAP.CD = 
#  GNI per capita, Atlas method (current US$)


paises <- c('AR','CL','UY','BR')
paises <- "all"
desde <- 1990
hasta <- 2016
  

dat = WDI(indicator='NY.GNP.PCAP.CD' , 
          country= paises , 
          start= desde , 
          end=hasta
          )

dt_datos <- as.data.table(dat)

# saco los registros nulos
dt_datos <- dt_datos[NY.GNP.PCAP.CD != 'NA']
setkey(dt_datos,year)


# grafico con todos los paises
g <- ggplot(data = dt_datos , aes(x = year, y = NY.GNP.PCAP.CD, color = iso2c))
g + 
  geom_line() + 
  xlab('Year') + 
  ylab('GDI per capita (Atlas Method USD)') + 
  labs(title = "GNI Per Capita ($USD Atlas Method)")


# los mejores 
# dt_datos[NY.GNP.PCAP.CD >= 70000 & year >= 2014]

### Media movil para AR
dt_datosAR <- dt_datos[iso2c == 'AR', c("year","iso2c" , "NY.GNP.PCAP.CD"), with = FALSE]

num_anios <- as.numeric( seq(from = desde , to = hasta-2))

## funcion para la media movil
media_movil <- function(argData, argMuestras){
  dt_datos.MA <- filter(argData , rep(1/argMuestras,argMuestras) ,circular = TRUE )
  num_MA <- as.data.table(dt_datos.MA) 
  num_MA <- as.numeric(as.matrix(num_MA))
  
  dt_datos.MA <- data.table( 
    year = num_anios, 
    iso2c = paste("MA",as.character(argMuestras),sep = "") ,
    NY.GNP.PCAP.CD = num_MA
  )
  return(dt_datos.MA)
}

## media movil con ventana de 3 y 5
dt_datosAR.MA3 <- media_movil(argData = dt_datosAR$NY.GNP.PCAP.CD, argMuestras = 3)
dt_datosAR.MA5 <- media_movil(argData = dt_datosAR$NY.GNP.PCAP.CD, argMuestras = 5)

##junto los datos originales con las medias para luego graficarlas
l = list(dt_datosAR , dt_datosAR.MA3 , dt_datosAR.MA5)
datos_grafico <- rbindlist(l)

# grafico de original con medias moviles
g <- ggplot(data = datos_grafico , aes(x = year, y = NY.GNP.PCAP.CD, color = iso2c))
g + 
  geom_line() + 
  xlab('Year') + 
  ylab('GDI per capita (Atlas Method USD)') + 
  labs(title = "GNI Per Capita ($USD Atlas Method)")




### correlacion entre dos series de tiempo

corr_MA3_MA5 = convolve(dt_datosAR.MA3$NY.GNP.PCAP.CD , dt_datosAR.MA5$NY.GNP.PCAP.CD , conj = TRUE)
dt_datos.CORR <- data.table( 
  year = num_anios, 
  iso2c = "CORR_AR",
  NY.GNP.PCAP.CD = corr_MA3_MA5
)
l = list(datos_grafico , dt_datos.CORR)
datos_grafico <- rbindlist(l)


g <- ggplot(subset(datos_grafico,iso2c=="CORR_AR"),aes(x = year, y = NY.GNP.PCAP.CD,dt_datos.CORR,  color = iso2c))    
g + 
  geom_line() + 
  xlab('Year') + 
  ylab('GDI per capita (Atlas Method USD)') + 
  labs(title = "GNI Per Capita ($USD Atlas Method)")



### correlacion cruzada
ccf(dt_datosAR.MA3$NY.GNP.PCAP.CD , dt_datosAR.MA5$NY.GNP.PCAP.CD)







## FIN










dt_datosUY.MA3 <- as.data.table(dt_datosUY.MA3)
dt_datosBR.MA3 <- as.data.table(dt_datosBR.MA3)

datos_grafico <- cbind(dt_datosAR.MA3,
                       dt_datosUY.MA3,
                       dt_datosBR.MA3
                       )

colnames(datos_grafico) <- c("AR","UY","BR")

#g <- ggplot(data = datos_grafico , aes(x = year, y = NY.GNP.PCAP.CD, color = iso2c))
g <- ggplot(data = datos_grafico)
g <- ggplot(data = as.data.frame(dt_datosBR.MA3) )
g + 
  geom_line() + 
  xlab('Year') + 
  ylab('GDI per capita (Atlas Method USD)') + 
  labs(title = "GNI Per Capita ($USD Atlas Method)")



setwd("H:\\DM\\71_ST\\proyectos\\WorlBank")

write.csv(dat, "data.csv")

data2 <- WDI(country="all", indicator=c("AG.AGR.TRAC.NO","TM.TAX.TCOM.BC.ZS"),
    start=2000, end=2015)
write.csv(data2, "data2.csv")

data3 <- WDI(country=c("AR","BR","CL","UY"), indicator="NY.GNS.ICTR.GN.ZS", start=2000, end=2015,
    extra=TRUE, cache=NULL)
write.csv(data3, "data3.csv")

