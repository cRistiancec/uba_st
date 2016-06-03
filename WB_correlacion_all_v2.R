
#Sys.setenv(http_proxy = 'http://10.1.103.240:3128')
#Sys.setenv(https_proxy = 'http://10.1.103.240:3128')

#install.packages('RJSONIO')
#install.packages('WDI')
setwd("/home/sergio/Escritorio/TimeSeries/TP/git/uba_st/")

library(WDI)
library(data.table)
library(ggplot2)

#WDIsearch("gni")

#Grab GNI per capita data for Chile, Hungary and Uruguay
#NY.GDP.PCAP.KD = 
#  GNI per capita, Atlas method (current US$)


#filtro_paises <- c('AR','CL','UY','BR')  ## argentina, chile, uruguay, brasil
filtro_paises <- c('ES','UY','CN','US','AR','BR','CA')  ## argentina, uruguay, china, usa
#filtro_paises <- "all"
desde <- 1990
hasta <- 2016
  

dat = WDI(indicator='NY.GDP.PCAP.KD' , 
          country= filtro_paises , 
          start= desde , 
          end=hasta
          )

dt_datos <- as.data.table(dat)

dat<-as.data.frame(dt_datos)
dat$ID<-1:nrow(dt_datos)
library(doBy)
summaryBy(NY.GDP.PCAP.KD, year~iso2c, data=dat, FUN=c(scale))


# saco los registros nulos
dt_datos <- dt_datos[NY.GDP.PCAP.KD != 'NA']
setkey(dt_datos,year)


# grafico con todos los paises
g <- ggplot(data = dt_datos , aes(x = year, y = NY.GDP.PCAP.KD, color = iso2c))
g + 
  geom_line() + 
  xlab('Year') + 
  ylab('GDI per capita (Atlas Method USD)') + 
  labs(title = "GNI Per Capita ($USD Atlas Method)")


# los mejores 
# dt_datos[NY.GDP.PCAP.KD >= 70000 & year >= 2014]

### Media movil para AR
#dt_datosAR <- dt_datos[iso2c == 'AR', c("year","iso2c" , "NY.GDP.PCAP.KD"), with = FALSE]
dt_datos <- dt_datos[, c("year","iso2c" , "NY.GDP.PCAP.KD"), with = FALSE]

#num_anios <- as.numeric( seq(from = desde , to = hasta-2))
num_anios <- as.numeric( dt_datos[,year,by=year]$year)


## funcion para la media movil
media_movil <- function(argNombrePais, argData, argMuestras){
  dt_datos.MA <- filter(argData , rep(1/argMuestras,argMuestras) ,circular = TRUE )
  num_MA <- as.data.table(dt_datos.MA) 
  num_MA <- as.numeric(as.matrix(num_MA))
  
  dt_datos.MA <- data.table( 
    year = num_anios, 
    iso2c = paste(argNombrePais,"MA",as.character(argMuestras),sep = "") ,
    NY.GDP.PCAP.KD = num_MA
  )
  return(dt_datos.MA)
}

paises <- dt_datos[,iso2c,by=iso2c]$iso2c

datos_grafico <- dt_datos

## media movil con ventana de 3 y 5
for (pais in paises) {
  dt_datosPais <- dt_datos[iso2c == pais, c("year","iso2c" , "NY.GDP.PCAP.KD"), with = FALSE]
  print("dt_datosPais")
  print(dt_datosPais)
  
  dt_datosPais.MA3 <- media_movil(argNombrePais = pais,
                                  argData = dt_datosPais$NY.GDP.PCAP.KD, 
                                  argMuestras = 3)
  dt_datosPais.MA5 <- media_movil(argNombrePais = pais, 
                                  argData = dt_datosPais$NY.GDP.PCAP.KD, 
                                  argMuestras = 5)
  print("dt_datosPais.MA5")
  print(dt_datosPais.MA5)
  
  l <- list(datos_grafico , dt_datosPais.MA3 , dt_datosPais.MA5)
  datos_grafico <- rbindlist(l)
  print("datos_grafico")
  print(datos_grafico)
  
}

##junto los datos originales con las medias para luego graficarlas
#l = list(dt_datos , dt_datos.MA3 , dt_datos.MA5)
#datos_grafico <- rbindlist(l)

# grafico de original con medias moviles
g <- ggplot(data = datos_grafico , aes(x = year, y = NY.GDP.PCAP.KD, color = iso2c))
g + 
  geom_line() + 
  xlab('Year') + 
  ylab('GDI per capita (Atlas Method USD)') + 
  labs(title = "GNI Per Capita ($USD Atlas Method)")


######################################################################
### correlacion entre las series de tiempo
combinatorias <- t(combn(paises, 2))
combinatorias <- lapply(1:nrow(combinatorias), function(i) combinatorias[i,])
datos_grafico <- NULL
for (comb in combinatorias[1:1]) {
  print(comb)
  convolucion = convolve(dt_datos[iso2c==comb[1],NY.GDP.PCAP.KD] , 
                         dt_datos[iso2c==comb[2],NY.GDP.PCAP.KD] , 
                          conj = TRUE)
  
  dt_datos.conv <- data.table( 
    year = num_anios, 
    iso2c = paste("CORR" , comb[1] , comb[2] , sep = "_"),
    NY.GDP.PCAP.KD = convolucion
  )
  l = list(datos_grafico , dt_datos.conv)
  datos_grafico <- rbindlist(l)
  
}

#g <- ggplot(subset(datos_grafico,iso2c=="CORR_AR"),aes(x = year, y = NY.GDP.PCAP.KD,dt_datos.CORR,  color = iso2c))    
g <- ggplot(datos_grafico, aes(x = year, y = NY.GDP.PCAP.KD,dt_datos.CORR,  color = iso2c))    
g + 
  geom_line() + 
  xlab('Year') + 
  ylab('GDI per capita (Atlas Method USD)') + 
  labs(title = "GNI Per Capita ($USD Atlas Method)")


######################################################################
### correlacion cruzada
par(mfrow=c(3,2))
for (comb in combinatorias) {
  ccf(dt_datos[iso2c==comb[1],NY.GDP.PCAP.KD] , 
      dt_datos[iso2c==comb[2],NY.GDP.PCAP.KD] ,
      ylab = "cross-correlation",
      main = paste ("ccf",comb[1],comb[2])
      )
}

### correlacion cruzada

for (comb in combinatorias) {
print(comb)
print( mean(ccf(dt_datos[iso2c==comb[1],NY.GDP.PCAP.KD] , 
      dt_datos[iso2c==comb[2],NY.GDP.PCAP.KD] ,
      plot=FALSE      )$acf))
}

## FIN
