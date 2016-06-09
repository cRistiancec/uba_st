
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

#write.csv(dt_datos_escalados, "dataset_all_escalados.csv")
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


########################################################
# CORRELACIONES
########################################################


#paises <- dt_datos[,iso2c,by=iso2c]$iso2c
paises <- c("AR","BR","CL")
combinatorias <- t(combn(paises, 2))
combinatorias <- lapply(1:nrow(combinatorias), function(i) combinatorias[i,])
num_anios <- as.numeric( dt_datos_escalados[,year,by=year]$year)


########################################
### DATOS Originales (SIN escalar)
########################################

#datos_grafico <- dt_datos_escalados[iso2c=="AR"|iso2c=="BR"|iso2c=="CL"]
datos_grafico <- NULL

for (comb in combinatorias) {
  print(comb)
  convolucion = convolve(dt_datos_escalados[iso2c==comb[1],value] , 
                         dt_datos_escalados[iso2c==comb[2],value] , 
                         conj = TRUE)
  
  dt_datos.conv <- data.table( 
    year = num_anios, 
    iso2c = paste("CORR" , comb[1] , comb[2] , sep = "_"),
    value = convolucion
  )
  
  l = list(datos_grafico , dt_datos.conv)
  datos_grafico <- rbindlist(l)
  
}

g <- ggplot(datos_grafico, aes(x = year, y = value,dt_datos.CORR,  color = iso2c))    
g + 
  geom_line() + 
  xlab('Year') + 
  ylab('GDI per capita (Atlas Method USD)') + 
  labs(title = "GNI Per Capita ($USD Atlas Method)")




########################################
### DATOS escalados
########################################

#datos_grafico <- dt_datos_escalados[iso2c=="AR"|iso2c=="BR"|iso2c=="CL"]
datos_grafico <- NULL

for (comb in combinatorias) {
  print(comb)
  convolucion = convolve(dt_datos_escalados[iso2c==comb[1],escalado] , 
                         dt_datos_escalados[iso2c==comb[2],escalado] , 
                         conj = TRUE)
  
  dt_datos.conv <- data.table( 
    year = num_anios, 
    iso2c = paste("CORR" , comb[1] , comb[2] , sep = "_"),
    escalado = convolucion
  )
  
  l = list(datos_grafico , dt_datos.conv)
  datos_grafico <- rbindlist(l)
  
}

g <- ggplot(datos_grafico, aes(x = year, y = escalado,dt_datos.CORR,  color = iso2c))    
g + 
  geom_line() + 
  xlab('Year') + 
  ylab('GDI per capita (Atlas Method USD)') + 
  labs(title = "GNI Per Capita ($USD Atlas Method)")


######################################################################
### correlacion cruzada
######################################################################


########################################
### DATOS Originales (SIN escalar)
########################################

par(mfrow=c(3,2))
for (comb in combinatorias) {
  ccf(dt_datos_escalados[iso2c==comb[1],value] , 
      dt_datos_escalados[iso2c==comb[2],value] ,
      ylab = "cross-correlation",
      main = paste ("ccf",comb[1],comb[2])
  )
}

### correlacion cruzada

for (comb in combinatorias) {
  print(comb)
  print( mean(ccf(dt_datos_escalados[iso2c==comb[1],value] , 
                  dt_datos_escalados[iso2c==comb[2],value] ,
                  plot=FALSE      )$acf))
}


########################################
### DATOS escalados
########################################

par(mfrow=c(3,2))
for (comb in combinatorias) {
  ccf(dt_datos_escalados[iso2c==comb[1],escalado] , 
      dt_datos_escalados[iso2c==comb[2],escalado] ,
      ylab = "cross-correlation",
      main = paste ("ccf",comb[1],comb[2])
  )
}

### correlacion cruzada

for (comb in combinatorias) {
  print(comb)
  print( mean(ccf(dt_datos_escalados[iso2c==comb[1],escalado] , 
                  dt_datos_escalados[iso2c==comb[2],escalado] ,
                  plot=FALSE      )$acf))
}


## FIN

