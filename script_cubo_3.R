#Set directory
#setwd("/home/sergio/Escritorio/TimeSeries/TP/git/uba_st/")
setwd("/run/media/jp/LIVE/DM/71_ST/proyectos/WorlBank/git/uba_st")


tab<-read.csv("dataset_all_escalados.csv",stringsAsFactors=FALSE)

### Cubo de correlaciones

paises<-unique(tab$iso2c) 
length(paises)

#Sample
ps <- paises[1:50]
#ps<-c("BR","CL","EU","CN")
#longitud de puntos

temp2 <- subset(tab, tab$iso2c%in%paises[2])
temp1 <- temp2

unique(temp1$year)
range <- seq(1980, 2014, 5)
subset(temp1,year>=range[1] & year<=range[2])
subset(temp2,year>=range[1] & year<=range[2])



lis<-list()
temp1<-subset(tab, tab$iso2c%in%c("AR"))
for (j in 1:length(ps))
{
	dat <- data.frame()
	temp2 <- subset(tab, tab$iso2c%in%ps[j])
	for (i in 1:(length(range)-1))
	{
		for (k in 1:(length(range)-1) )
		{
			dat[i,k] <- cor(subset(temp1,year>=range[i] & year<=range[i+1])$escalado,
				subset(temp2,year>=range[k] & year<=range[k+1])$escalado)
		}
	}
	
	colnames(dat) <- rownames(dat)<-range[1:length(range)-1]
	print(ps[j])
	print(dat)
	lis[[j]] <- dat
}

lis

install.packages('DescTools')
library('DescTools')

anios <- c(1980,1985,1990,1995,2000,2005)
combinatorias <- CombSet(anios, 2, TRUE, TRUE)
dat

datos <- t(dat)
colnames(datos) <- NULL
rownames(datos) <- NULL
datos <- unlist(datos)



####
# Instalar en el sistema operativo : 
# mesa-libGL-devel 
# mesa-libGLU-devel

#install.packages('rgl')
#install.packages('magrittr')
library('rgl')
library('magrittr')


getColor <- function(paraValor){
  #1 AZUL
  #-1 ROJO
  if(paraValor > 0){
    #valorAzul <- paraValor
    valorAzul <- 1
    valorRojo <- 0
  }
  else{
    valorAzul <- 0
    #valorRojo <- abs(paraValor)
    valorRojo <- 1
  }
  
  return( rgb(valorRojo,0,valorAzul) )
}



dat
datos
x <- combinatorias[,1]
y <- combinatorias[,2]
# x <- c(1980,1985,1990,1995,2000,2005)
# y <- c(1980,1985,1990,1995,2000,2005)
# x <- 1
# y <- 2

z <- 1

colores <- lapply(datos, getColor)

rgl.open() # Open a new RGL device
rgl.bg(color = "white") # Setup the background color
#rgl.points(x, y, z, color ="lightgray") # Scatter plot
#rgl_init()
rgl.spheres(x
            ,y
            ,z
            ,r = abs(datos)
            ,color = colores
            ) 

#rgl_add_axes(x, y, z)
# Show tick marks
axis3d('x', pos=c( NA, 1975, 0 ), col = "darkgrey")
axis3d('y', pos=c( 1975, NA, 0 ), col = "darkgrey")
axis3d('z', pos=c( 0, 0, NA ), col = "darkgrey")


# Closes the current device
rgl.close()




