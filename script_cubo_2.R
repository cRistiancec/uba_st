#Set directory
#setwd("/home/sergio/Escritorio/TimeSeries/TP/git/uba_st/")
setwd("/run/media/jp/LIVE/DM/71_ST/proyectos/WorlBank/git/uba_st")


tab<-read.csv("dataset_all_escalados.csv",stringsAsFactors=FALSE)

### Cubo de correlaciones

paises<-unique(tab$iso2c); length(paises)

#Sample
ps<-paises[1:50]
#ps<-c("BR","CL","EU","CN")
#longitud de puntos

temp2<-subset(tab, tab$iso2c%in%paises[2])

unique(temp1$year)
range<-seq(1980, 2014, 5)
subset(temp1,year>=range[1] & year<=range[2])
subset(temp2,year>=range[1] & year<=range[2])



lis<-list()
temp1<-subset(tab, tab$iso2c%in%c("AR"))
for (j in 1:length(ps))
{
	dat<-data.frame()
	temp2<-subset(tab, tab$iso2c%in%ps[j])
	for (i in 1:(length(range)-1))
	{
		for (k in 1:(length(range)-1) )
		{
			dat[i,k]<-cor(subset(temp1,year>=range[i] & year<=range[i+1])$escalado,
				subset(temp2,year>=range[k] & year<=range[k+1])$escalado)
		}
	}
	
	colnames(dat)<-rownames(dat)<-range[1:length(range)-1]
	print(ps[j])
	print(dat)
	lis[[j]]<-dat
}
colnames(dat)<-rownames(dat)<-range[1:length(range)-1]

