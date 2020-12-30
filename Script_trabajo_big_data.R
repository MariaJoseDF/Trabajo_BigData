
##Instalando los paquetes 
install.packages(rvest)
install.packages(gdata)
install.packages(dylyr)


##Corriendo los paquetes
library(rvest)
library(gdata)
library(dplyr)


#Lectura de pagina con la informacion requerida
broker <- read_html("https://www.brokeronline.es/criptomonedas/ranking/")

#Creando variable que contiene la informacion a obtener
contenedoranking <- html_nodes(broker,xpath = "//*[@id=\"post-13673\"]/div")

#Obetiendo divs con la informacion a extraer
InfoRanking <- html_nodes(contenedoranking, css = ".td-width-content")

#Creando variables booleanas para la extraccion de la informacion
Lanzamiento <- FALSE
CapMercado <- FALSE
CantMax <- FALSE

#Creando variables para el almacenamiento de la informacion extraida
LaunchDate <- c()
MarketShare <- c()
Quantity <- c()

#Obteniendo nombres de criptomonedas
N <- html_nodes(contenedoranking, css = ".col-xs-12.text-center")
##Transformando a texto los nombres extraidos
TxtN <- html_text(N)
##Limpiando la información
TxtN <- gsub("\n","",TxtN)
TxtN <- gsub("\t","", TxtN)

##Eliminando espacios vacios dentro del vector de nombres por medio de un Loop
for (i in 1:length(TxtN)) {
  if(TxtN[i] == ""){
    TxtN <- TxtN[-i]
  }
}

#Creación de Loop para la extracción de la información
for (i in InfoRanking) {
  L <- html_nodes(i, css = ".col-xs-12 > p")
  for (a in L) {
    Txta <- html_text(a)
    if(Lanzamiento){
      print(Txta)
      LaunchDate <- c(LaunchDate,Txta)
      Lanzamiento <- FALSE
    }
    if(Txta == "Lanzamiento"){
      Lanzamiento <- TRUE
    }
  }
  for (b in L) {
    Txtb <- html_text(b)
    if(CapMercado){
      print(Txtb)
      MarketShare <- c(MarketShare,Txtb)
      CapMercado <- FALSE
    }
    if(Txtb == "Capacidad de mercado"){
      CapMercado <- TRUE
    }
  }
  for (c in L) {
    Txtc <- html_text(c)
    if(CantMax){
      print(Txtc)
      Quantity <- c(Quantity,Txtc)
      CantMax <- FALSE
    }
    if(Txtc == "Cantidad máxima"){
      CantMax <- TRUE
    }
  }
}

#Creacion de data frame con la informacion extraída
Criptomonedas <- data.frame("Criptomoneda" = TxtN,"Lanzamiento" = LaunchDate, "Capacidad de mercado" = MarketShare, "Cantidad maxima" = Quantity)

#Cambiando las variables capacacidad de mercado por numeros enteros
Criptomonedas[,3]<-gsub("En torno al","",Criptomonedas[,3])
Criptomonedas[,3]<-gsub("Menos del","",Criptomonedas[,3])
Criptomonedas [,3]<-gsub("[%]","",Criptomonedas[,3])

#Cambiando nombre de la tabla Capacodad.de.mercado por Capacidad.de.mercado (%)
Criptomonedas<-rename(Criptomonedas,"Capacidad.de.mercado (%)"="Capacidad.de.mercado")

#Cambiando numeros a as.numeric
Criptomonedas[["Capacidad.de.mercado (%)"]]<-as.numeric(Criptomonedas[["Capacidad.de.mercado (%)"]])
typeof(Criptomonedas[["Capacidad.de.mercado (%)"]])

#Cambiar las variable Lanzamiento por solo año
Criptomonedas[,2]<-gsub("03.01.2009","2009",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("30.07.2015","2015",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("26.05.2020","2020",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("7.10.2011","2011",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("27.06.2017","2017",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("27.07.2017","2017",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("Noviembre 2014","2014",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("Septiembre 2017","2017",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("29.09.2017","2017",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("Planeado en 2020","2020",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("26.06.2017","2017",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("Abril 2014","2014",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("09.09.2017","2017",Criptomonedas[,2])

#Cambiando nombre de la tabla lanzamiento por lanzamiento (año)
Criptomonedas<-rename(Criptomonedas,"Lanzamiento (año)"="Lanzamiento")


#Cambiando los billones y millones por millones
Criptomonedas[,4]<-gsub("21 millones BTC","21",Criptomonedas[,4])
Criptomonedas[,4]<-gsub("Sin límites","N/A",Criptomonedas[,4])
Criptomonedas[,4]<-gsub("100 millones XRP","100",Criptomonedas[,4])
Criptomonedas[,4]<-gsub("1.000 Millones DOTs","1.000",Criptomonedas[,4])
Criptomonedas[,4]<-gsub("84 millones LTC","84",Criptomonedas[,4])
Criptomonedas[,4]<-gsub("200 millones BNB","200",Criptomonedas[,4])
Criptomonedas[,4]<-gsub("21.000.000 BCH","21",Criptomonedas[,4])
Criptomonedas[,4]<-gsub("2.580.109.970 USDt","2.580",Criptomonedas[,4])
Criptomonedas[,4]<-gsub("1 Billón LINKs","1000",Criptomonedas[,4])
Criptomonedas[,4]<-gsub("45.000 millones ADA","45.000",Criptomonedas[,4])
Criptomonedas[,4]<-gsub("Sin datos","0",Criptomonedas[,4])
Criptomonedas[,4]<-gsub("1 billón EOS","1.000",Criptomonedas[,4])
Criptomonedas[,4]<-gsub("18,4 millones XMRs","18.4",Criptomonedas[,4])
Criptomonedas[,4]<-gsub("100.000.000.000 TRX","100.000",Criptomonedas[,4])

#Cambiar Cantidad.maxima por Cantidad.maxima ($) (M)
Criptomonedas<-rename(Criptomonedas,"Cantidad.maxima ($) (M)"="Cantidad.maxima")
#Cambiando numeros como as.nemeric
Criptomonedas[["Cantidad.maxima ($) (M)"]]<-as.numeric(Criptomonedas[["Cantidad.maxima ($) (M)"]])
typeof(Criptomonedas[["Cantidad.maxima ($) (M)"]])






