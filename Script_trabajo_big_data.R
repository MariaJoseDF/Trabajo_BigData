####################################### IG.COM ################################################################

#setwd("C:/Users/maria/OneDrive/Escritorio/ICI Cuarto año/Segundo semestre/Big data/Trabajo_BigData")

#Instalando paquetes
install.packages("rvest")
library("rvest")
install.packages("dplyr")
library("dplyr")
install.packages("gdata")
library("gdata")

#Lectura de pagina con la informacion requerida
ig <- read_html("https://www.ig.com/es/trading-de-criptomonedas/comparativa-criptomonedas#information-banner-dismiss")

#Extrayendo tabla comparativa de criptomonedas
Tabla_comparativa <- html_table(ig)[[2]]

#Cambiando filas por columnas
Tabla_comparativa <- as.data.frame(t(Tabla_comparativa))

#Renombrando columnas

Tabla_comparativa <- rename(Tabla_comparativa, "Criptomoneda" = V1, "Lanzamiento" = V2, "Cantidad en circulación (>M)" = V3,
                            "Oferta máxima (M)" = V4, "Ratio de minado/emisión" = V5, "Transacciones por segundo" = V6,
                            "Red" = V7, "Tiempo para un bloque (segundos)" = V8)

#Eliminando fila sobrante
Tabla_comparativa <- Tabla_comparativa[-1,]

#Creando vector con los nombres de las filas
Filas <- list(1,2,3,4,5,6,7,8)

#Reasignando los nombres de las filas
Tabla_comparativa <- data.frame(Tabla_comparativa, row.names = Filas)

#Estableciendo loop que reemplazara por NA las variables sin datos

for (i in 1:nrow(Tabla_comparativa)) {
  if(Tabla_comparativa[i,7] == "n/a"){
    Tabla_comparativa[i,7] <- NA
  }
}

#Transformando el tiempo de la columna 8 en minutos por segundos y a valores numericos
Tabla_comparativa[1,8] <- 10*60
Tabla_comparativa[2,8] <- 10*60
Tabla_comparativa[3,8] <- 15
Tabla_comparativa[4,8] <- 2.5 * 60
Tabla_comparativa[6,8] <- 0.5
Tabla_comparativa[7,8] <- 5
Tabla_comparativa[8,8] <- 15
Tabla_comparativa[5,8] <- 0.1
Tabla_comparativa[,8] <- as.numeric(Tabla_comparativa[,8])

#Tranformando la columna 6 a valores numericos
Tabla_comparativa[,6] <- gsub("[.]","",Tabla_comparativa[,6])
Tabla_comparativa[,6] <- as.numeric(Tabla_comparativa[,6])

#Transformando las variables de la columna 3
Tabla_comparativa[,3] <- gsub("[>]","",Tabla_comparativa[,3])
Tabla_comparativa[,3] <- gsub("millones","",Tabla_comparativa[,3])
Tabla_comparativa[,3] <- gsub("[.]","",Tabla_comparativa[,3])
Tabla_comparativa[,3] <- as.numeric(Tabla_comparativa[,3])

#Transformando las variables de la columna 2
Tabla_comparativa[,2] <- as.numeric(Tabla_comparativa[,2])

#Transformando las variables de la columna 1
Tabla_comparativa[,1] <- gsub("1","",Tabla_comparativa[,1])
Tabla_comparativa[,1] <- gsub("2","",Tabla_comparativa[,1])
Tabla_comparativa[,1] <- gsub("3","",Tabla_comparativa[,1])
Tabla_comparativa[,1] <- gsub("4","",Tabla_comparativa[,1])
Tabla_comparativa[,1] <- gsub("5","",Tabla_comparativa[,1])

#Reemplazando las variables de la columna 4 que indican sin limite por NA
for (i in 1:nrow(Tabla_comparativa)) {
  if(Tabla_comparativa[i,4] == "Sin límite máximo"){
    Tabla_comparativa[i,4] <- NA
  }
}
#Transformando la columna 4 a valores numericos
Tabla_comparativa[,4] <- gsub("millones","",Tabla_comparativa[,4])
Tabla_comparativa[,4] <- gsub("(preminados)","",Tabla_comparativa[,4])
Tabla_comparativa[,4] <- gsub("[()]","",Tabla_comparativa[,4])
Tabla_comparativa[,4] <- gsub("[.]","",Tabla_comparativa[,4])
Tabla_comparativa[,4] <- as.numeric(Tabla_comparativa[,4])

#Corrigiendo error en la sintaxis de las columnas
Tabla_comparativa <- rename(Tabla_comparativa, "Cantidad en circulación (>M)" = Cantidad.en.circulación...M., 
                            "Oferta máxima (M)" = Oferta.máxima..M., "Ratio de minado/emisión" = Ratio.de.minado.emisión,
                            "Transacciones por sg" = Transacciones.por.segundo, "Tiempo para un bloque (sg)" = Tiempo.para.un.bloque..segundos.)

########################################### INVESTING. COM ########################################################
#setwd("C:/Users/theca/OneDrive/Escritorio/Trabajo_BigData")
# Cargar pagina 
# Borrando variables de entorno 
#rm(list = ls())

# Instalando librerias
#install.packages('rvest')
#install.packages('gdata')

# Importando librerias
#library('rvest')
#library('gdata')

# Creando variable
pagina <- read_html('https://es.investing.com/crypto/')

# Saca el texto del html 
textoHtml <- html_text(pagina)
print(textoHtml)

# Sacar informacion de la tabla
# Sacar tabla real hecha
tCPCL <- html_table(pagina)[[1]]


# Eliminar columna 1 que contenia NA representando una imagen
tCPCL <- tCPCL[,-1]


# Cambiar simbolos a la fila de caracteristicas
#install.packages("dplyr")
#library("dplyr")
tCPCL <- rename(tCPCL, "Cap. mercado ($) (M)" = "Cap. mercado")
tCPCL <- rename(tCPCL, "Vol. (24h) ($) (M)" = "Vol. (24h)")
tCPCL <- rename(tCPCL, "Vol. total (%)" = "Vol. total")
tCPCL <- rename(tCPCL, "Var. (24h) (%)" = "Var. (24h)")
tCPCL <- rename(tCPCL, "Var. (7d) (%)" = "Var. (7d)")

# Eliminar signos en las columnas
tCPCL[,3] <- gsub("[.]", "", tCPCL[,3])
tCPCL[,3] <- gsub("[,]", ".", tCPCL[,3])
tCPCL[,4] <- gsub("[$]","", tCPCL[,4])
tCPCL[,4] <- gsub("[,]", ".", tCPCL[,4])
tCPCL[,4] <- gsub("[B]", "", tCPCL[,4])
tCPCL[,5] <- gsub("[$]", "", tCPCL[,5])
tCPCL[,5] <- gsub("[,]", ".", tCPCL[,5])
tCPCL[,5] <- gsub("[B]", "", tCPCL[,5])
tCPCL[,5] <- gsub("[M]", "", tCPCL[,5])
tCPCL[,6] <- gsub("[%]", "", tCPCL[,6])
tCPCL[,6] <- gsub("[,]", ".", tCPCL[,6])
tCPCL[,7] <- gsub("[%]", "", tCPCL[,7])
tCPCL[,7] <- gsub("[,]", ".", tCPCL[,7])
tCPCL[,8] <- gsub("[%]", "", tCPCL[,8])
tCPCL[,8] <- gsub("[,]", ".", tCPCL[,8])

# Conocer que tipo de variable corresponden las de la tabla
typeof(tCPCL[["Nombre"]])
typeof(tCPCL[["Precio (USD)"]])
typeof(tCPCL[["Cap. mercado ($) (M)"]])
typeof(tCPCL[["Vol. (24h) ($) (M)"]])
typeof(tCPCL[["Vol. total (%)"]])
typeof(tCPCL[["Var. (24h) (%)"]])
typeof(tCPCL[["Var. (7d) (%)"]])

#Cambiar tipo de variable de caracter a numero 
tCPCL[["Precio (USD)"]] <- as.numeric(tCPCL[["Precio (USD)"]])
typeof(tCPCL[["Precio (USD)"]])

tCPCL[["Cap. mercado ($) (M)"]] <- as.numeric(tCPCL[["Cap. mercado ($) (M)"]])
typeof(tCPCL[["Cap. mercado ($) (M)"]])

tCPCL[["Vol. (24h) ($) (M)"]] <- as.numeric(tCPCL[["Vol. (24h) ($) (M)"]])
typeof(tCPCL[["Vol. (24h) ($) (M)"]])

tCPCL[["Vol. total (%)"]] <- as.numeric(tCPCL[["Vol. total (%)"]])
typeof(tCPCL[["Vol. total (%)"]])

# Cambiar variable 4 a millones
tCPCL[,4] <- tCPCL[,4]*1000
tCPCL[1,5] <- tCPCL[1,5]*1000
tCPCL[2,5] <- tCPCL[2,5]*1000
tCPCL[3,5] <- tCPCL[3,5]*1000
tCPCL[4,5] <- tCPCL[4,5]*1000
tCPCL[5,5] <- tCPCL[5,5]*1000
tCPCL[6,5] <- tCPCL[6,5]*1000
tCPCL[7,5] <- tCPCL[7,5]*1000
tCPCL[8,5] <- tCPCL[8,5]*1000
tCPCL[10,5] <- tCPCL[10,5]*1000

#############################################BROKERONLINE.ES###############################################
##Instalando los paquetes 
#install.packages(rvest)
#install.packages(gdata)
#install.packages(dylyr)


##Corriendo los paquetes
#library(rvest)
#library(gdata)
#library(dplyr)


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

######################################## JUNTANDO LAS DISTINTAS BASES DE DATOS ###############################

#Realizando cambios en las bases de datos para poder juntarlas

##Ordenamiento burbuja de la base de datos ig.com

tmp <- Tabla_comparativa[3,]
Tabla_comparativa[3,] <- Tabla_comparativa[2,]
Tabla_comparativa[2,] <- tmp

tmp <- Tabla_comparativa[5,]
Tabla_comparativa[5,] <- Tabla_comparativa[3,]
Tabla_comparativa[3,] <- tmp

tmp <- Tabla_comparativa[4,]
Tabla_comparativa[4,] <- c("Polkadot (DOT)",NA,NA,NA,NA,NA,NA,NA)
Tabla_comparativa <- rbind(Tabla_comparativa,tmp)

tmp <- Tabla_comparativa[5,]
Tabla_comparativa[5,] <- Tabla_comparativa[9,]
Tabla_comparativa[9,] <- tmp

tmp <- Tabla_comparativa[6,]
Tabla_comparativa[6,] <- c("Binance coin (BNB)",NA,NA,NA,NA,NA,NA,NA)
Tabla_comparativa <- rbind(Tabla_comparativa,tmp)

tmp <- Tabla_comparativa[7,]
Tabla_comparativa[7,] <- Tabla_comparativa[9,]
Tabla_comparativa[9,] <- tmp
Tabla_comparativa[9,] <- tmp

tmp <- Tabla_comparativa[8,]
Tabla_comparativa[8,] <- c("Tether (USDt)",NA,NA,NA,NA,NA,NA,NA)
Tabla_comparativa <- rbind(Tabla_comparativa,tmp)

tmp <- Tabla_comparativa[9,]
Tabla_comparativa[9,] <- c("Chainlink (LINK)",NA,NA,NA,NA,NA,NA,NA)
Tabla_comparativa <- rbind(Tabla_comparativa,tmp)

tmp <- Tabla_comparativa[10,]
Tabla_comparativa[10,] <- c("Cardano (ADA)",NA,NA,NA,NA,NA,NA,NA)
Tabla_comparativa <- rbind(Tabla_comparativa,tmp)

tmp <- Tabla_comparativa[11,]
Tabla_comparativa[11,] <- c("Libra",NA,NA,NA,NA,NA,NA,NA)
Tabla_comparativa <- rbind(Tabla_comparativa,tmp)

tmp <- Tabla_comparativa[12,]
Tabla_comparativa[12,] <- Tabla_comparativa[13,]
Tabla_comparativa[13,] <- tmp

tmp <- Tabla_comparativa[13,]
Tabla_comparativa[13,] <- c("Monero (XMR)",NA,NA,NA,NA,NA,NA,NA)
Tabla_comparativa <- rbind(Tabla_comparativa,tmp)

tmp <- Tabla_comparativa[14,]
Tabla_comparativa[14,] <- Tabla_comparativa[15,]
Tabla_comparativa[15,] <- tmp

tmp <- Tabla_comparativa[15,]
Tabla_comparativa[15,] <- c("Tron (TRX)",NA,NA,NA,NA,NA,NA,NA)
Tabla_comparativa <- rbind(Tabla_comparativa,tmp)

##Ordenamiento burbuja de la base de datos de broker.com

tmp <- Criptomonedas[15,]
Criptomonedas[15,] <- c("Neo (NEO)",NA,NA,NA)
Criptomonedas <- rbind(Criptomonedas,tmp)

tmp <- Criptomonedas[15,]
Criptomonedas[15,] <- Criptomonedas[16,]
Criptomonedas[16,] <- tmp

##Ordamiento burbuja de la base de datos de investing.com

tmp <- tCPCL[3,]
tCPCL[3,] <- tCPCL[4,]
tCPCL[4,] <- tmp

tmp <- tCPCL[4,]
tCPCL[4,] <- tCPCL[7,]
tCPCL[7,] <- tmp

tmp <- tCPCL[6,]
tCPCL[6,] <- tCPCL[9,]
tCPCL[9,] <- tmp

tmp <- tCPCL[7,]
tCPCL[7,] <- tCPCL[9,]
tCPCL[9,] <- tmp

tmp <- tCPCL[8,]
tCPCL[8,] <- tCPCL[9,]
tCPCL[9,] <- tmp

tmp <- tCPCL[9,]
tCPCL[9,] <- tCPCL[10,]
tCPCL[10,] <- tmp

###Agregando filas con las criptomonedas restantes a la base de datos de investing.com

Libra <- c("Libra",NA,NA,NA,NA,NA,NA,NA)
Eos <- c("Eos","EOS",NA,NA,NA,NA,NA,NA)
Monero <- c("Monero","XMR",NA,NA,NA,NA,NA,NA)
Stellar <- c("Stellar","XLM",NA,NA,NA,NA,NA,NA)
Tron <- c("Tron","TRX",NA,NA,NA,NA,NA,NA)
Neo <- c("Neo","NEO",NA,NA,NA,NA,NA,NA)

tCPCL <- rbind(tCPCL,Libra,Eos,Monero,Stellar,Tron,Neo)

#Juntando las columnas de las bases de datos

Criptomonedas <- cbind(Criptomonedas,Tabla_comparativa$`Cantidad en circulación (>M)`,Tabla_comparativa$`Ratio de minado/emisión`,
                       Tabla_comparativa$`Transacciones por sg`,Tabla_comparativa$Red,Tabla_comparativa$`Tiempo para un bloque (sg)`,
                       tCPCL$`Precio (USD)`,tCPCL$`Vol. (24h)`,tCPCL$`Vol. total`,tCPCL$`Var. (24h)`,tCPCL$`Var. (7d)`)

#Corrigiendo errores en los nombres de las columnas

Criptomonedas <- rename(Criptomonedas, "Capacidad de mercado (%)" = "Capacidad.de.mercado (%)", "Cantidad máxima ($) (M)" = "Cantidad.maxima ($) (M)", 
                        "Cantidad en circulación (>M)" = "Tabla_comparativa$`Cantidad en circulación (>M)`", 
                        "Ratio de minado/emisión" = "Tabla_comparativa$`Ratio de minado/emisión`",
                        "Transacciones por sg" = "Tabla_comparativa$`Transacciones por sg`",
                        "Red" = "Tabla_comparativa$Red", "Tiempo para un bloque (sg)" = "Tabla_comparativa$`Tiempo para un bloque (sg)`",
                        "Precio (USD)" = "tCPCL$`Precio (USD)`", "Vol. (24h) ($) (M)" = "tCPCL$`Vol. (24h)`", 
                        "Vol. Total (%)" = "tCPCL$`Vol. total`", "Var. (24h) (%)" = "tCPCL$`Var. (24h)`",
                        "Var. (7d) (%)" = "tCPCL$`Var. (7d)`")

#Convalidando informacion perdida
Criptomonedas[16,2] <- Tabla_comparativa[16,2]
Criptomonedas[16,4] <- Tabla_comparativa[16,4]

#Corrigiendo errores en la transformacion de las columnas

Criptomonedas[,3] <- as.numeric(Criptomonedas[,3])
Criptomonedas[,4] <- as.numeric(Criptomonedas[,4])
Criptomonedas[,7] <- as.numeric(Criptomonedas[,7])
Criptomonedas[,2] <- as.numeric(Criptomonedas[,2])
Criptomonedas[,5] <- as.numeric(Criptomonedas[,5])
Criptomonedas[,9] <- as.numeric(Criptomonedas[,9])
Criptomonedas[,10] <- as.numeric(Criptomonedas[,10])
Criptomonedas[,11] <- as.numeric(Criptomonedas[,11])
Criptomonedas[,12] <- as.numeric(Criptomonedas[,12])
Criptomonedas[,13] <- as.numeric(Criptomonedas[,13])
Criptomonedas[,14] <- as.numeric(Criptomonedas[,14])

Criptomonedas[,4] <- Criptomonedas[,4] * 1000

#Corrigiendo errores en la data producidos por el cambio de año

Criptomonedas <- Criptomonedas[-11,]

#Creando vector con la numeracion de filas para facilitar su posterior trabajo

Rows <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
Criptomonedas <- data.frame(Criptomonedas, row.names = Rows)

#Cambiando el nombre de las columnas debido a error producido con el cambio de filas

Headers <- c("Criptomoneda","Lanzamiento (año)","Capacidad de mercado (%)", "Cantidad máxima ($) (M)", 
             "Cantidad en circulación (>M)", "Ratio de minado/emisión", "Transacciones por sg",
             "Red", "Tiempo para un bloque (sg)","Precio (USD)", "Vol. (24h) ($) (M)", 
             "Vol. Total (%)", "Var. (24h) (%)","Var. (7d) (%)")
colnames(Criptomonedas) <- Headers


############################################### ESTABLECIENDO LAS OPCIONES PARA LA OBTENCION DE DATOS ######################################

#AÑO DE LANZAMIENTO

  ##El mas reciente
  ##El mas antiguo

#Capacidad de mercado

  ##Rangos de 10 en 10 (%)

#Cantidad Maxima

  ##El max
  ##El min

#Cantidad en circulacion

  ##El max
  ##El min

#Ratio de minado/emision

  ##Todas las variables son opcion -> Arroje la criptomoneda correspondiente

#Transacciones por sg

  ##Rangos de: 0 - 999 ; 1000 - 1999 ; 2000 - 2999

#Tiempo para un bloque (sg)

  ##El max
  ##El min

#Precio

  ##Rangos de: 0 - 100 ; 101 - 1000 ; 1001 - 10000 ; 10001 - 100000

#Vol. 24hr

  ##Rangos de: 0 - 1000 ; 1001 - 2000 ; 2001 - 3000 ; 3001 - 4000 ; 4001 - 5000 ; 5001 - 6000 ; 6001 - 7000 ; 7001 - 8000 ; 8001 - 9000 ; 9001 - 10000 ; 10001 - 30000 ; 30001 - 60000 ; 60001 - 100000

#Vol total (%)

  ##Rangos de 10 en 10 (%)

##Var (42h)

  ##El max
  ##El min

##Var (7d)

  ##El max
  ##El min

################################################### ESTABLECIENDO SUBFUNCIONES ##############################################

#Creando funcion que obtiene la criptomoneda de lanzamiento mas reciente o más antigua

Año <- function(Año){
  if(Año == "Más reciente"){
    Maximo <- max(Criptomonedas$`Lanzamiento (año)`)
    Cmax <- c()
    for (i in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[i,2] == Maximo)
        Cmax <- c(Cmax,Criptomonedas[i,1])
    }
    print(paste("La criptomoneda con fecha de lanzamiento mas reciente es:", Cmax, ", en el año", Maximo))
  }
  else if(Año == "Más antigua"){
    Minimo <- min(Criptomonedas$`Lanzamiento (año)`)
    Cmin <- c()
    for (b in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[b,2] == Minimo)
        Cmin <- c(Cmin,Criptomonedas[b,1])
    }
    print(paste("La criptomoneda con fecha de lanzamiento mas antigua es:", Cmin, ", en el año", Minimo))
  }
}

##Probando dicha funcion
Año("Más reciente")

#Creando funcion que obtiene la criptomoneda acorde a los rangos de capacidad de mercado solicitados

Cript <- function(a){
  ChosenCapacity <- c()
  CriptCapacity <- c()
  Criptomonedas <- Criptomonedas[-15,]
  if(a == "0 - 10%"){
    for (i in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[i,3] >= 0 && Criptomonedas[i,3] <= 10){
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
  }
  else if(a == "10 - 20%"){
    for (i in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[i,3] >= 10 && Criptomonedas[i,3] <= 20){
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
  }
  else if(a == "20 - 30%"){
    for (i in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[i,3] >= 20 && Criptomonedas[i,3] <= 30){
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
  }
  else if(a == "30 - 40%"){
    for (i in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[i,3] >= 30 && Criptomonedas[i,3] <= 40){
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
  }
  else if(a == "40 - 50%"){
    for (i in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[i,3] >= 40 && Criptomonedas[i,3] <= 50){
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
  }
  else if(a == "50 - 60%"){
    for (i in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[i,3] >= 50 && Criptomonedas[i,3] <= 60){
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
  }
  else if(a == "60 - 70%"){
    for (i in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[i,3] >= 60 && Criptomonedas[i,3] <= 70){
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
  }
  else if(a == "70 - 80%"){
    for (i in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[i,3] >= 70 && Criptomonedas[i,3] <= 80){
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
  }
  else if(a == "80 - 90%"){
    for (i in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[i,3] >= 80 && Criptomonedas[i,3] <= 90){
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
  }
  else if(a == "90 - 100%"){
    for (i in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[i,3] >= 90 && Criptomonedas[i,3] <= 100){
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
  }
  print(paste("La criptomoneda con con una capacidad de mercado entre", a, "es:", CriptCapacity, "especificamente con una capacidad de mercado de un", ChosenCapacity, "%"))
}

##Probando funcion
Cript("0 - 10%")

#Creando funcion que obtiene las criptomonedas acorde a los rangos establecidos de transacciones por segundos

Transg <- function(rango){
  SelectedTran <- c()
  CorrespondentCript <- c()
  Criptomonedas <- Criptomonedas[-c(4,6,8,9,10,12,14),]
  if(rango == "0 - 999"){
    for (i in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[i,7] >= 0 && Criptomonedas[i,7] <= 999){
        SelectedTran <- c(SelectedTran,Criptomonedas[i,7])
        CorrespondentCript <- c(CorrespondentCript,Criptomonedas[i,1])
      }
    }
  }
  else if(rango == "1000 - 1999"){
    for (i in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[i,7] >= 1000 && Criptomonedas[i,7] <= 1999){
        SelectedTran <- c(SelectedTran,Criptomonedas[i,7])
        CorrespondentCript <- c(CorrespondentCript,Criptomonedas[i,1])
      }
    }
  }
  else if(rango == "2000 - 2999"){
    for (i in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[i,7] >= 2000 && Criptomonedas[i,7] <= 2999){
        SelectedTran <- c(SelectedTran,Criptomonedas[i,7])
        CorrespondentCript <- c(CorrespondentCript,Criptomonedas[i,1])
      }
    }
  }
  print(paste("La criptomoneda", CorrespondentCript, "con", SelectedTran,"transacciones por segundo se encuentra en el rango seleccionado de", rango))
}

##Probando funcion

Transg("2000 - 2999")

#Creando funcion que obtiene la criptomoneda con menor o mayor tiempo en segundos requeridos por bloque

TimeBloque <- function(Tiempo){
  Criptomonedas <- Criptomonedas[-c(4,6,8,9,10,12,14),]
  if(Tiempo == "Mayor"){
    Mayor <- max(Criptomonedas$`Tiempo para un bloque (sg)`)
    Cmayor <- c()
    for (f in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[f,9] == Mayor)
        Cmayor <- c(Cmayor,Criptomonedas[f,1])
    }
    print(paste("La criptomoneda con mayor tiempo requerido por bloque es:", Cmayor, ", con un tiempo de", Mayor, "segundos"))
  }
  else if(Tiempo == "Menor"){
    Menor <- min(Criptomonedas$`Tiempo para un bloque (sg)`)
    Cmenor <- c()
    for (c in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[c,2] == Menor)
        Cmenor <- c(Cmenor,Criptomonedas[c,1])
    }
    print(paste("La criptomoneda con menor tiempor requerido por bloque es:", Cmenor, ", con un tiempo de", Menor, "segundos"))
  }
}

##Probando funcion

TimeBloque("Mayor")

#Creando funcion que obtiene la criptomoneda con la cantidad maxima o minima

Mill <- function(Mil){
  Criptomonedas <- Criptomonedas[-c(2),]
  if(Mil == "Mayor"){
    Ma <- max(Criptomonedas$`Cantidad máxima ($) (M)`)
    Cma <- c()
    for (i in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[i,4] == Ma)
        Cma <- c(Cma,Criptomonedas[i,1])
    }
    print(paste("La criptomoneda con una mayor Cantidad Maxima es :", Cma, ", con US (M)", Ma))
  }
  else if(Mil == "Menor"){
    Mi <- min(Criptomonedas$`Cantidad máxima ($) (M)`)
    Cmi <- c()
    for (b in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[b,4] == Mi)
        Cmi <- c(Cmi,Criptomonedas[b,1])
    }
    print(paste("La criptomoneda con una menor Cantidad Maxima es :", Cmi, ", con US (M)", Mi))
  }
}

##Probando la funcion 

Mill("Mayor")

#Creando funcion que obtiene la criptomoneda con la mayor o menor cantidad en cirulacion

Circulacion <- function(Circulacion){
  Criptomonedas <- Criptomonedas[-c(4,6,8,9,10,12,14),]
  if(Circulacion == "Mayor"){
    MaC <- max(Criptomonedas$`Cantidad en circulación (>M)`)
    CmaC <- c()
    for (i in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[i,5] == MaC)
        CmaC <- c(CmaC,Criptomonedas[i,1])
    }
    print(paste("La criptomoneda con una mayor  :", CmaC, ", con (>M)", MaC))
  }
  else if(Circulacion == "Menor"){
    MiC <- min(Criptomonedas$`Cantidad en circulación (>M)`)
    CmiC <- c()
    for (b in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[b,5] == MiC)
        CmiC <- c(CmiC,Criptomonedas[b,1])
    }
    print(paste("La criptomoneda con una menor Cantidad en circulacion es :", CmiC, ", con  (>M)", MiC))
  }
}
## Probando funcion 

Circulacion("Menor")

# Creando funcion: Precio (USD). Que calcule en intervalos.  

Criptomonedas$`Precio (USD)` <- as.numeric(Criptomonedas$`Precio (USD)`)

PrecioUSD <- function(x){
  cx <- c()
  xp <- c()
  Criptomonedas <- Criptomonedas[-11:-15,]
  if(x == "0 - 100"){
    for (xx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xx,10] >= 0.000 && Criptomonedas[xx,10] <= 100.000){
        cx <- c(cx,Criptomonedas[xx,10])
        xp <- c(xp,Criptomonedas[xx,1])
      }
    }
  }
  else if(x == "101 - 1000"){
    for (xx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xx,10] >= 101.000 && Criptomonedas[xx,10] <= 1000.000){
        cx <- c(cx,Criptomonedas[xx,10])
        xp <- c(xp,Criptomonedas[xx,1])
      }
    }
  }
  else if(x == "1001 - 10000"){
    for (xx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xx,10] >= 1001.000 && Criptomonedas[xx,10] <= 10000.000){
        cx <- c(cx,Criptomonedas[xx,10])
        xp <- c(xp,Criptomonedas[xx,1])
      }
    }
  }
  else if(x == "10001 - 100000"){
    for (xx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xx,10] >= 10001.000 && Criptomonedas[xx,10] <= 100000.000){
        cx <- c(cx,Criptomonedas[xx,10])
        xp <- c(xp,Criptomonedas[xx,1])
      }
    }
  }
  print(paste("La criptomoneda que tiene un precio (USD) entre", x, "es:", xp, "representando especificamente un precio de", cx, "USD"))
}


#Probando funcion
PrecioUSD("0 - 100")
PrecioUSD("101 - 1000")
PrecioUSD("1001 - 10000")
PrecioUSD("10001 - 100000")

# Creando funcion: Vol. (24h) ($) (M). Que calcule en intervalos. 

Criptomonedas$`Vol. (24h) ($) (M)` <- as.numeric(Criptomonedas$`Vol. (24h) ($) (M)`)

valorHr <- function(xd){
  cxx <- c()
  xpp <- c()
  Criptomonedas <- Criptomonedas[-11:-15,]
  if(xd == "0 - 1000"){
    for (xxx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xxx,11] >= 0.000 && Criptomonedas[xxx,11] <= 1000.000){
        cxx <- c(cxx,Criptomonedas[xxx,11])
        xpp <- c(xpp,Criptomonedas[xxx,1])
      }
    }
  }
  else if(xd == "1001 - 2000"){
    for (xxx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xxx,11] >= 1001.000 && Criptomonedas[xxx,11] <= 2000.000){
        cxx <- c(cxx,Criptomonedas[xxx,11])
        xpp <- c(xpp,Criptomonedas[xxx,1])
      }
    }
  }
  else if(xd == "2001 - 3000"){
    for (xxx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xxx,11] >= 2001.000 && Criptomonedas[xxx,11] <= 3000.000){
        cxx <- c(cxx,Criptomonedas[xxx,11])
        xpp <- c(xpp,Criptomonedas[xxx,1])
      }
    }
  }
  else if(xd == "3001 - 4000"){
    for (xxx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xxx,11] >= 3001.000 && Criptomonedas[xxx,11] <= 4000.000){
        cxx <- c(cxx,Criptomonedas[xxx,11])
        xpp <- c(xpp,Criptomonedas[xxx,1])
      }
    }
  }
  else if(xd == "4001 - 5000"){
    for (xxx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xxx,11] >= 4001.000 && Criptomonedas[xxx,11] <= 5000.000){
        cxx <- c(cxx,Criptomonedas[xxx,11])
        xpp <- c(xpp,Criptomonedas[xxx,1])
      }
    }
  }
  else if(xd == "5001 - 6000"){
    for (xxx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xxx,11] >= 5001.000 && Criptomonedas[xxx,11] <= 6000.000){
        cxx <- c(cxx,Criptomonedas[xxx,11])
        xpp <- c(xpp,Criptomonedas[xxx,1])
      }
    }
  }
  else if(xd == "6001 - 7000"){
    for (xxx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xxx,11] >= 6001.000 && Criptomonedas[xxx,11] <= 7000.000){
        cxx <- c(cxx,Criptomonedas[xxx,11])
        xpp <- c(xpp,Criptomonedas[xxx,1])
      }
    }
  }
  else if(xd == "8001 - 9000"){
    for (xxx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xxx,11] >= 8001.000 && Criptomonedas[xxx,11] <= 9000.000){
        cxx <- c(cxx,Criptomonedas[xxx,11])
        xpp <- c(xpp,Criptomonedas[xxx,1])
      }
    }
  }
  else if(xd == "9001 - 10000"){
    for (xxx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xxx,11] >= 9001.000 && Criptomonedas[xxx,11] <= 10000.000){
        cxx <- c(cxx,Criptomonedas[xxx,11])
        xpp <- c(xpp,Criptomonedas[xxx,1])
      }
    }
  }
  else if(xd == "10001 - 30000"){
    for (xxx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xxx,11] >= 10001.000 && Criptomonedas[xxx,11] <= 30000.000){
        cxx <- c(cxx,Criptomonedas[xxx,11])
        xpp <- c(xpp,Criptomonedas[xxx,1])
      }
    }
  }
  else if(xd == "30001 - 60000"){
    for (xxx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xxx,11] >= 30001.000 && Criptomonedas[xxx,11] <= 60000.000){
        cxx <- c(cxx,Criptomonedas[xxx,11])
        xpp <- c(xpp,Criptomonedas[xxx,1])
      }
    }
  }
  else if(xd == "60001 - 100000"){
    for (xxx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xxx,11] >= 60001.000 && Criptomonedas[xxx,11] <= 100000.000){
        cxx <- c(cxx,Criptomonedas[xxx,11])
        xpp <- c(xpp,Criptomonedas[xxx,1])
      }
    }
  }
  print(paste("La criptomoneda que tiene una variacion (24h) ($) entre", xd, "es:", xpp, "representando una variacion (24h) de", cxx, "($) (M)"))
}


#Probando funcion
valorHr("0 - 1000")
valorHr("1001 - 2000")
valorHr("2001 - 3000")
valorHr("3001 - 4000")
valorHr("4001 - 5000")
valorHr("5001 - 6000")
valorHr("6001 - 7000")
valorHr("8001 - 9000")
valorHr("9001 - 10000")
valorHr("10001 - 30000")
valorHr("30001 - 60000")
valorHr("60001 - 100000")

# Creando funcion: Vol. total (%). Que calcule en intervalos.  

Criptomonedas$`Vol. Total (%)` <- as.numeric(Criptomonedas$`Vol. Total (%)`)

volTotalPorcentaje <- function(b){
  cb <- c()
  xb <- c()
  Criptomonedas <- Criptomonedas[-c(11,12,13,14,15),]
  if(b == "0 - 10%"){
    for (bb in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[bb,12] >= 0 && Criptomonedas[bb,12] <= 10){
        cb <- c(cb,Criptomonedas[bb,12])
        xb <- c(xb,Criptomonedas[bb,1])
      }
    }
  }
  else if(b == "10 - 20%"){
    for (bb in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[bb,12] >= 10 && Criptomonedas[bb,12] <= 20){
        cb <- c(cb,Criptomonedas[bb,12])
        xb <- c(xb,Criptomonedas[bb,1])
      }
    }
  }
  else if(b == "20 - 30%"){
    for (bb in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[bb,12] >= 20 && Criptomonedas[bb,12] <= 30){
        cb <- c(cb,Criptomonedas[bb,12])
        xb <- c(xb,Criptomonedas[bb,1])
      }
    }
  }
  else if(b == "30 - 40%"){
    for (bb in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[bb,12] >= 30 && Criptomonedas[bb,12] <= 40){
        cb <- c(cb,Criptomonedas[bb,12])
        xb <- c(xb,Criptomonedas[bb,1])
      }
    }
  }
  else if(b == "40 - 50%"){
    for (bb in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[bb,12] >= 40 && Criptomonedas[bb,12] <= 50){
        cb <- c(cb,Criptomonedas[bb,12])
        xb <- c(xb,Criptomonedas[bb,1])
      }
    }
  }
  else if(b == "50 - 60%"){
    for (bb in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[bb,12] >= 50 && Criptomonedas[bb,12] <= 60){
        cb <- c(cb,Criptomonedas[bb,12])
        xb <- c(xb,Criptomonedas[bb,1])
      }
    }
  }
  else if(b == "60 - 70%"){
    for (bb in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[bb,12] >= 60 && Criptomonedas[bb,12] <= 70){
        cb <- c(cb,Criptomonedas[bb,12])
        xb <- c(xb,Criptomonedas[bb,1])
      }
    }
  }
  else if(b == "70 - 80%"){
    for (bb in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[bb,12] >= 70 && Criptomonedas[bb,12] <= 80){
        cb <- c(cb,Criptomonedas[bb,12])
        xb <- c(xb,Criptomonedas[bb,1])
      }
    }
  }
  else if(b == "80 - 90%"){
    for (bb in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[bb,12] >= 80 && Criptomonedas[bb,12] <= 90){
        cb <- c(cb,Criptomonedas[bb,12])
        xb <- c(xb,Criptomonedas[bb,1])
      }
    }
  }
  else if(b == "90 - 100%"){
    for (bb in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[bb,12] >= 90 && Criptomonedas[bb,12] <= 100){
        cb <- c(cb,Criptomonedas[bb,12])
        xb <- c(xb,Criptomonedas[bb,1])
      }
    }
  }
  print(paste("La criptomoneda con vol. total (%) entre", b, "es:", xb, "especificamente con un vol. total de", cb, "%"))
}

#Probando funcion
volTotalPorcentaje("0 - 10%")
volTotalPorcentaje("10 - 20%")
volTotalPorcentaje("20 - 30%")
volTotalPorcentaje("30 - 40%")
volTotalPorcentaje("40 - 50%")
volTotalPorcentaje("50 - 60%")
volTotalPorcentaje("60 - 70%")
volTotalPorcentaje("70 - 80%")
volTotalPorcentaje("80 - 90%")
volTotalPorcentaje("90 - 100%")


#Creando funcion que obtiene la criptomoneda con la mayor o menor Var. (7d) (%)

variacionDiasPorcentaje <- function(variacion){
  Criptomonedas <- Criptomonedas[-c(11,12,13,14,15),]
  if(variacion == "Mayor"){
    vad <- max(Criptomonedas$`Var. (7d) (%)`)
    vac <- c()
    for (yy in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[yy,14] == vad)
        vac <- c(vac,Criptomonedas[yy,1])
    }
    print(paste("La criptomoneda con una mayor variacion en 7 dias (%) es  :", vac, ", con ", vad))
  }
  else if(variacion == "Menor"){
    vid <- min(Criptomonedas$`Var. (7d) (%)`)
    vic <- c()
    for (ee in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[ee,14] == vid)
        vic <- c(vic,Criptomonedas[ee,1])
    }
    print(paste("La criptomoneda con una menor variacion en 7 dias (%) es :", vic, ", con ", vid))
  }
}
## Probando funcion 
variacionDiasPorcentaje("Mayor")
variacionDiasPorcentaje("Menor")

#Creando funcion que obtiene la criptomoneda con menor o mayor variacion en 24 horas

Varhr <- function(Coin){
  Criptomonedas <- Criptomonedas[-c(11,12,13,14,15),]
  if(Coin == "Mayor"){
    Biggest <- max(Criptomonedas$`Var. (24h) (%)`)
    MonedaMa <- c()
    for (z in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[z,13] == Biggest){
        MonedaMa <- c(MonedaMa,Criptomonedas[z,1])
      }
    }
    print(paste("La criptomoneda con la mayor variacion en 24 horas es:", MonedaMa, "con una variación de un", Biggest, "%"))
  }
  else if(Coin == "Menor"){
    Smaller <- min(Criptomonedas$`Var. (24h) (%)`)
    MonedaMe <- c()
    for (z in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[z,13] == Smaller){
        MonedaMe <- c(MonedaMe,Criptomonedas[z,1])
      }
    }
    print(paste("La criptomoneda con la menor variacion en 24 horas es:", MonedaMe, "con una variación de un", Smaller, "%"))
  }
}

##Probando la funcion

Varhr("Menor")

#Creando funcion que obtiene la moneda correspondienten al ratio de minado/emision solicitado

Mining <- function(h){
  if(h == "12,5 por bloque"){
    print(paste("Las criptomonedas con el ratio de minado/emision de", h, "son:",Criptomonedas[1,1] ,"y",Criptomonedas[7,1] ))
  }else if(h == "3 por bloque"){
    print(paste("La criptomoneda con el ratio de minado/emision de", h, "es:",Criptomonedas[2,1]))
  }else if(h == "1.000 millones al mes"){
    print(paste("La criptomoneda con el ratio de minado/emision de", h, "es:",Criptomonedas[3,1]))
  }else if(h == "25 por bloque"){
    print(paste("La criptomoneda con el ratio de minado/emision de", h, "es:",Criptomonedas[5,1]))
  }else if(h == "Hasta un 5% de inflación por año"){
    print(paste("La criptomoneda con el ratio de minado/emision de", h, "es:",Criptomonedas[11,1]))
  }else if(h == "Hasta un 1% de inflación por año"){
    print(paste("La criptomoneda con el ratio de minado/emision de", h, "es:",Criptomonedas[13,1]))
  }else if(h == "Hasta 15 millones por año"){
    print(paste("La criptomoneda con el ratio de minado/emision de", h, "es:",Criptomonedas[15,1]))
  }else{
    print("No hay criptomoneda alguna con el ratio de minado/emision inidicado")
  }
}

##Probando la funcion
Mining("1.000 millones al mes")

############################################### CREANDO FUNCION FINAL #####################################################

Filtrar <- function(variable,opcion){
  y <- opcion
  if(variable == "año"){
    Año(y)
  }
  else if(variable == "capacidad de mercado"){
    Cript(y)
  }
  else if(variable == "Cantidad máxima $M"){
    Mill(y)
  }
  else if(variable == "Cantidad en circulación"){
    Circulacion(y)
  }
  else if(variable == "Ratio de minado/emisión"){
    Mining(y)
  }
  else if(variable == "Transacciones por segundo"){
    Transg(y)
  }
  else if(variable == "Tiempo para el minado de un bloque"){
    TimeBloque(y)
  }
  else if(variable == "Precio USD"){
    PrecioUSD(y)
  }
  else if(variable == "Volumen 24hr $M"){
    valorHr(y)
  }
  else if(variable == "Volumen total %"){
    volTotalPorcentaje(y)
  }
  else if(variable == "Variación 24hr %"){
    Varhr(y)
  }
  else if(variable == "Variación 7d %"){
    variacionDiasPorcentaje(y)
  }else{
    print("No es posible filtrar acorde a la opcion indicada")
  }
}

##Probando la funcion
Filtrar("Variación 7d %","Mayor")

####################################### OBTENIENDO ESTADISTICAS ###########################################

#Se obtendrá, orden: 
# media, moda, mediana, maximo, minimo, desviacion estandar y varianza

# Estadistica Precio (USD)
install.packages("modeest")
library("modeest")

mediaPrecioUSD <- mean(Criptomonedas$`Precio (USD)`, na.rm = TRUE)
modaPrecioUSD <- mfv(Criptomonedas$`Precio (USD)`, na_rm = TRUE)
medianaPrecioUSD <- median(Criptomonedas$`Precio (USD)`, na.rm = TRUE)
maximoPrecioUSD <- max(Criptomonedas$`Precio (USD)`, na.rm = TRUE)
minimoPrecioUSD <- min(Criptomonedas$`Precio (USD)`, na.rm = TRUE)
desviacionEstandarPrecioUSD <- sd(Criptomonedas$`Precio (USD)`, na.rm = TRUE)
varianzaPrecioUSD <- var(Criptomonedas$`Precio (USD)`, na.rm = TRUE)

EstPrecioUSD <- c(mediaPrecioUSD, NA, medianaPrecioUSD, maximoPrecioUSD, minimoPrecioUSD, desviacionEstandarPrecioUSD, varianzaPrecioUSD)

# Estadistica Vol. (24h) ($) (M)
mediaVol24h <- mean(Criptomonedas$`Vol. (24h) ($) (M)`, na.rm = TRUE)
modaVol24h <- mfv(Criptomonedas$`Vol. (24h) ($) (M)`)
medianaVol24h <- median(Criptomonedas$`Vol. (24h) ($) (M)`, na.rm = TRUE)
maximoVol24h <- max(Criptomonedas$`Vol. (24h) ($) (M)`, na.rm = TRUE)
minimoVol24h <- min(Criptomonedas$`Vol. (24h) ($) (M)`, na.rm = TRUE)
desviacionEstandarVol24h <- sd(Criptomonedas$`Vol. (24h) ($) (M)`, na.rm = TRUE)
varianzaVol24h <- var(Criptomonedas$`Vol. (24h) ($) (M)`, na.rm = TRUE)

EstVol24h <- c(mediaVol24h, NA, medianaVol24h, maximoVol24h, minimoVol24h, desviacionEstandarVol24h, varianzaVol24h)

# Estadistica Vol. total (%) 
mediaVoltotal <- mean(Criptomonedas$`Vol. Total (%)`, na.rm = TRUE)
modaVoltotal <- mfv(Criptomonedas$`Vol. Total (%)`)
medianaVoltotal <- median(Criptomonedas$`Vol. Total (%)`, na.rm = TRUE)
maximoVoltotal <- max(Criptomonedas$`Vol. Total (%)`, na.rm = TRUE)
minimoVoltotal <- min(Criptomonedas$`Vol. Total (%)`, na.rm = TRUE)
desviacionEstandarVoltotal <- sd(Criptomonedas$`Vol. Total (%)`, na.rm = TRUE)
varianzatVoltotal <- var(Criptomonedas$`Vol. Total (%)`, na.rm = TRUE)

EstVoltotal <- c(mediaVoltotal, NA, medianaVoltotal, maximoVoltotal, minimoVoltotal, desviacionEstandarVoltotal, varianzatVoltotal)


# Estadistica Var. (24h) (%) 
mediaVar24h <- mean(Criptomonedas$`Var. (24h) (%)`, na.rm = TRUE)
modaVar24h <- mfv(Criptomonedas$`Var. (24h) (%)`)
medianaVar24h <- median(Criptomonedas$`Var. (24h) (%)`, na.rm = TRUE)
maximoVar24h <- max(Criptomonedas$`Var. (24h) (%)`, na.rm = TRUE)
minimoVar24h <- min(Criptomonedas$`Var. (24h) (%)`, na.rm = TRUE)
desviacionEstandarVar24h <- sd(Criptomonedas$`Var. (24h) (%)`, na.rm = TRUE)
varianzatVar24h <- var(Criptomonedas$`Var. (24h) (%)`, na.rm = TRUE)

EstVar24h <- c(mediaVar24h, NA, medianaVar24h, maximoVar24h, minimoVar24h, desviacionEstandarVar24h, varianzatVar24h)

# Estadistica Var. (7d) (%) 
mediaVar7d <- mean(Criptomonedas$`Var. (7d) (%)`, na.rm = TRUE)
modaVar7d <- mfv(Criptomonedas$`Var. (7d) (%)`)
medianaVar7d <- median(Criptomonedas$`Var. (7d) (%)`, na.rm = TRUE)
maximoVar7d <- max(Criptomonedas$`Var. (7d) (%)`, na.rm = TRUE)
minimoVar7d <- min(Criptomonedas$`Var. (7d) (%)`, na.rm = TRUE)
desviacionEstandarVar7d <- sd(Criptomonedas$`Var. (7d) (%)`, na.rm = TRUE)
varianzatVar7d <- var(Criptomonedas$`Var. (7d) (%)`, na.rm = TRUE)

EstVar7d <- c(mediaVar7d, NA, medianaVar7d, maximoVar7d, minimoVar7d, desviacionEstandarVar7d, varianzatVar7d)

#Estadisticas año de lanzamiento

MaxYear <- max(Criptomonedas$`Lanzamiento (año)`)
MinYear <- min(Criptomonedas$`Lanzamiento (año)`)
ModaYear <- mfv(Criptomonedas$`Lanzamiento (año)`)
MedianYear <- median(Criptomonedas$`Lanzamiento (año)`, na.rm = TRUE)
PromedioYear <- mean(Criptomonedas$`Lanzamiento (año)`, na.rm = TRUE)
DesvEstYear <- sd(Criptomonedas$`Lanzamiento (año)`, na.rm = TRUE)
VarYear <- var(Criptomonedas$`Lanzamiento (año)`, na.rm = TRUE)

EstLanzamiento <- c(PromedioYear,ModaYear,MedianYear,MaxYear,MinYear,DesvEstYear,VarYear)

#Estadisticas capacidad de mercado

PromedioCapacity <- mean(Criptomonedas$`Capacidad de mercado (%)`, na.rm = TRUE)
ModaCapacity <- mfv(Criptomonedas$`Capacidad de mercado (%)`, na_rm = TRUE)
MedianCapacity <- median(Criptomonedas$`Capacidad de mercado (%)`, na.rm = TRUE)
MaxCapacity <- max(Criptomonedas$`Capacidad de mercado (%)`, na.rm = TRUE)
MinCapacity <- min(Criptomonedas$`Capacidad de mercado (%)`, na.rm = TRUE)
DesvEstCapacity <- sd(Criptomonedas$`Capacidad de mercado (%)`, na.rm = TRUE)
VarCapacity <- var(Criptomonedas$`Capacidad de mercado (%)`, na.rm = TRUE)

EstCapacidadMercado <- c(PromedioCapacity,ModaCapacity,MedianCapacity,MaxCapacity,MinCapacity,DesvEstCapacity,VarCapacity)

#Estadisticas cantidad máxima

PromedioQuantityMax <- mean(Criptomonedas$`Cantidad máxima ($) (M)`, na.rm = TRUE)
ModaQuantityMax <- mfv(Criptomonedas$`Cantidad máxima ($) (M)`, na.rm = TRUE)
MedianQuantityMax <- median(Criptomonedas$`Cantidad máxima ($) (M)`, na.rm = T)
MaxQuantityMax <- max(Criptomonedas$`Cantidad máxima ($) (M)`, na.rm = T)
MinQuantityMax <- min(Criptomonedas$`Cantidad máxima ($) (M)`, na.rm = T)
DesvEstQuantityMax <- sd(Criptomonedas$`Cantidad máxima ($) (M)`, na.rm = T)
VarQuantityMax <- var(Criptomonedas$`Cantidad máxima ($) (M)`, na.rm = T)

EstQuantityMax <- c(PromedioQuantityMax,ModaQuantityMax,MedianQuantityMax,MaxQuantityMax,MinQuantityMax,DesvEstQuantityMax,VarQuantityMax)

#Estadisticas cantidad en circulacion

PromedioCircQuant <- mean(Criptomonedas$`Cantidad en circulación (>M)`, na.rm = T)
ModaCircQuantity <- mfv(Criptomonedas$`Cantidad en circulación (>M)`, na_rm = T)
MedianCircQuantity <- median(Criptomonedas$`Cantidad en circulación (>M)`, na.rm = T)
MaxCircQuantity <- max(Criptomonedas$`Cantidad en circulación (>M)`, na.rm = T)
MinCircQuantity <- min(Criptomonedas$`Cantidad en circulación (>M)`, na.rm = T)
DesvEstCircQuantity <- sd(Criptomonedas$`Cantidad en circulación (>M)`, na.rm = T)
VarCircQuantity <- var(Criptomonedas$`Cantidad en circulación (>M)`, na.rm = T)

EstCircQuantity <- c(PromedioCircQuant,ModaCircQuantity,MedianCircQuantity,MaxCircQuantity,MinCircQuantity,DesvEstCircQuantity,VarCircQuantity)

#Estadisticas transacciones por segundo

PromedioTran <- mean(Criptomonedas$`Transacciones por sg`, na.rm = T)
ModaTran <- mfv(Criptomonedas$`Transacciones por sg`, na_rm = T)
MedianTran <- median(Criptomonedas$`Transacciones por sg`, na.rm = T)
MaxTran <- max(Criptomonedas$`Transacciones por sg`, na.rm = T)
MinTran <- min(Criptomonedas$`Transacciones por sg`, na.rm = T)
DesvEstTran <- sd(Criptomonedas$`Transacciones por sg`, na.rm = T)
VarTran <- var(Criptomonedas$`Transacciones por sg`, na.rm = T)

EstTransactions <- c(PromedioTran,ModaTran,MedianTran,MaxTran,MinTran,DesvEstTran,VarTran)

####################################### UNIENDO LAS ESTADISTICAS OBTENIDAS #################################
