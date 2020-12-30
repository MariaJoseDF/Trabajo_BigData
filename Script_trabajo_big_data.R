setwd("C:/Users/maria/OneDrive/Escritorio/ICI Cuarto año/Segundo semestre/Big data/Trabajo_BigData")

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
