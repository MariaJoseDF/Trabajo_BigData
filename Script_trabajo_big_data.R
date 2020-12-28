#setwd("C:/Users/maria/OneDrive/Escritorio/ICI Cuarto año/Segundo semestre/Big data/Trabajo_BigData")

#Instalando paquete
install.packages("rvest")
library("rvest")
install.packages("dplyr")
library("dplyr")

#Lectura de pagina con la informacion requerida
ig <- read_html("https://www.ig.com/es/trading-de-criptomonedas/comparativa-criptomonedas#information-banner-dismiss")

#Extrayendo tabla comparativa de criptomonedas
Tabla_comparativa <- html_table(ig)[[2]]

#Renombrando columnas

Tabla_comparativa <- rename(Tabla_comparativa, "Bitcoin (BTC)" = X2, "Bitcoin cash (BCH)" = X3, "Ether (ETH)" = X4, "Litecoin (LTC)" = X5,
                            "Ripple (XRP)" = X6, "EOS (EOS)" = X7, "Stellar (XLM)" = X8, "NEO (NEO)" = X9, "Característica" = X1)

#Creando vector con los nombres de las filas
Filas <- list("Lanzamiento (año)", "Cantidad en circulación (M)", "Oferta máxima (M)", "Ratio de minado/emisión", "Transacciones por segundo (máximo)",
           "Red", "Tiempo para un bloque (aprox.)")

#Eliminando fila sobrante
Tabla_comparativa <- Tabla_comparativa[-1,]

#Reasignando los nombres de las filas
Tabla_comparativa <- data.frame(Tabla_comparativa, row.names = Filas)

#Renombrando las columnas debido a error ocurrido al cambiar filas
Tabla_comparativa <- rename(Tabla_comparativa,"Bitcoin (BTC)" = Bitcoin..BTC., "Bitcoin cash (BCH)" = Bitcoin.cash..BCH., "Ether (ETH)" = Ether..ETH., "Litecoin (LTC)" = Litecoin..LTC.,
                            "Ripple (XRP)" = Ripple..XRP., "EOS (EOS)" = EOS..EOS., "Stellar (XLM)" = Stellar..XLM., "NEO (NEO)" = NEO..NEO.)

#Eliminando columna sobrante
Tabla_comparativa <- Tabla_comparativa[,-1]

#Estableciendo loop que reemplazara por NA las variables sin datos

for (i in 1:nrow(Tabla_comparativa)) {
  if(Tabla_comparativa[6,i] == "n/a"){
    Tabla_comparativa[6,i] <- NA
  }
}

#Transformando variables a valores numericos
Tabla_comparativa[1,] <- as.numeric(Tabla_comparativa[1,])
Tabla_comparativa[2,] <- gsub("millones","",Tabla_comparativa[2,])
Tabla_comparativa[3,] <- gsub("millones","",Tabla_comparativa[3,])
Tabla_comparativa[3,] <- gsub("(preminados)","",Tabla_comparativa[3,])
Tabla_comparativa[3,] <- gsub("[()]","",Tabla_comparativa[3,])
