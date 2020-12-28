setwd("C:/Users/theca/OneDrive/Escritorio/Trabajo_BigData")
# Cargar pagina 
# Borrando variables de entorno 
rm(list = ls())

# Instalando librerias
install.packages('rvest')
install.packages('gdata')

# Importando librerias
library('rvest')
library('gdata')

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
install.packages("dplyr")
library("dplyr")
tCPCL <- rename(tCPCL, "Cap. mercado ($)" = "Cap. mercado")
tCPCL <- rename(tCPCL, "Vol. (24h) ($)" = "Vol. (24h)")
tCPCL <- rename(tCPCL, "Vol. total (%)" = "Vol. total")
tCPCL <- rename(tCPCL, "Var. (24h) (%)" = "Var. (24h)")
tCPCL <- rename(tCPCL, "Var. (7d) (%)" = "Var. (7d)")

# Eliminar signos en las columnas
tCPCL[,4] <- gsub("[$]","", tCPCL[,4])
tCPCL[,5] <- gsub("[$]", "", tCPCL[,5])
tCPCL[,6] <- gsub("[%]", "", tCPCL[,6])
tCPCL[,7] <- gsub("[%]", "", tCPCL[,7])
tCPCL[,8] <- gsub("[%]", "", tCPCL[,8])
