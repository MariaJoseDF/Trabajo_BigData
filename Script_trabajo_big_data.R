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
