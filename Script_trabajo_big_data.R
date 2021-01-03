#######################################  SCRIPT DEL PROYECYO: CRIPTOMONEDAS.  ################################################################

# Se consideran paquetes de utilidad y uso en la ejecución del desarrollo.

# Instalando paquetes.
# Instalación paquete "rvest" ya que permite extraer y manipular datos de las páginas webs.
install.packages("rvest")
library("rvest")

# Instalación paquete "dplyr" dado a que permite ágilmente manejar los datos.
install.packages("dplyr")
library("dplyr")

# Instalación paquete "gdta" debido a que permite manipular la data con diversas herramientas que provee.
install.packages("gdata")
library("gdata")

#######################################  EXTRACCION DE DATOS DE LA PAGINA WEB: IG.COM  ################################################################

# Se realiza la lectura de la página web respecto a la información seleccionada por el equipo.
ig <- read_html("https://www.ig.com/es/trading-de-criptomonedas/comparativa-criptomonedas#information-banner-dismiss")

# Se extrae la tabla comparativa que contiene la información requerida y se establece su nombre.
Tabla_comparativa <- html_table(ig)[[2]]

# Por orden se decide cambiar la posición de filas por columnas.
Tabla_comparativa <- as.data.frame(t(Tabla_comparativa))

# Se renombran las columnas.
Tabla_comparativa <- rename(Tabla_comparativa, "Criptomoneda" = V1, "Lanzamiento" = V2, "Cantidad en circulación (>M)" = V3,
                            "Oferta máxima (M)" = V4, "Ratio de minado/emisión" = V5, "Transacciones por segundo" = V6,
                            "Red" = V7, "Tiempo para un bloque (segundos)" = V8)

# Se elimina una fila sobrante.
Tabla_comparativa <- Tabla_comparativa[-1,]

# Se crea un vector que contenga los nombres de las filas.
Filas <- list(1,2,3,4,5,6,7,8)

# Se reasigna el nombre de las filas para su orden lógico.
Tabla_comparativa <- data.frame(Tabla_comparativa, row.names = Filas)

# Se coordina la consolidación de las mediciones, tipo de dato, estética y orden del data que se utilizara en el trabajo.
# Se realizan todos los cambios que se desean respecto a la estética y orden de la tabla, para su trabajo.

# Se establece y diseña un loop que reemplaza por NA las variables que no contienen datos.
# Diseño: for que recorre la columna 7 y reemplaza por NA.
for (i in 1:nrow(Tabla_comparativa)) {
  if(Tabla_comparativa[i,7] == "n/a"){
    Tabla_comparativa[i,7] <- NA
  }
}

# Se transforma la medición de la columna 8: de minutos a segundos.
Tabla_comparativa[1,8] <- 10*60
Tabla_comparativa[2,8] <- 10*60
Tabla_comparativa[3,8] <- 15
Tabla_comparativa[4,8] <- 2.5 * 60
Tabla_comparativa[6,8] <- 0.5
Tabla_comparativa[7,8] <- 5
Tabla_comparativa[8,8] <- 15
Tabla_comparativa[5,8] <- 0.1

# Se transforma la columna 8 de la tabla a valor numérico.
Tabla_comparativa[,8] <- as.numeric(Tabla_comparativa[,8])

# Se transforma la medición de la columna 6: cambio de puntos para el correcto reconocimiento numérico. 
Tabla_comparativa[,6] <- gsub("[.]","",Tabla_comparativa[,6])

# Se transforma la columna 6 de la tabla a valor numérico.
Tabla_comparativa[,6] <- as.numeric(Tabla_comparativa[,6])

# Se transforma la medición de la columna 3: cambio de signos y medición valórica para el correcto reconocimiento numérico. 
Tabla_comparativa[,3] <- gsub("[>]","",Tabla_comparativa[,3])
Tabla_comparativa[,3] <- gsub("millones","",Tabla_comparativa[,3])
Tabla_comparativa[,3] <- gsub("[.]","",Tabla_comparativa[,3])

# Se transforma la columna 3 de la tabla a valor numérico.
Tabla_comparativa[,3] <- as.numeric(Tabla_comparativa[,3])

# Se transforma la columna 2 de la tabla a valor numérico.
Tabla_comparativa[,2] <- as.numeric(Tabla_comparativa[,2])

# Se transforma la medición de la columna 1: cambio de designación numérica. 
Tabla_comparativa[,1] <- gsub("1","",Tabla_comparativa[,1])
Tabla_comparativa[,1] <- gsub("2","",Tabla_comparativa[,1])
Tabla_comparativa[,1] <- gsub("3","",Tabla_comparativa[,1])
Tabla_comparativa[,1] <- gsub("4","",Tabla_comparativa[,1])
Tabla_comparativa[,1] <- gsub("5","",Tabla_comparativa[,1])

# Se establece y diseña un loop que reemplaza por NA las variables que indican que no tiene límite.
# Diseño: for que recorre la columna 4 y reemplaza por NA.
for (i in 1:nrow(Tabla_comparativa)) {
  if(Tabla_comparativa[i,4] == "Sin límite máximo"){
    Tabla_comparativa[i,4] <- NA
  }
}

# Se transforma la medición de la columna 4: cambio de signos y texto para el correcto reconocimiento numérico. 
# Luego se transforma la columna 4 de la tabla a valor numérico.
Tabla_comparativa[,4] <- gsub("millones","",Tabla_comparativa[,4])
Tabla_comparativa[,4] <- gsub("(preminados)","",Tabla_comparativa[,4])
Tabla_comparativa[,4] <- gsub("[()]","",Tabla_comparativa[,4])
Tabla_comparativa[,4] <- gsub("[.]","",Tabla_comparativa[,4])
Tabla_comparativa[,4] <- as.numeric(Tabla_comparativa[,4])

# Se detecta un error de sintaxis de las columnas por nuevo diseño de medición común.
# Se Corrigiendo el error de sintaxis renombrando las variables en las columnas.
Tabla_comparativa <- rename(Tabla_comparativa, "Cantidad en circulación (>M)" = Cantidad.en.circulación...M., 
                            "Oferta máxima (M)" = Oferta.máxima..M., "Ratio de minado/emisión" = Ratio.de.minado.emisión,
                            "Transacciones por sg" = Transacciones.por.segundo, "Tiempo para un bloque (sg)" = Tiempo.para.un.bloque..segundos.)

#######################################  EXTRACCION DE DATOS DE LA PAGINA WEB: INVESTING.COM  ################################################################

# Se realiza la lectura de la página web respecto a la información seleccionada por el equipo.
pagina <- read_html('https://es.investing.com/crypto/')

# Se extrae el texto de toda la página html para ver su contenido total. 
textoHtml <- html_text(pagina)
print(textoHtml)

# Se extrae la tabla comparativa que contiene la información requerida y se establece su nombre.
tCPCL <- html_table(pagina)[[1]]

# Se elimina una fila sobrante.
tCPCL <- tCPCL[,-1]

# Se renombran las columnas.
tCPCL <- rename(tCPCL, "Cap. mercado ($) (M)" = "Cap. mercado")
tCPCL <- rename(tCPCL, "Vol. (24h) ($) (M)" = "Vol. (24h)")
tCPCL <- rename(tCPCL, "Vol. total (%)" = "Vol. total")
tCPCL <- rename(tCPCL, "Var. (24h) (%)" = "Var. (24h)")
tCPCL <- rename(tCPCL, "Var. (7d) (%)" = "Var. (7d)")

# Se coordina la consolidación de las mediciones, tipo de dato, estética y orden del data que se utilizara en el trabajo.
# Se realizan todos los cambios que se desean respecto a la estética y orden de la tabla, para su trabajo.

# Se transforma la medición de las columnas 3,4,5,6,7,8: cambio de signos y medición valórica para el correcto reconocimiento numérico. 
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

# Se desarrolla la ejecución de typeof para conocer el tipo de variable de la tabla.
typeof(tCPCL[["Nombre"]])
typeof(tCPCL[["Precio (USD)"]])
typeof(tCPCL[["Cap. mercado ($) (M)"]])
typeof(tCPCL[["Vol. (24h) ($) (M)"]])
typeof(tCPCL[["Vol. total (%)"]])
typeof(tCPCL[["Var. (24h) (%)"]])
typeof(tCPCL[["Var. (7d) (%)"]])

# Se transforman algunas columnas de la tabla a valor numérico. Y se confirma su cambio.
tCPCL[["Precio (USD)"]] <- as.numeric(tCPCL[["Precio (USD)"]])
typeof(tCPCL[["Precio (USD)"]])
tCPCL[["Cap. mercado ($) (M)"]] <- as.numeric(tCPCL[["Cap. mercado ($) (M)"]])
typeof(tCPCL[["Cap. mercado ($) (M)"]])
tCPCL[["Vol. (24h) ($) (M)"]] <- as.numeric(tCPCL[["Vol. (24h) ($) (M)"]])
typeof(tCPCL[["Vol. (24h) ($) (M)"]])
tCPCL[["Vol. total (%)"]] <- as.numeric(tCPCL[["Vol. total (%)"]])
typeof(tCPCL[["Vol. total (%)"]])

# Se transforma la medición de la columna 4: de billones a millones. 
tCPCL[,4] <- tCPCL[,4]*1000

# Se transforma la medición de la columna 5 respecto a casi todas sus filas necesarias al cambio: de billones a millones. 
tCPCL[1,5] <- tCPCL[1,5]*1000
tCPCL[2,5] <- tCPCL[2,5]*1000
tCPCL[3,5] <- tCPCL[3,5]*1000
tCPCL[4,5] <- tCPCL[4,5]*1000
tCPCL[5,5] <- tCPCL[5,5]*1000
tCPCL[6,5] <- tCPCL[6,5]*1000
tCPCL[7,5] <- tCPCL[7,5]*1000
tCPCL[8,5] <- tCPCL[8,5]*1000
tCPCL[10,5] <- tCPCL[10,5]*1000

#######################################  EXTRACCION DE DATOS DE LA PAGINA WEB: BROKERONLINE.ES  ################################################################

# Se realiza la lectura de la página web respecto a la información seleccionada por el equipo.
broker <- read_html("https://www.brokeronline.es/criptomonedas/ranking/")

# Se crea la variable que contendrá la información específica a obtener. Se busca y extrae la información directamente con xpath.
contenedoranking <- html_nodes(broker,xpath = "//*[@id=\"post-13673\"]/div")

# Se obtienen los divs que contienen la clase con la información deseada a extraer. Se extrae aquella por medio de css. 
InfoRanking <- html_nodes(contenedoranking, css = ".td-width-content")

# Se crean variables booleanas para la extracción de la información.
Lanzamiento <- FALSE
CapMercado <- FALSE
CantMax <- FALSE

# Se crea el almacenamiento de la información que se extrae por medio de variables.
LaunchDate <- c()
MarketShare <- c()
Quantity <- c()

# Inicio de la extracción de la información por data especifica. 
# Se obtienen los nombres de las criptomonedas por medio de su clase con css.
N <- html_nodes(contenedoranking, css = ".col-xs-12.text-center")

# Se transforman los nombres extraídos a texto.
TxtN <- html_text(N)

# Se limpian los nombres extraídos.
TxtN <- gsub("\n","",TxtN)
TxtN <- gsub("\t","", TxtN)

# Se establece y diseña un loop que elimina los espacios vacíos que se encuentran dentro del vector de los nombres.
# Diseño: for que recorre el vector para eliminar los espacios vacíos.
for (i in 1:length(TxtN)) {
  if(TxtN[i] == ""){
    TxtN <- TxtN[-i]
  }
}

# Se observa y reconoce que la clase que contiene la información deseada a extraer es general compartiendo el mismo nombre con otra data en su misma página web.
# Se establece y diseña un loop que extrae de la clase toda la información en definitiva deseada de los nombres anteriormente obtenidos.
# Diseño: for que recorre la variable designada de la página en su lectura. 
for (i in InfoRanking) {
  # Se indica la clase a considerar.
  L <- html_nodes(i, css = ".col-xs-12 > p")
  for (a in L) {
    # Se almacena la informacion.
    Txta <- html_text(a)
    # Condicionales para la extracción de la información únicamente de los nombres designados.
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

# Se crea un data frame en forma de tabla con la información extraída de las 3 variables (su nombre).
Criptomonedas <- data.frame("Criptomoneda" = TxtN,"Lanzamiento" = LaunchDate, "Capacidad de mercado" = MarketShare, "Cantidad maxima" = Quantity)

# Se coordina la consolidación de las mediciones, tipo de dato, estética y orden del data que se utilizara en el trabajo.
# Se realizan todos los cambios que se desean respecto a la estética y orden de la tabla, para su trabajo.

#Se transforma la medición de la columna 3: cambio de signos y texto para el correcto reconocimiento numérico. 
Criptomonedas[,3]<-gsub("En torno al","",Criptomonedas[,3])
Criptomonedas[,3]<-gsub("Menos del","",Criptomonedas[,3])
Criptomonedas [,3]<-gsub("[%]","",Criptomonedas[,3])

# Se cambia nombre de la variable de la columna.
Criptomonedas<-rename(Criptomonedas,"Capacidad.de.mercado (%)"="Capacidad.de.mercado")

# Se transforma la columna de la tabla a valor numérico. Y se confirma su cambio con typeof.
Criptomonedas[["Capacidad.de.mercado (%)"]]<-as.numeric(Criptomonedas[["Capacidad.de.mercado (%)"]])
typeof(Criptomonedas[["Capacidad.de.mercado (%)"]])

#Se transforma la medición de la columna 2: cambio de signos para el correcto reconocimiento numerico. 
Criptomonedas[,2]<-gsub("03.01.2009","2009",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("30.07.2015","2015",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("26.05.2020","2020",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("7.10.2011","2011",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("27.06.2017","2017",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("27.07.2017","2017",Criptomonedas[,2])
Criptomonedas[,2]<-gsub("Noviembre 2014","2014",Criptomonedas[,2])


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

# Se crea la función de Precio (USD).
# Se desarrolla una función respecto a la tabla general llamada "Criptomonedas" que recibe una variable encontrada como condición en intervalos personalizados específicamente y designados que se encuentran 
# sujetos a recorrer la columna 10 (Precio (USD)) identificada por un for correspondiente, reconociendo a la vez su columna 1 como base.
# Es entonces que dará como resultado el precio (USD) que se encuentra en el intervalo que se solicite.
# PrecioUSD: String -> 
# Ejemplo: Entra: "0 - 100" -> Salida:

# Se transforma la columna 10 a valores numéricos.
Criptomonedas$`Precio (USD)` <- as.numeric(Criptomonedas$`Precio (USD)`)

# Creación de la función y establecimiento de su nombre.
PrecioUSD <- function(x){
  # Se establecen vectores para almacenar la información.
  cx <- c()
  xp <- c()
  # Se elimina las filas que no debe considerar (aquellas que contienen NA en la posición de la columna)
  Criptomonedas <- Criptomonedas[-11:-15,]
  # Condicionales personalizadas respecto a las necesidades. 
  if(x == "0 - 100"){
    # Se estable el for que recorre la tabla principal a trabajar.
    for (xx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xx,10] >= 0.000 && Criptomonedas[xx,10] <= 100.000){
        # Se indica que información se almacena en los vectores.
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
  # Se establece y redacta el print que dará la respuesta de la función. 
  print(paste("La criptomoneda que tiene un precio (USD) entre", x, "es:", xp, "representando específicamente un precio de", cx, "USD"))
}


# Se consolida y prueba la función con todas sus opciones.
PrecioUSD("0 - 100")
PrecioUSD("101 - 1000")
PrecioUSD("1001 - 10000")
PrecioUSD("10001 - 100000")

# Se crea la función de Vol. (24h) ($) (M).
# Se desarrolla una función respecto a la tabla general llamada "Criptomonedas" que recibe una variable encontrada como condición en intervalos personalizados específicamente y designados que se encuentran 
# sujetos a recorrer la columna 11 (Vol. (24h) ($) (M)) identificada por un for correspondiente, reconociendo a la vez su columna 1 como base.
# Es entonces que dará como resultado el Vol. (24h) ($) (M) que se encuentra en el intervalo que se solicite.
# ValorHr: String -> 
# Ejemplo: Entra: "0 - 1000" -> Salida:

# Se transforma la columna 11 a valores numéricos.
Criptomonedas$`Vol. (24h) ($) (M)` <- as.numeric(Criptomonedas$`Vol. (24h) ($) (M)`)

# Creación de la función y establecimiento de su nombre.
valorHr <- function(xd){
  # Se establecen vectores para almacenar la información.
  cxx <- c()
  xpp <- c()
  # Se elimina las filas que no debe considerar (aquellas que contienen NA en la posición de la columna).
  Criptomonedas <- Criptomonedas[-11:-15,]
  # Condicionales personalizadas respecto a las necesidades. 
  if(xd == "0 - 1000"){
    # Se estable el for que recorre la tabla principal a trabajar.
    for (xxx in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[xxx,11] >= 0.000 && Criptomonedas[xxx,11] <= 1000.000){
        # Se indica que información se almacena en los vectores.
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
  # Se establece y redacta el print que dará la respuesta de la función. 
  print(paste("La criptomoneda que tiene una variación (24h) ($) entre", xd, "es:", xpp, "representando una variación (24h) de", cxx, "($) (M)"))
}


# Se consolida y prueba la función con todas sus opciones.
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

# Se crea la función de Vol. Total (%).
# Se desarrolla una función respecto a la tabla general llamada "Criptomonedas" que recibe una variable encontrada como condición en intervalos específicos en el orden lógico porcentual y designados que se encuentran 
# sujetos a recorrer la columna 12 (Vol. Total (%)) identificada por un for correspondiente, reconociendo a la vez su columna 1 como base.
# Es entonces que dará como resultado el Vol. Total (%) que se encuentra en el intervalo que se solicite.
# VolTotalPorcentaje: String -> 
# Ejemplo: Entra: "0 - 10" -> Salida:

# Se transforma la columna 12 a valores numéricos.
Criptomonedas$`Vol. Total (%)` <- as.numeric(Criptomonedas$`Vol. Total (%)`)

# Creación de la función y establecimiento de su nombre.
volTotalPorcentaje <- function(b){
  # Se establecen vectores para almacenar la información.
  cb <- c()
  xb <- c()
  # Se elimina las filas que no debe considerar (aquellas que contienen NA en la posición de la columna).
  Criptomonedas <- Criptomonedas[-c(11,12,13,14,15),]
  # Condicionales personalizadas respecto a las necesidades. 
  if(b == "0 - 10%"){
    # Se estable el for que recorre la tabla principal a trabajar.
    for (bb in 1:nrow(Criptomonedas)) {
      if(Criptomonedas[bb,12] >= 0 && Criptomonedas[bb,12] <= 10){
        # Se indica que información se almacena en los vectores.
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
  # Se establece y redacta el print que dará la respuesta de la función. 
  print(paste("La criptomoneda con vol. total (%) entre", b, "es:", xb, "especificamente con un vol. total de", cb, "%"))
}

# Se consolida y prueba la función con todas sus opciones.
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
