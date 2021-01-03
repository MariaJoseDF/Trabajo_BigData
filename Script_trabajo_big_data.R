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

#Se transforma la medición de la columna 2: cambio de signos para el correcto reconocimiento numerico. 
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

#Cambiando nombre de la tabla "Lanzamiento" por "Lanzamiento (año)".
Criptomonedas<-rename(Criptomonedas,"Lanzamiento (año)"="Lanzamiento")

#Transformando a solo una conversión de dinero las cantidades que se muestran, en este caso todos pasan a millones con solo el monto que contiene.
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

#Cambiando "Cantidad.maxima" por "Cantidad.maxima ($) (M)".
Criptomonedas<-rename(Criptomonedas,"Cantidad.maxima ($) (M)"="Cantidad.maxima")

#Cambiando números como as.nemeric, específicamente double.
Criptomonedas[["Cantidad.maxima ($) (M)"]]<-as.numeric(Criptomonedas[["Cantidad.maxima ($) (M)"]])
typeof(Criptomonedas[["Cantidad.maxima ($) (M)"]])

######################################## JUNTANDO LAS DISTINTAS BASES DE DATOS ###############################

#Se juntaran las siguientes tablas a continuación Criptomonedas, Tabla_comparativa y tCPCL, en una sola tabla llamada Criptomonedas con el propósito de poder realizar las operaciones en conjunto.

# Se realiza el ordenamiento burbuja de la base de datos ig.com, para que cada tabla se junte posteriormente en una sola tabla con toda la información recopilada. 
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

#Se realiza el ordenamiento burbuja de la base de datos de broker.com, para que cada tabla se junte posteriormente en una sola tabla con toda la información recopilada. 
tmp <- Criptomonedas[15,]
Criptomonedas[15,] <- c("Neo (NEO)",NA,NA,NA)
Criptomonedas <- rbind(Criptomonedas,tmp)

tmp <- Criptomonedas[15,]
Criptomonedas[15,] <- Criptomonedas[16,]
Criptomonedas[16,] <- tmp

# Se realiza el ordamiento burbuja de la base de datos de investing.com, para que cada tabla se junte posteriormente en una sola tabla con toda la información recopilada. 
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

#Agregando filas con las criptomonedas restantes a la base de datos de investing.com, esats se agregan con NA ya que la información no se encuentra.
Libra <- c("Libra",NA,NA,NA,NA,NA,NA,NA)
Eos <- c("Eos","EOS",NA,NA,NA,NA,NA,NA)
Monero <- c("Monero","XMR",NA,NA,NA,NA,NA,NA)
Stellar <- c("Stellar","XLM",NA,NA,NA,NA,NA,NA)
Tron <- c("Tron","TRX",NA,NA,NA,NA,NA,NA)
Neo <- c("Neo","NEO",NA,NA,NA,NA,NA,NA)

tCPCL <- rbind(tCPCL,Libra,Eos,Monero,Stellar,Tron,Neo)

#Juntando las columnas de las bases de datos.
Criptomonedas <- cbind(Criptomonedas,Tabla_comparativa$`Cantidad en circulación (>M)`,Tabla_comparativa$`Ratio de minado/emisión`,
                       Tabla_comparativa$`Transacciones por sg`,Tabla_comparativa$Red,Tabla_comparativa$`Tiempo para un bloque (sg)`,
                       tCPCL$`Precio (USD)`,tCPCL$`Vol. (24h)`,tCPCL$`Vol. total`,tCPCL$`Var. (24h)`,tCPCL$`Var. (7d)`)

#Corrigiendo errores en los nombres de las columnas como los puntos o mayusculas.
Criptomonedas <- rename(Criptomonedas, "Capacidad de mercado (%)" = "Capacidad.de.mercado (%)", "Cantidad máxima ($) (M)" = "Cantidad.maxima ($) (M)", 
                        "Cantidad en circulación (>M)" = "Tabla_comparativa$`Cantidad en circulación (>M)`", 
                        "Ratio de minado/emisión" = "Tabla_comparativa$`Ratio de minado/emisión`",
                        "Transacciones por sg" = "Tabla_comparativa$`Transacciones por sg`",
                        "Red" = "Tabla_comparativa$Red", "Tiempo para un bloque (sg)" = "Tabla_comparativa$`Tiempo para un bloque (sg)`",
                        "Precio (USD)" = "tCPCL$`Precio (USD)`", "Vol. (24h) ($) (M)" = "tCPCL$`Vol. (24h)`", 
                        "Vol. Total (%)" = "tCPCL$`Vol. total`", "Var. (24h) (%)" = "tCPCL$`Var. (24h)`",
                        "Var. (7d) (%)" = "tCPCL$`Var. (7d)`")

#Convalidando información perdida.
Criptomonedas[16,2] <- Tabla_comparativa[16,2]
Criptomonedas[16,4] <- Tabla_comparativa[16,4]

#Corrigiendo errores en la transformacion de las columnas.
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

#Corrigiendo errores en la data producidos por el cambio de año, debido a que la Criptomoneda Libra fue pospuesta para el año 2021 y se tuvo que eliminar de la tabla.
Criptomonedas <- Criptomonedas[-11,]

#Creando vector con la numeración de filas para facilitar su posterior trabajo.
Rows <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
Criptomonedas <- data.frame(Criptomonedas, row.names = Rows)

#Cambiando el nombre de las columnas debido a error producido con el cambio de filas, específicamente la eliminación de la Criptomoneda Libra.
Headers <- c("Criptomoneda","Lanzamiento (año)","Capacidad de mercado (%)", "Cantidad máxima ($) (M)", 
             "Cantidad en circulación (>M)", "Ratio de minado/emisión", "Transacciones por sg",
             "Red", "Tiempo para un bloque (sg)","Precio (USD)", "Vol. (24h) ($) (M)", 
             "Vol. Total (%)", "Var. (24h) (%)","Var. (7d) (%)")
colnames(Criptomonedas) <- Headers


############################################### ESTABLECIENDO LAS OPCIONES PARA LA OBTENCION DE DATOS ######################################

#Los datos seran filtrados acorde los siguientes criterios:

#Año de lanzamiento:
#El mas reciente
#El mas antiguo

#Capacidad de mercado:
#Rangos de 10 en 10 (%)

#Cantidad máxima:
#El máximo
#El mínimo

#Cantidad en circulación:
#El máximo
#El mínimo

#Ratio de minado/emisión:
#Todas las variables son una opción a escoger

#Transacciones por sg:
#Rangos de: 0 - 999 ; 1000 - 1999 ; 2000 - 2999

#Tiempo para un bloque (sg):
#El máximo
#El mínimo

#Precio:
#Rangos de: 0 - 100 ; 101 - 1000 ; 1001 - 10000 ; 10001 - 100000

#Vol. 24hr:
#Rangos de: 0 - 1000 ; 1001 - 2000 ; 2001 - 3000 ; 3001 - 4000 ; 4001 - 5000 ; 5001 - 6000 ; 6001 - 7000 ; 7001 - 8000 ; 8001 - 9000 ; 9001 - 10000 ; 10001 - 30000 ; 30001 - 60000 ; 60001 - 100000

#Vol total (%):
#Rangos de 10 en 10 (%)

#Var. (24h):
#El máximo
#El mínimo

#Var. (7d):
#El máximo
#El mínimo

################################################### ESTABLECIENDO SUBFUNCIONES ##############################################

#Creando funcion que obtiene la criptomoneda de lanzamiento mas reciente o más antigua

#Se crea un función que obtiene la criptomoneda con la fecha de lanzamaiento más reciente o más antigua segun lo especificado
#Año: Entrada: String -> Salida: String
#Ejemplo: Entrada: Más reciente -> Salida: La criptomoneda con fecha de lanzamiento mas reciente es: Polkadot (DOT) ,siendo en el año 2020

#Creando función
Año <- function(Año){
  #Estableciendo primera condicional que aplica en caso de seleccionar la filtración por más reciente
  if(Año == "Más reciente"){
    #Calculando y almacenado el máximo
    Maximo <- max(Criptomonedas$`Lanzamiento (año)`)
    #Creando vector que almacenará el nombre de la criptomoneda asociada al valor máximo
    Cmax <- c()
    #[Inicia loop]
    for (i in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la igualdad de la posición del recorrido y el valor máximo
      if(Criptomonedas[i,2] == Maximo)
        #Almacenando el nombre de la criptomoneda asociada al valor máximo
        Cmax <- c(Cmax,Criptomonedas[i,1])
    }
    #[Finaliza loop]
    #Imprimiendo resultado
    print(paste("La criptomoneda con fecha de lanzamiento mas reciente es:", Cmax, ",siendo en el año", Maximo))
  }
  #Estableciendo segunda condicional que aplica en caso de seleccionar la filtración por mas antigua
  else if(Año == "Más antigua"){
    #Calculando y almacenando el mínimo
    Minimo <- min(Criptomonedas$`Lanzamiento (año)`)
    #Creando vector que almacenará el nombre de la criptomoneda asociada al valor mínimo
    Cmin <- c()
    #[Inicia Loop]
    for (b in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la igualdad de la posición del recorrido y el valor mínimo
      if(Criptomonedas[b,2] == Minimo)
        #Almacenando el nombre de la criptomoneda asociada al valor mínimo
        Cmin <- c(Cmin,Criptomonedas[b,1])
    }
    #[Finaliza loop]
    #Imprimiendo resultado
    print(paste("La criptomoneda con fecha de lanzamiento mas antigua es:", Cmin, ",siendo en el año", Minimo))
  }
}

#Probando la función
Año("Más reciente")

#Se crea función que obtiene la criptomoneda acorde al rango de capacidad de mercado indicado
#Cript: Entrada: String -> Salida: String
#Ejemplo: Entrada: 40 - 50% -> Salida: La criptomoneda con con una capacidad de mercado entre 40 - 50% es: BITCOIN (BTC) específicamente con una capacidad de un 44 %

#Creando función
Cript <- function(a){
  #Creando vector que almacenará la capacidad dentro del rango seleccionado
  ChosenCapacity <- c()
  #Creando vector que almacenará el nombre de la criptomoneda correspondiente al las capacidades obtenidas
  CriptCapacity <- c()
  #Eliminando filas con valores NA
  Criptomonedas <- Criptomonedas[-15,]
  #Creando la primera condicional acorde a un rango de 0 - 10%
  if(a == "0 - 10%"){
    #[Inicia loop]
    for (i in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda que se encuentre dentro del rango
      if(Criptomonedas[i,3] >= 0 && Criptomonedas[i,3] <= 10){
        #Almacenando la capacidad de la criptomoneda que cumple con la condición
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
    #[Finaliza loop]
  }
  #Creando la segunda condicional acorde a un rango de 10  20%
  else if(a == "10 - 20%"){
    #[Inicia loop]
    for (i in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda que se encuentre dentro del rango
      if(Criptomonedas[i,3] >= 10 && Criptomonedas[i,3] <= 20){
        #Almacenando la capacidad de la criptomoneda que cumple con la condición
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
    #[Finaliza loop]
  }
  #Creando la tercera condicional acorde a un rango de 20 - 30%
  else if(a == "20 - 30%"){
    #[Inicia loop]
    for (i in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda que se encuentre dentro del rango
      if(Criptomonedas[i,3] >= 20 && Criptomonedas[i,3] <= 30){
        #Almacenado la capacidad de la criptomoneda que cumple con la condición
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
    #[Finaliza loop]
  }
  #Creando al cuarta condicional acorde a un rango de 30 - 40%
  else if(a == "30 - 40%"){
    #[Inicia loop]
    for (i in 1:nrow(Criptomonedas)) {
      #Estableciendo la condicional que busca la criptomoneda que se encuentre dentro del rango
      if(Criptomonedas[i,3] >= 30 && Criptomonedas[i,3] <= 40){
        #Almacenando la capacidad de la criptomoneda que cumple con la condición
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
    #[Finaliza loop]
  }
  #Creando la quinta condicional acorde a un rango de 40 - 50%
  else if(a == "40 - 50%"){
    #[Inicia loop]
    for (i in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda que se encuentre dentro del rango
      if(Criptomonedas[i,3] >= 40 && Criptomonedas[i,3] <= 50){
        #Almacenando la capacidad de la criptomoneda que cumple con la condición
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
    #[Finaliza loop]
  }
  #Estableciendo la sexta condicional acorde a un rango de 50 - 60%
  else if(a == "50 - 60%"){
    #[Inicia loop]
    for (i in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda que se encuentre dentro del rango
      if(Criptomonedas[i,3] >= 50 && Criptomonedas[i,3] <= 60){
        #Almacenando la capacidad de la criptomoneda que cumple con la condición
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
    #[Finaliza loop]
  }
  #Estableciendo la séptima condicional acorde a un rango de 60 - 70%
  else if(a == "60 - 70%"){
    #[Inicia loop]
    for (i in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda que se encuentre dentro del rango
      if(Criptomonedas[i,3] >= 60 && Criptomonedas[i,3] <= 70){
        #Almacenando la capacidad de la criptomoneda que cumple con la condición
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
    #[Finaliza loop]
  }
  #Estableciendo la octava condicional acorde a un rango de 70 - 80%
  else if(a == "70 - 80%"){
    #[Inicia loop]
    for (i in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda que se encuentre dentro del rango
      if(Criptomonedas[i,3] >= 70 && Criptomonedas[i,3] <= 80){
        #Almacenando la capacidad de la criptomoneda que cumple con la condición
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
    #[Finaliza loop]
  }
  #Estableciendo la novena condicional acorde a un rango de 80 - 90%
  else if(a == "80 - 90%"){
    #[Inicia loop]
    for (i in 1:nrow(Criptomonedas)) {
      #Estableciendo la condicional que busca la criptomoneda que se encuentre dentro del rango
      if(Criptomonedas[i,3] >= 80 && Criptomonedas[i,3] <= 90){
        #Almacenando la capacidad de la criptomoneda que cumple con la condición
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
    #[Finaliza loop]
  }
  #Estableciendo la décima condicional acorde a un rango de 90 - 100%
  else if(a == "90 - 100%"){
    #[Inicia loop]
    for (i in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda que se encuentre dentro del rango
      if(Criptomonedas[i,3] >= 90 && Criptomonedas[i,3] <= 100){
        #Almacenando la capacidad de la criptomoneda que cumple con la condición
        ChosenCapacity <- c(ChosenCapacity,Criptomonedas[i,3])
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        CriptCapacity <- c(CriptCapacity,Criptomonedas[i,1])
      }
    }
    #[Finaliza loop]
  }
  #Imprimiendo resultado
  print(paste("La criptomoneda con con una capacidad de mercado entre", a, "es:", CriptCapacity, "específicamente con una capacidad de un", ChosenCapacity, "%"))
}

#Probando la función
Cript("40 - 50%")

#Se crea función que obtiene las criptomonedas acorde a los rangos establecidos de transacciones por segundos
#Transg: Entrada: String -> Salida: String
#Ejemplo: Entrada: 2000 - 2999 -> Salida: La criptomoneda EOS (EOS) con 2800 transacciones por segundo se encuentra en el rango seleccionado de 2000 - 2999

#Creando función
Transg <- function(rango){
  #Creando vector que almacenará las transacciones por segundo de la criptomoneda que corresponde al rango establecido
  SelectedTran <- c()
  #Creando vector que almacenará el nombre de la criptomoneda correspondiente a las transacciones por segundo
  CorrespondentCript <- c()
  #Eliminando filas con valores NA
  Criptomonedas <- Criptomonedas[-c(4,6,8,9,10,12,14),]
  #Estableciendo la primera condicional acorde a un rango de 0 - 999
  if(rango == "0 - 999"){
    #[Inicia loop]
    for (i in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda que se encuentre dentro del rango
      if(Criptomonedas[i,7] >= 0 && Criptomonedas[i,7] <= 999){
        #Almacenando las transacciones por segundo de la criptomoneda que cumple con la condición
        SelectedTran <- c(SelectedTran,Criptomonedas[i,7])
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        CorrespondentCript <- c(CorrespondentCript,Criptomonedas[i,1])
      }
    }
    #[Finaliza loop]
  }
  #Estableciendo la segunda condicional acorde a un rango de 1000 - 1999
  else if(rango == "1000 - 1999"){
    #[Inicia loop]
    for (i in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda que se encuentre dentro del rango
      if(Criptomonedas[i,7] >= 1000 && Criptomonedas[i,7] <= 1999){
        #Almacenando las transacciones por segundo de la criptomoneda que cumple con la condición
        SelectedTran <- c(SelectedTran,Criptomonedas[i,7])
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        CorrespondentCript <- c(CorrespondentCript,Criptomonedas[i,1])
      }
    }
    #[Finaliza loop]
  }
  #Estableciendo al tercera condicional acorde a un rango de 2000 - 2999
  else if(rango == "2000 - 2999"){
    #[Inicia loop]
    for (i in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda que se encuentre dentro del rango
      if(Criptomonedas[i,7] >= 2000 && Criptomonedas[i,7] <= 2999){
        #Almacenando las transacciones por segundo de la criptomoneda que cumple con la condición
        SelectedTran <- c(SelectedTran,Criptomonedas[i,7])
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        CorrespondentCript <- c(CorrespondentCript,Criptomonedas[i,1])
      }
    }
    #[Finaliza loop]
  }
  #Imprimiendo resultado
  print(paste("La criptomoneda", CorrespondentCript, "con", SelectedTran,"transacciones por segundo se encuentra en el rango seleccionado de", rango))
}

#Probando la función

Transg("2000 - 2999")

#Se crea una función que obtiene la criptomoneda con menor o mayor tiempo en segundos requeridos por bloque acorde a lo indicado
#TimeBloque: Entrada: String -> Salida: String
#Ejemplo: Entrada: Mayor -> Salida: La criptomoneda con menor tiempor requerido por bloque es: RIPPLE (XRP) , con un tiempo de 0.1 segundos

#Creando la función
TimeBloque <- function(Tiempo){
  #Eliminando filas con valores NA
  Criptomonedas <- Criptomonedas[-c(4,6,8,9,10,12,14),]
  #Estableciendo condicional ante la elección de la criptomoneda con el mayor tiempo
  if(Tiempo == "Mayor"){
    #Obteniendo el tiempo máximo
    Mayor <- max(Criptomonedas$`Tiempo para un bloque (sg)`)
    #Creando vector que almacenará la criptomoneda asociada al tiempo máximo
    Cmayor <- c()
    #[Inicia loop]
    for (f in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda con mayor tiempo
      if(Criptomonedas[f,9] == Mayor)
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        Cmayor <- c(Cmayor,Criptomonedas[f,1])
    }
    #[Finaliza loop]
    #Imprimiendo resultado
    print(paste("La criptomoneda con mayor tiempo requerido por bloque es:", Cmayor, ", con un tiempo de", Mayor, "segundos"))
  }
  #Estableciendo condicional ante la elección de la criptomoneda con el menor tiempo
  else if(Tiempo == "Menor"){
    #Obteniendo el tiempo mínimo
    Menor <- min(Criptomonedas$`Tiempo para un bloque (sg)`)
    #Creando vector que almacenará la criptomoneda asociada al tiempo mínimo
    Cmenor <- c()
    #[Inicia loop]
    for (c in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda con el menor tiempo
      if(Criptomonedas[c,9] == Menor)
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        Cmenor <- c(Cmenor,Criptomonedas[c,1])
    }
    #[Finaliza loop]
    #Imprimiendo resultado
    print(paste("La criptomoneda con menor tiempor requerido por bloque es:", Cmenor, ", con un tiempo de", Menor, "segundos"))
  }
}

#Probando la función

TimeBloque("Menor")

#Se crea una función que obtiene la criptomoneda con la cantidad máxima mayor o menor
#Mill: Entrada: String -> Salida: String
#Ejemplo: Entrada: Mayor -> Salida: La criptomoneda con una mayor Cantidad Maxima es : Chainlink (LINK) , con US (M) 1e+06

#Se crea la función
Mill <- function(Mil){
  #Se elimina la fila que contine el NA 
  Criptomonedas <- Criptomonedas[-c(2),]
  #Se establece la primera condicional
  if(Mil == "Mayor"){
    #Se obtiene la variación maxima
    Ma <- max(Criptomonedas$`Cantidad máxima ($) (M)`)
    #Creando vector que almacenará el nombre de la criptomoneda con la variación máxima
    Cma <- c()
    #Se inicia el LOOP 
    for (i in 1:nrow(Criptomonedas)) {
      #Estableciendo la condicional que busca la criptomoneda correspondiente a la variación máxima
      if(Criptomonedas[i,4] == Ma)
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        Cma <- c(Cma,Criptomonedas[i,1])
    }
    #Finaliza Loop 
    #Se imprime el resultado
    print(paste("La criptomoneda con una mayor cantidad máxima es :", Cma, ", con US (M)", Ma))
  }
  #Estableciendo la segunda condicional
  else if(Mil == "Menor"){
    #Obteniendo la variación mínima
    Mi <- min(Criptomonedas$`Cantidad máxima ($) (M)`)
    #Creando vector que almacenará el nombre de la criptomoneda con la variación mínima
    Cmi <- c()
    #Inicia un Loop
    for (b in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda correspondiente a la variacion mínima
      if(Criptomonedas[b,4] == Mi)
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        Cmi <- c(Cmi,Criptomonedas[b,1])
    }
    #Finaliza Loop 
    #Se imprime el resultado
    print(paste("La criptomoneda con una menor cantidad máxima es :", Cmi, ", con US (M)", Mi))
  }
}

#Probando la función.
Mill("Mayor")

#Creando funcion que obtiene la criptomoneda con la mayor o menor cantidad en cirulación
#Circulacion: Entrada: string -> Salida: string 
#Ejemplo:Entrda:Menor ->Salida: La criptomoneda con una menor Cantidad en circulacion es : BITCOIN (BTC) , con  (>M) 17

#Se crea la función
Circulacion <- function(Circulacion){
  #se eliminan las filas que contine el NA 
  Criptomonedas <- Criptomonedas[-c(4,6,8,9,10,12,14),]
  #Se establece la primera condicional
  if(Circulacion == "Mayor"){
    #Se obtiene la variación maxima
    MaC <- max(Criptomonedas$`Cantidad en circulación (>M)`)
    #Creando vector que almacenará el nombre de la criptomoneda con la variación máxima
    CmaC <- c()
    #Se inicia el LOOP
    for (i in 1:nrow(Criptomonedas)) {
      #Estableciendo la condicional que busca la criptomoneda correspondiente a la variación máxima
      if(Criptomonedas[i,5] == MaC)
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        CmaC <- c(CmaC,Criptomonedas[i,1])
    }
    #Finaliza Loop 
    #Se imprime el resultado
    print(paste("La criptomoneda con una mayor  :", CmaC, ", con (>M)", MaC))
  }
  #Estableciendo la segunda condicional
  else if(Circulacion == "Menor"){
    #Obteniendo la variación mínima
    MiC <- min(Criptomonedas$`Cantidad en circulación (>M)`)
    #Creando vector que almacenará el nombre de la criptomoneda con la variación mínima
    CmiC <- c()
    #Inicia un Loop
    for (b in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda correspondiente a la variacion mínima
      if(Criptomonedas[b,5] == MiC)
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        CmiC <- c(CmiC,Criptomonedas[b,1])
    }
    #Finaliza Loop 
    #Se imprime el resultado
    print(paste("La criptomoneda con una menor Cantidad en circulacion es :", CmiC, ", con  (>M)", MiC))
  }
}
## Probando la función 
Circulacion("Menor")

# Se crea la función de Precio (USD).
# Se desarrolla una función respecto a la tabla general llamada "Criptomonedas" que recibe una variable encontrada como condición en intervalos personalizados específicamente y designados que se encuentran 
# sujetos a recorrer la columna 10 (Precio (USD)) identificada por un for correspondiente, reconociendo a la vez su columna 1 como base.
# Es entonces que dará como resultado el precio (USD) que se encuentra en el intervalo que se solicite.
# PrecioUSD: String -> String
# Ejemplo: Entra: "0 - 100" -> Salida: "La criptomoneda que tiene un precio (USD) entre 0 - 100 es: RIPPLE (XRP) representando específicamente un precio de 0.23314 USD" ...

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
# ValorHr: String -> String
# Ejemplo: Entra: "0 - 1000" -> Salida: La criptomoneda que tiene una variación (24h) ($) entre 0 - 1000 es: Binance Coin (BNB) representando una variación (24h) de 750.39 ($) (M)" ...

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
# VolTotalPorcentaje: String -> String
# Ejemplo: Entra: "0 - 10%" -> Salida: "La criptomoneda con vol. total (%) entre 0 - 10% es: RIPPLE (XRP) especificamente con un vol. total de 1.74 %" ...

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

#Se crea un función que obtiene la criptomoneda con la mayor o menor variación en 7 días (%)
#variacionDiasPorcentaje: Entrada: String -> Salida: String
#Ejemplo: Entrada: Mayor -> Salida: La criptomoneda con una mayor variación en 7 días (%) es  : Bitcoin Cash (BCH) , con  82.44

#Creando la función
variacionDiasPorcentaje <- function(variacion){
  #Eliminando filas con valores NA
  Criptomonedas <- Criptomonedas[-c(11,12,13,14,15),]
  #Estableciendo la primera condicional
  if(variacion == "Mayor"){
    #Obteniendo la variación máxima
    vad <- max(Criptomonedas$`Var. (7d) (%)`)
    #Creando vector que almacenará el nombre de la criptomoneda con la variación máxima
    vac <- c()
    #[Inicia loop]
    for (yy in 1:nrow(Criptomonedas)) {
      #Estableciendo la condicional que busca la criptomoneda correspondiente a la variación máxima
      if(Criptomonedas[yy,14] == vad)
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        vac <- c(vac,Criptomonedas[yy,1])
    }
    #[Finaliza loop]
    #Imprimiendo resultado
    print(paste("La criptomoneda con una mayor variación en 7 días (%) es  :", vac, ", con ", vad))
  }
  #Estableciendo la segunda condicional
  else if(variacion == "Menor"){
    #Obteniendo la variación mínima
    vid <- min(Criptomonedas$`Var. (7d) (%)`)
    #Creando vector que almacenará el nombre de la criptomoneda con la variación mínima
    vic <- c()
    #[Inicia loop]
    for (ee in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda correspondiente a la variacion mínima
      if(Criptomonedas[ee,14] == vid)
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        vic <- c(vic,Criptomonedas[ee,1])
    }
    #[Finaliza loop]
    #Imprimiendo resultado
    print(paste("La criptomoneda con una menor variación en 7 días (%) es :", vic, ", con ", vid))
  }
}

#Probando la función 
variacionDiasPorcentaje("Mayor")

#Se crea un función que obtiene la criptomoneda con menor o mayor variación en 24 horas (%)
#Varhr: Entrada: String -> Salida: String
#Ejemplo: Entrada: Mayor -> Salida: La criptomoneda con la mayor variación en 24 horas es: LITECOIN (LTC, L) con una variación de un 17.52 %

#Creando la función
Varhr <- function(Coin){
  #Eliminando las filas que contienen NA
  Criptomonedas <- Criptomonedas[-c(11,12,13,14,15),]
  #Estableciendo la primera condicional
  if(Coin == "Mayor"){
    #Obteniendo la variación máxima
    Biggest <- max(Criptomonedas$`Var. (24h) (%)`)
    #Creando vector que almacenará el nombre de la criptomoneda correspondiente a la variación máxima
    MonedaMa <- c()
    #[Inicia loop]
    for (z in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda con la variación mázima
      if(Criptomonedas[z,13] == Biggest){
        #Almacenando el nombre de la criptomoneda que cumple con la condición
        MonedaMa <- c(MonedaMa,Criptomonedas[z,1])
      }
    }
    #[Finaliza loop]
    #Imprimiendo resultado
    print(paste("La criptomoneda con la mayor variación en 24 horas es:", MonedaMa, "con una variación de un", Biggest, "%"))
  }
  #Estableciendo segunda condicional
  else if(Coin == "Menor"){
    #Obteniendo la variación mínima
    Smaller <- min(Criptomonedas$`Var. (24h) (%)`)
    #Creando vector que almacenará la criptomoneda correspondiente a la variación mínima
    MonedaMe <- c()
    #[Inicia loop]
    for (z in 1:nrow(Criptomonedas)) {
      #Estableciendo condicional que busca la criptomoneda con la variación mínima
      if(Criptomonedas[z,13] == Smaller){
        #Almacenando el nombre de la criptomoneda que cumple con la condicón
        MonedaMe <- c(MonedaMe,Criptomonedas[z,1])
      }
    }
    #[Finaliza loop]
    #Imprimiendo resultado
    print(paste("La criptomoneda con la menor variación en 24 horas es:", MonedaMe, "con una variación de un", Smaller, "%"))
  }
}

#Probando la función
Varhr("Mayor")

#Se crea función que obtiene la moneda correspondienten al ratio de minado/emision solicitado
#Mining: Entrada: String -> Salida: String
#Ejemplo: Entrada: 1.000 millones al mes -> Salida: La criptomoneda con el ratio de minado/emision de 1.000 millones al mes es: RIPPLE (XRP)

#Creando función
Mining <- function(h){
  #Estableciendo la primera condicional
  if(h == "12,5 por bloque"){
    #Imprimiendo el resultado
    print(paste("Las criptomonedas con el ratio de minado/emision de", h, "son:",Criptomonedas[1,1] ,"y",Criptomonedas[7,1] ))
  }
  #Estableciendo la segunda condicional
  else if(h == "3 por bloque"){
    #Imprimiendo el resultado
    print(paste("La criptomoneda con el ratio de minado/emision de", h, "es:",Criptomonedas[2,1]))
  }
  #Estableciendo la tercera condicional
  else if(h == "1.000 millones al mes"){
    #Imprimiendo el resultado
    print(paste("La criptomoneda con el ratio de minado/emision de", h, "es:",Criptomonedas[3,1]))
  }
  #Estableciendo la cuarta condicional
  else if(h == "25 por bloque"){
    #Imprimiendo el resultado
    print(paste("La criptomoneda con el ratio de minado/emision de", h, "es:",Criptomonedas[5,1]))
  }
  #Estableciendo la quinta condicional
  else if(h == "Hasta un 5% de inflación por año"){
    #Imprimiendo el resultado
    print(paste("La criptomoneda con el ratio de minado/emision de", h, "es:",Criptomonedas[11,1]))
  }
  #Estableciendo la sexta condicional
  else if(h == "Hasta un 1% de inflación por año"){
    #Imprimiendo el resultado
    print(paste("La criptomoneda con el ratio de minado/emision de", h, "es:",Criptomonedas[13,1]))
  }
  #Estableciendo la séptima condicional
  else if(h == "Hasta 15 millones por año"){
    #Imprimiendo el resultado
    print(paste("La criptomoneda con el ratio de minado/emision de", h, "es:",Criptomonedas[15,1]))
  }
  #Estableciendo opción ante situación en la que se seleccione una opción no existente
  else{
    #Imprimiendo mensaje
    print("No hay criptomoneda alguna con el ratio de minado/emision inidicado")
  }
}

#Probando la función
Mining("1.000 millones al mes")

############################################### CREANDO FUNCION FINAL #####################################################

#Se crea una función que contiene las subfunciones previas en base a la cual se devuelven las criptomonedas que cumplan con el criterio de filtro establecido
#Filtrar: Entrada: String,String -> Salida: String
#Ejemplo: Entrada: capacidad de mercado,40 - 50% -> Salida:La criptomoneda con con una capacidad de mercado entre 40 - 50% es: BITCOIN (BTC) especificamente con una capacidad de mercado de un 44 %

#Creando la función
Filtrar <- function(variable,opcion){
  #Almacenando segunda variable
  y <- opcion
  #Estableciendo condicional acorde al año de lanzamiento
  if(variable == "año"){
    #Llamando la función correspondiente
    Año(y)
  }
  #Estableciendo condicional acorde a la capacidad de mercado
  else if(variable == "capacidad de mercado"){
    #Llamando la función correspondiente
    Cript(y)
  }
  #Estableciendo condicional acorde a la cantidad máxima
  else if(variable == "Cantidad máxima $M"){
    #Llamando la función correspondiente
    Mill(y)
  }
  #Estableciendo la condicional acorde a la cantidad de circulación
  else if(variable == "Cantidad en circulación"){
    #Llamando la función correspondiente
    Circulacion(y)
  }
  #Estableciendo la condicional acorde al ratio de minado/emisión
  else if(variable == "Ratio de minado/emisión"){
    #Llamando la función correspondiente
    Mining(y)
  }
  #Estableciendo la condicional acorde a las transacciones por segundo
  else if(variable == "Transacciones por segundo"){
    #Llamando la función correspondiente
    Transg(y)
  }
  #Estableciendo la condicional acorde al tiempo para el minado de un bloque
  else if(variable == "Tiempo para el minado de un bloque"){
    #Llamando la función correspondiente
    TimeBloque(y)
  }
  #Estableciendo la condicional acorde al precio
  else if(variable == "Precio USD"){
    #Llamando la función correspondiente
    PrecioUSD(y)
  }
  #Estableciendo la condicional acorde al volumen en 24 horas
  else if(variable == "Volumen 24hr $M"){
    #Llamando la función correspondiente
    valorHr(y)
  }
  #Estableciendo la condicional acrode al volumen total
  else if(variable == "Volumen total %"){
    #Llamando la función correspondiente
    volTotalPorcentaje(y)
  }
  #Estableciendo la condicional acorde a la variación en 24 horas
  else if(variable == "Variación 24hr %"){
    #Llamando la función correspondiente
    Varhr(y)
  }
  #Estableciendo la condicional acorde a la variación en 7 días
  else if(variable == "Variación 7d %"){
    #Llamando la función correspondiente
    variacionDiasPorcentaje(y)
    #Estableciendo la ultima opción en caso de que el filtro indicado no sea posible realizar
  }else{
    #Imprimiendo mensaje
    print("No es posible filtrar acorde a la opcion indicada")
  }
}

#Probando la funcion
Filtrar("capacidad de mercado","40 - 50%")

################################################ OBTENIENDO ESTADISTICAS ###############################################

#Las estadísticas de la base de datos creada a obtener serán las siguientes:
#Media, moda, mediana, maximo, minimo, desviacion estandar y varianza.
#Estas luego de obtenidas serán almacenadas en una serie de vectores para luego ser unidos en un data.frame para su posterior análisis.

#Instalando y accediendo paquetes para análisis estadístico
install.packages("modeest")
library("modeest")

#Obteniendo las estadísticas correspondiente al Precio (USD)
mediaPrecioUSD <- mean(Criptomonedas$`Precio (USD)`, na.rm = TRUE)
modaPrecioUSD <- mfv(Criptomonedas$`Precio (USD)`, na_rm = TRUE)
medianaPrecioUSD <- median(Criptomonedas$`Precio (USD)`, na.rm = TRUE)
maximoPrecioUSD <- max(Criptomonedas$`Precio (USD)`, na.rm = TRUE)
minimoPrecioUSD <- min(Criptomonedas$`Precio (USD)`, na.rm = TRUE)
desviacionEstandarPrecioUSD <- sd(Criptomonedas$`Precio (USD)`, na.rm = TRUE)
varianzaPrecioUSD <- var(Criptomonedas$`Precio (USD)`, na.rm = TRUE)

#Almacenando resultados en un vector (Nota: se almacena un NA en lugar de la moda debido a que para dicha columna no exite una moda)
EstPrecioUSD <- c(mediaPrecioUSD, NA, medianaPrecioUSD, maximoPrecioUSD, minimoPrecioUSD, desviacionEstandarPrecioUSD, varianzaPrecioUSD)

#Obteniendo las estadísticas correspondientes al Vol. (24h) ($) (M)
mediaVol24h <- mean(Criptomonedas$`Vol. (24h) ($) (M)`, na.rm = TRUE)
modaVol24h <- mfv(Criptomonedas$`Vol. (24h) ($) (M)`)
medianaVol24h <- median(Criptomonedas$`Vol. (24h) ($) (M)`, na.rm = TRUE)
maximoVol24h <- max(Criptomonedas$`Vol. (24h) ($) (M)`, na.rm = TRUE)
minimoVol24h <- min(Criptomonedas$`Vol. (24h) ($) (M)`, na.rm = TRUE)
desviacionEstandarVol24h <- sd(Criptomonedas$`Vol. (24h) ($) (M)`, na.rm = TRUE)
varianzaVol24h <- var(Criptomonedas$`Vol. (24h) ($) (M)`, na.rm = TRUE)

#Almacenando resultados en un vector (Nota: se almacena un NA en lugar de la moda debido a que para dicha columna no exite una moda)
EstVol24h <- c(mediaVol24h, NA, medianaVol24h, maximoVol24h, minimoVol24h, desviacionEstandarVol24h, varianzaVol24h)

#Obteniendo estadísticas correspondientes al Vol. total (%)
mediaVoltotal <- mean(Criptomonedas$`Vol. Total (%)`, na.rm = TRUE)
modaVoltotal <- mfv(Criptomonedas$`Vol. Total (%)`)
medianaVoltotal <- median(Criptomonedas$`Vol. Total (%)`, na.rm = TRUE)
maximoVoltotal <- max(Criptomonedas$`Vol. Total (%)`, na.rm = TRUE)
minimoVoltotal <- min(Criptomonedas$`Vol. Total (%)`, na.rm = TRUE)
desviacionEstandarVoltotal <- sd(Criptomonedas$`Vol. Total (%)`, na.rm = TRUE)
varianzatVoltotal <- var(Criptomonedas$`Vol. Total (%)`, na.rm = TRUE)

#Almacenando resultados en un vector (Nota: se almacena un NA en lugar de la moda debido a que para dicha columna no exite una moda)
EstVoltotal <- c(mediaVoltotal, NA, medianaVoltotal, maximoVoltotal, minimoVoltotal, desviacionEstandarVoltotal, varianzatVoltotal)


#Obteniendo estadísticas correspondientes a la Var. (24h) (%) 
mediaVar24h <- mean(Criptomonedas$`Var. (24h) (%)`, na.rm = TRUE)
modaVar24h <- mfv(Criptomonedas$`Var. (24h) (%)`)
medianaVar24h <- median(Criptomonedas$`Var. (24h) (%)`, na.rm = TRUE)
maximoVar24h <- max(Criptomonedas$`Var. (24h) (%)`, na.rm = TRUE)
minimoVar24h <- min(Criptomonedas$`Var. (24h) (%)`, na.rm = TRUE)
desviacionEstandarVar24h <- sd(Criptomonedas$`Var. (24h) (%)`, na.rm = TRUE)
varianzatVar24h <- var(Criptomonedas$`Var. (24h) (%)`, na.rm = TRUE)

#Almacenando resultados en un vector (Nota: se almacena un NA en lugar de la moda debido a que para dicha columna no exite una moda)
EstVar24h <- c(mediaVar24h, NA, medianaVar24h, maximoVar24h, minimoVar24h, desviacionEstandarVar24h, varianzatVar24h)

#Obteniendo las estadísticas correspondientes a la Var. (7d) (%)
mediaVar7d <- mean(Criptomonedas$`Var. (7d) (%)`, na.rm = TRUE)
modaVar7d <- mfv(Criptomonedas$`Var. (7d) (%)`)
medianaVar7d <- median(Criptomonedas$`Var. (7d) (%)`, na.rm = TRUE)
maximoVar7d <- max(Criptomonedas$`Var. (7d) (%)`, na.rm = TRUE)
minimoVar7d <- min(Criptomonedas$`Var. (7d) (%)`, na.rm = TRUE)
desviacionEstandarVar7d <- sd(Criptomonedas$`Var. (7d) (%)`, na.rm = TRUE)
varianzatVar7d <- var(Criptomonedas$`Var. (7d) (%)`, na.rm = TRUE)

#Almacenando resultados en un vector (Nota: se almacena un NA en lugar de la moda debido a que para dicha columna no exite una moda)
EstVar7d <- c(mediaVar7d, NA, medianaVar7d, maximoVar7d, minimoVar7d, desviacionEstandarVar7d, varianzatVar7d)

#Obteniendo las estadísticas correspondientes al año de lanzamiento
MaxYear <- max(Criptomonedas$`Lanzamiento (año)`)
MinYear <- min(Criptomonedas$`Lanzamiento (año)`)
ModaYear <- mfv(Criptomonedas$`Lanzamiento (año)`)
MedianYear <- median(Criptomonedas$`Lanzamiento (año)`, na.rm = TRUE)
PromedioYear <- mean(Criptomonedas$`Lanzamiento (año)`, na.rm = TRUE)
DesvEstYear <- sd(Criptomonedas$`Lanzamiento (año)`, na.rm = TRUE)
VarYear <- var(Criptomonedas$`Lanzamiento (año)`, na.rm = TRUE)

#Almacenando resultados en un vector
EstLanzamiento <- c(PromedioYear,ModaYear,MedianYear,MaxYear,MinYear,DesvEstYear,VarYear)

#Obteniendo las estadísticas correspondientes a la capacidad de mercado
PromedioCapacity <- mean(Criptomonedas$`Capacidad de mercado (%)`, na.rm = TRUE)
ModaCapacity <- mfv(Criptomonedas$`Capacidad de mercado (%)`, na_rm = TRUE)
MedianCapacity <- median(Criptomonedas$`Capacidad de mercado (%)`, na.rm = TRUE)
MaxCapacity <- max(Criptomonedas$`Capacidad de mercado (%)`, na.rm = TRUE)
MinCapacity <- min(Criptomonedas$`Capacidad de mercado (%)`, na.rm = TRUE)
DesvEstCapacity <- sd(Criptomonedas$`Capacidad de mercado (%)`, na.rm = TRUE)
VarCapacity <- var(Criptomonedas$`Capacidad de mercado (%)`, na.rm = TRUE)

#Almacenando los resultados en un vector
EstCapacidadMercado <- c(PromedioCapacity,ModaCapacity,MedianCapacity,MaxCapacity,MinCapacity,DesvEstCapacity,VarCapacity)

#Obteniendo las estadísticas correspondientes a la cantidad máxima
PromedioQuantityMax <- mean(Criptomonedas$`Cantidad máxima ($) (M)`, na.rm = TRUE)
ModaQuantityMax <- mfv(Criptomonedas$`Cantidad máxima ($) (M)`, na.rm = TRUE)
MedianQuantityMax <- median(Criptomonedas$`Cantidad máxima ($) (M)`, na.rm = T)
MaxQuantityMax <- max(Criptomonedas$`Cantidad máxima ($) (M)`, na.rm = T)
MinQuantityMax <- min(Criptomonedas$`Cantidad máxima ($) (M)`, na.rm = T)
DesvEstQuantityMax <- sd(Criptomonedas$`Cantidad máxima ($) (M)`, na.rm = T)
VarQuantityMax <- var(Criptomonedas$`Cantidad máxima ($) (M)`, na.rm = T)

#Almacenando los resultados en un vector
EstQuantityMax <- c(PromedioQuantityMax,ModaQuantityMax,MedianQuantityMax,MaxQuantityMax,MinQuantityMax,DesvEstQuantityMax,VarQuantityMax)

#Obteniendo las estadísticas correspondientes a la cantidad en circulación
PromedioCircQuant <- mean(Criptomonedas$`Cantidad en circulación (>M)`, na.rm = T)
ModaCircQuantity <- mfv(Criptomonedas$`Cantidad en circulación (>M)`, na_rm = T)
MedianCircQuantity <- median(Criptomonedas$`Cantidad en circulación (>M)`, na.rm = T)
MaxCircQuantity <- max(Criptomonedas$`Cantidad en circulación (>M)`, na.rm = T)
MinCircQuantity <- min(Criptomonedas$`Cantidad en circulación (>M)`, na.rm = T)
DesvEstCircQuantity <- sd(Criptomonedas$`Cantidad en circulación (>M)`, na.rm = T)
VarCircQuantity <- var(Criptomonedas$`Cantidad en circulación (>M)`, na.rm = T)

#Almacenando los resultados en un vector
EstCircQuantity <- c(PromedioCircQuant,ModaCircQuantity,MedianCircQuantity,MaxCircQuantity,MinCircQuantity,DesvEstCircQuantity,VarCircQuantity)

#Obteniendo las estadísticas correspondientes a las transacciones por segundo
PromedioTran <- mean(Criptomonedas$`Transacciones por sg`, na.rm = T)
ModaTran <- mfv(Criptomonedas$`Transacciones por sg`, na_rm = T)
MedianTran <- median(Criptomonedas$`Transacciones por sg`, na.rm = T)
MaxTran <- max(Criptomonedas$`Transacciones por sg`, na.rm = T)
MinTran <- min(Criptomonedas$`Transacciones por sg`, na.rm = T)
DesvEstTran <- sd(Criptomonedas$`Transacciones por sg`, na.rm = T)
VarTran <- var(Criptomonedas$`Transacciones por sg`, na.rm = T)

#Almacenando los resultados en un vector
EstTransactions <- c(PromedioTran,ModaTran,MedianTran,MaxTran,MinTran,DesvEstTran,VarTran)

#Obteniendo las estadísticas correspondientes al tiempo para un bloque
PromedioTime <- mean(Criptomonedas$`Tiempo para un bloque (sg)`, na.rm = T)
ModaTime <- mfv(Criptomonedas$`Tiempo para un bloque (sg)`, na_rm = T)
MedianTime <- median(Criptomonedas$`Tiempo para un bloque (sg)`, na.rm = T)
MaxTime <- max(Criptomonedas$`Tiempo para un bloque (sg)`, na.rm = T)
MinTime <- min(Criptomonedas$`Tiempo para un bloque (sg)`, na.rm = T)
DesvEstTime <- sd(Criptomonedas$`Tiempo para un bloque (sg)`, na.rm = T)
VarTime <- var(Criptomonedas$`Tiempo para un bloque (sg)`, na.rm = T)

#Promediando la moda debido a que se obtuvieron dos
MediaModaTime <- mean(ModaTime)

#Almacenando los resultados en un vector
EstTiempoBloque <- c(PromedioTime,MediaModaTime,MedianTime,MaxTime,MinTime,DesvEstTime,VarTime)

################################################# UNIENDO LAS ESTADÍSTICAS OBTENIDAS ###################################

#Creando data frame con los vectores estadísticos creados
Estadistica <- data.frame(EstLanzamiento,EstCapacidadMercado,EstQuantityMax,EstCircQuantity,EstTransactions,EstTiempoBloque,EstPrecioUSD,EstVol24h,EstVoltotal,EstVar24h,EstVar7d)

#Cambiando filas por columnas 
Estadistica <- as.data.frame(t(Estadistica))

#Creando vector con los nombres de las columnas
Head <- c("Media","Moda","Mediana","Máximo","Mínimo","Desviación Estándar","Varianza")

#Cambiando el nommbre de las columnas
colnames(Estadistica) <- Head

#Creando vector con los nombres de las filas
Fil <- c("Lanzamiento (año)","Capacidad de mercado (%)","Cantidad máxima $M","Cantidad en circulación (>M)","Transacciones por sg","Tiempo para un bloque (sg)","Precio $USD","Volumen $M (24h)","Volumen total (%)","Variación 24h (%)","Variación 7d (%)")

#Cambiando el nombre de las filas
Estadistica <- data.frame(Estadistica, row.names = Fil)

########################################## GRAFICANDO INFORMACIÓN SELECCIONADA ##########################################

#Se seleccionó como información a graficar las siguientes:
#Capacidad de mercado, por medio de un gráfico de torta.
#Tiempo de minado para un bloque, por medio de un gráfico de barras.
#Variación de 7d, por medio de un gráfico de lineas.

#Nota: Se eliminaron las criptomonedas que poseen NA's en la información a graficar.

#Graficando el tiempo de minado para un bloque

#Creando vector que almacena la información de la columna correspondiente
TiempoBloque <- c(Criptomonedas$`Tiempo para un bloque (sg)`)

#Eliminando criptomonedas con valores NA
TiempoBloque <- TiempoBloque[-c(4,6,8,9,10,12,14)]

#Creando vector que almacena los nombres de las criptomonedas correspondientes
Nombres <- c(Criptomonedas$Criptomoneda)

#Eliminando los nombres de las criptomonedas que fueron eliminadas 
Nombres <- Nombres[-c(4,6,8,9,10,12,14)]

#Graficando la información correspondiente
GraficoTiempoBloque <- barplot(height = TiempoBloque, names.arg = Nombres, main = "Tiempo de bloque (sg) por Criptomoneda", ylab = "Tiempo de bloque (sg)", xlab = "Criptomoneda", col = rainbow(8))

#Graficando la capacidad de mercado de las criptomonedas en un grafico de tortas

#Creando vector que almacena los porcentajes de cada criptomoneda
CapacidadMercado <- c(Criptomonedas$`Capacidad de mercado (%)`)

#Eliminando criptomonedas con valores NA
CapacidadMercado <- CapacidadMercado[-15]

#Creando vector con los nombres de las criptomonedas asociadas a los porcentajes
Etiquetas <- c(Criptomonedas$Criptomoneda)

#Eliminando los nombres de las criptomonedas que fueron eliminadas
Etiquetas <- Etiquetas[-15]

#Uniendo los valores con la criptomoneda correspondiente 
Etiquetas <- paste(Etiquetas,CapacidadMercado)

#Agregando el simbolo de porcentaje a los valores
Etiquetas <- paste(Etiquetas, "%", sep = "")

#Graficando la información correspondiente
GraficoCapacidadMercado <- pie(CapacidadMercado, Etiquetas,main = "Capacidad de mercado", sub = "Evaluación de las distintas criptomonedas", col = rainbow(14), cex = 0.7)

#Graficando la variación 7d de las criptomonedas

##Creando variable con las distintas variaciones
Variaciondias <- c(Criptomonedas$`Var. (7d) (%)`)

#Eliminando las criptomonedas que contienen NA
Variaciondias <- Variaciondias[-c(11,12,13,14,15)]

#Creando variable que almacena los nombres de las criptomonedas correspondientes
Names <- c(Criptomonedas$Criptomoneda)

#Eliminando las filas con los nombres de las criptomonedas que fueron eliminadas
Names <- Names[-c(11,12,13,14,15)]

#Editando la variable de nombres para que solo permanezcan las abreviaciones de cada criptomoneda
Names <- gsub("[()]","",Names)
Names <- gsub("BITCOIN","",Names)
Names <- gsub("ETHEREUM","",Names)
Names <- gsub("RIPPLE","",Names)
Names <- gsub("Polkadot","",Names)
Names <- gsub("LITECOIN","",Names)
Names <- gsub("Binance Coin","",Names)
Names <- gsub("Bitcoin Cash","",Names)
Names <- gsub("Tether","",Names)
Names <- gsub("USDt","",Names)
Names <- gsub("Chainlink","",Names)
Names <- gsub("CARDANO","",Names)
Names <- gsub("[,]","",Names)
Names <- trim(Names)

#Creando data frame que contiene los datos para la creación del gráfico
data <- data.frame(Names,Variaciondias)

#Instalando y accediendo paquetes para la creación y edición personalizada del gráfico
install.packages("ggplot2")
library("ggplot2")
install.packages("extrafont")
library("extrafont")

#Graficando la información correspondiente
GraficoVariacion <- ggplot(data, aes(x=Names,y=Variaciondias))+geom_point( size=2, shape=21, fill="white", colour="red") + 
  geom_line(colour = "red",group = FALSE) + 
  theme(text = element_text(size = 10)) +
  ggtitle("Variación % del precio de las distintas criptomonedas en periodos de 7 días") + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Criptomonedas (Abreviación)", y ="Variación (%)")

################################################# EXTRAYENDO BASE DE DATOS ##############################################

#Extrayendo la base de datos principal usada para la creacion de funciones
write.csv(Criptomonedas, file="Base_de_datos_criptomonedas.csv")


attr(Criptomonedas$`Red`, "Descripción") <- "La columna Red se refiere a la organización formada a la cual pertenece la Criptomoneda según su creador o proveedor, permitiendo su funcionamiento."
attr(Criptomonedas$`Tiempo para un bloque (sg)`, "Descripción") <- "La columna Tiempo para un bloque (sg) se refiere al tiempo que un bloque almacena la información financiera sin actualizarse, como las transacciones respecto a la Criptomoneda."
attr(Criptomonedas$`Precio (USD)`, "Descripción") <- "La columna Precio (USD) se refiere al valor monetario en dólares Estadounidenses actuales."
attr(Criptomonedas$`Vol. (24h) ($) (M)`, "Descripción") <- "La columna Vol. (24h) ($) (M) se refiere al volumen de transacciones que obtiene la Criptomoneda en el mercado, esto en millones de dólares que se generan en un periodo de 24 horas. Mientras más alto más activo es el mercado de la Criptomoneda y viceversa."
attr(Criptomonedas$`Vol. Total (%)`, "Descripción") <- "La columna Vol. Total (%) se refiere al volumen porcentual de transacciones total que obtiene la Criptomoneda en el mercado. Mientras más alto más activo es el mercado de la Criptomoneda y viceversa."
attr(Criptomonedas$`Var. (24h) (%)`, "Descripción") <- "La columna Var. (24h) (%) se refiere a la variación porcentual del precio en dólares de la Criptomoneda en un periodo de 24 horas."
attr(Criptomonedas$`Var. (7d) (%)`, "Descripción") <- "La columna Var. (7d) (%) se refiere a la variación porcentual del precio en dólares de la Criptomoneda en un periodo de 7 días."
lapply(Criptomonedas, attr, "Descripción")

#Extrayendo la base de datos que contiene la estadística recopilada en base a la base de datos principal
write.csv(Estadistica, file="Base_de_datos_estadistica_criptomonedas.csv")


attr(Estadistica$Máximo, "Descripción") <- "Columna que contiene el mayor valor alcanzado por el conjunto de datos de las columnas de la tabla llamada Criptomoneda."
attr(Estadistica$Mínimo, "Descripción") <- "Columna que contiene el menor valor alcanzado por el conjunto de datos de las columnas de la tabla llamada Criptomoneda."
attr(Estadistica$Desviación.Estándar, "Descripción") <- "Columna que contiene la desviación estándar por el conjunto de datos de las columnas de la tabla llamada Criptomoneda. Esta indica qué tan dispersos están los datos con respecto a la media, siendo la raíz de la varianza. Mientras mayor sea la desviación estándar, mayor será la dispersión de los datos."
attr(Estadistica$Varianza, "Descripción") <- "Columna que contiene la Varianza por el conjunto de datos de las columnas de la tabla llamada Criptomoneda. Representa la variabilidad del conjunto de datos respecto a su media."
lapply(Estadistica, attr, "Descripción")



