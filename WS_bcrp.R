
#------------------------------------#
#-----Cambiando el Directorio--------#
#------------------------------------#
# Antes de nada, limpiamos el workspace, por si hubiera alg?n dataset o informaci?n cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
wd

# Limpiamos la consola
cat("\014")

library(data.table)
library(foreign) #Para guardar .dta


#---------------------------------------------------------------------------
# LISTA DE TABLAS DEL BCRP
#---------------------------------------------------------------------------

#Datos de Investign
if (!("rvest" %in% installed.packages())) {
  install.packages("rvest")
}
if (!("dplyr" %in% installed.packages())) {
  install.packages("dplyr")
}
library(rvest)
library(dplyr)
library(RSelenium)

# install.packages("RSelenium")

url <- "https://estadisticas.bcrp.gob.pe/estadisticas/series/mensuales/producto-bruto-interno-y-demanda-interna-variaciones-indice-2007"


htmIndices <- xml2::read_html(url)
htmIndices


allTables <- html_nodes(htmIndices, css = "table")
allTables


Tables <- html_table(allTables, fill = TRUE)[[2]]
Tables
names(Tables)[1] <- c('borrar')
Tables

#---------------------------------------------------------------------------
# ANALISIS DE LA DATA
#---------------------------------------------------------------------------


indicador <- "PN01770AM"
FechaFinal <- "20210228"
FechaFinalURL <- paste0(substr(FechaFinal,1,4),"-",
                        substr(FechaFinal,5,6))

URL <- paste0("https://estadisticas.bcrp.gob.pe/estadisticas/series/api/",indicador,
              "/json/2002-1/",FechaFinalURL)
library(jsonlite)
objJson <- jsonlite::fromJSON(URL)

objJson  <- jsonlite::fromJSON(readLines(URL, warn="F"))
dt_BCRP <- data.table(objJson$periods)

dt_BCRP2 <-as.data.frame(lapply(objJson$periods, function(y) gsub("n.d.", "-99999.99", y)))
names(dt_BCRP2) <- c("name",paste("",indicador,"",sep=""))
write.dta(dt_BCRP2, file=paste(indicador,'.dta', sep=""))
write.csv2(dt_BCRP2, file=paste(indicador,'.csv', sep=""))


dt_BCRP <- dt_BCRP2[dt_BCRP2$name %like% c("Dic"),]

#---------------------------------------------------------------------------
# PREDICCIÓN
#---------------------------------------------------------------------------

indicador <- "PN01770AM"
FechaFinal <- "20210228"
FechaFinalURL <- paste0(substr(FechaFinal,1,4),"-",
                        substr(FechaFinal,5,6))

URL <- paste0("https://estadisticas.bcrp.gob.pe/estadisticas/series/api/",indicador,
              "/xml/2002-1/",FechaFinalURL)

library(httr)
library(XML)

objXML <- httr::GET(url = URL)
prsXML <- XML::xmlParse(objXML)
l_XML <- XML::xmlToList(prsXML)

#Ingresar  a la información
l_XML$periods[[1]][[1]]

#Acomodar la información
dt_BCRP <- data.table(t(sapply(l_XML$periods,c)))
dt_BCRP <- data.table(Reduce(f=rbind,l_XML$periods))

colnames(dt_BCRP) <- c(indicador,"periodo")
sapply(dt_BCRP, class)
dt_BCRP <- as.data.frame(dt_BCRP)

dt_BCRP[,indicador] <- as.numeric(dt_BCRP[,indicador])




# install.packages("TSstudio")
library(forecast)
library(TSstudio)

ts.pbi <- ts(data = as.numeric(dt_BCRP[,c(indicador)]),
             start = c(2003,1),
             frequency = 12
)



model.arima <- forecast::auto.arima(ts.pbi,
                                    seasonal = TRUE)

fcst.model.arima <- forecast::forecast(object = model.arima,
                                       h=12,
                                       level = c(30,60,90))

fcst.model.arima$method
plot(fcst.model.arima,
     main = paste("Predicción del Tipo de cambio por ",fcst.model.arima$method,sep = "")
)
test_forecast(actual)



