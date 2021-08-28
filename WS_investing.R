rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
wd
getwd()

# Limpiamos la consola
cat("\014")


library(data.table)


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

url <- "https://www.investing.com/currencies/usd-pen-historical-data"


htmIndices <- xml2::read_html(url)
htmIndices


allTables <- html_nodes(htmIndices, css = "table")
allTables

Tables <- html_table(allTables)[[1]]
Tables
head(Tables)

library(rvest)
library(httr)
library(dplyr)
library(lubridate)
library(data.table)
library(tidyverse)

startDate <- as.Date("2010-07-01")
endDate <- Sys.Date() #today

userAgent <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Safari/537.36"
mainUrl <- "https://www.investing.com/currencies/usd-pen-historical-data"

s <- html_session(mainUrl)

  pair_ids <- s %>% 
  html_nodes("div[pair_ids]") %>%
  html_attr("pair_ids")

header <- s %>% html_nodes(".instrumentHeader h2") %>% html_text()

resp <- s %>% rvest:::request_POST(
  "https://www.investing.com/instruments/HistoricalDataAjax",
  add_headers('X-Requested-With'= 'XMLHttpRequest'),
  user_agent(userAgent),
  body = list(
    curr_id = pair_ids,
    header = header[[1]],
    st_date = format(startDate, format="%m/%d/%Y"),
    end_date = format(endDate, format="%m/%d/%Y"),
    interval_sec = "Daily",
    sort_col = "date",
    sort_ord = "DESC",
    action = "historical_data"
  ), 
  encode = "form") %>%
  html_table

print(resp[[1]])
tmp <- print(resp[[1]])


TABSDB <- tmp %>%
  mutate(Periodo = mdy(Date),
         Price = str_replace_all(string=Price, pattern=",", repl="")) %>%
  mutate(Price = as.numeric(as.character(Price)))
TABSDB <-TABSDB[order(TABSDB$Periodo),]
TABSDB <- TABSDB[,c("Periodo","Price")]


looping="PEN"
library(ggplot2)
p1 <- TABSDB %>%
  ggplot( aes(x=as.Date(Periodo), y=Price, color=as.factor(Periodo))) +
  # geom_point(aes(colour = as.factor(AÑO)), size = 0.5, color = "black") +
  labs(title = paste("Tipo de Cambio USD - ",toupper(looping),sep = ""),
       color = "Periodo",
       caption = paste0("Fuente: www.investing.com/currencies \n",
                        "Actualizado al ",max(TABSDB$Periodo),"\n ",
                        "Elaboración propia \n"),
       x="Fecha",
       y=paste(toupper(looping)," por US Dollar",sep = ""))+
  theme(plot.title = element_text(hjust = 0.5))+  # Centrar título
  geom_line(aes(y=Price),size = 1, colour = "red") +
  # geom_line(aes(y=cop),size = 1, colour = "green") +
  scale_x_date(limits = as.Date(c(min(TABSDB$Periodo),max(TABSDB$Periodo)))) 
p1




png(filename=paste("TC_",looping,".png",sep = ""))
plot(p1)
dev.off()

