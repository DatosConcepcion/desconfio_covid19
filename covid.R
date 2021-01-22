paquetes<-c("stringr","stringi", "lubridate", "ggplot2", "dplyr", "plotly", "rgdal", "leaflet", "htmlwidgets", "htmltools", "tidytext", "tidyverse", "wordcloud")

for(i in paquetes){
  if(!require(i, character.only = TRUE)){
    install.packages(i, dependencies=TRUE)
  }
  require(i, character.only = TRUE)  
}

#leo la base de datos
base <- read.csv("Base de datos.csv", sep= ",")

#primera mirada a los datos
View(base)

#chequeo tipo de datos
str(base)

#corrijo los acentos y caracteres extraños
original <- base


#verifico existencia de datos nulos
#hay valores en blanco en lugar de NA. Arreglo eso
base[base==""]<-NA

as.data.frame(apply(is.na(base),2,sum))





for(i in names(base)){
  eval(parse(text=paste0("base$\"",i,"\"<-str_replace_all(stri_trans_general(original$\"",i,"\",id=\"latin-ascii\"), \"[^[:alnum:]]\", \" \")")))
}
names(base)<-stri_trans_general(names(base),id="latin-ascii")

#corrección del tipo de datos
#cambio el formato de fecha de texto a date
base$When.did.you.see.the.claim. <- as.Date(base$When.did.you.see.the.claim., format="%m %d %Y")

#cambio formato de pais a factor
base$Country.1 <- as.factor(base$Country.1)
base$Country.2 <- as.factor(base$Country.2)
base$Country.3 <- as.factor(base$Country.3)
base$Country.4 <- as.factor(base$Country.4)

#cambio organizacion a factor
base$Organization <- as.factor(base$Organization)

#cambio quien posteo a factor
base$Who.said.posted.it. <- as.factor(base$Who.said.posted.it.)

#cambio lenguaje a factor
base$Language.of.your.fact.check <- as.factor(base$Language.of.your.fact.check)

#cambio rating factor
base$Final.rating <- as.factor(base$Final.rating)

#cambio categoría a factor
base$Category <- as.factor(base$Category)

#importo base de paises para extraer continente
paises <- read.csv("https://pkgstore.datahub.io/JohnSnowLabs/country-and-continent-codes-list/country-and-continent-codes-list-csv_csv/data/b7876b7f496677669644f3d1069d3121/country-and-continent-codes-list-csv_csv.csv", sep = "," )
paises$nombre_corto <- stringr::str_split_fixed(paises$Country_Name,",",2)[,1]
paises <- paises[,c("Continent_Name", "nombre_corto")]

#corrijo algunos paises
paises$nombre_corto[paises$nombre_corto == "United Kingdom of Great Britain & Northern Ireland"] <- "United Kingdom"
paises$nombre_corto[paises$nombre_corto == "United States of America"] <- "United States"

#creo cuatro variables continente, 1 por pais
base <- dplyr::left_join(base, paises, by=c("Country.1" = "nombre_corto"), keep= FALSE, suffix=c("1","2"))
base <- dplyr::left_join(base, paises, by=c("Country.2" = "nombre_corto"), keep= FALSE, suffix=c("1","2"))
base <- dplyr::left_join(base, paises, by=c("Country.3" = "nombre_corto"), keep= FALSE, suffix=c("3","4"))
base <- dplyr::left_join(base, paises, by=c("Country.4" = "nombre_corto"), keep= FALSE, suffix=c("3","4"))

#reviso cuales quedaron sin dato
View(base[is.na(base$Continent_Name1),])
View(base[is.na(base$Continent_Name2) & (base$Country.2 != ""),])

#arreglo los paises/areas sin continente
base$Continent_Name1[base$Country.1 == "Middle East"] <- "Asia"
base$Continent_Name2[base$Country.2 == "North Africa"] <- "Africa"

####################################
########## BACK UP DE LA BASE ######
####################################
#auxiliar <- base
#base <- auxiliar


#analisis de fechas evaluadas
min(base$When.did.you.see.the.claim.)
max(base$When.did.you.see.the.claim.)

#analisis por paises
total_x_pais <- rbind(as.data.frame(table(base$Country.1)), as.data.frame(table(base$Country.2)))
total_x_pais <- aggregate(total_x_pais$Freq, by=list(Var1 = total_x_pais$Var1), FUN=sum)

colnames(total_x_pais) <- c("País", "Freq")
total_x_pais <- total_x_pais[total_x_pais$País != "", ]

graf_pais <- ggplot(total_x_pais, aes(x=País, y=Freq, fill=Freq)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ggtitle("Cantidad de casos por país")

plotly::ggplotly(graf_pais)

colnames(total_x_pais) <- c("name", "Freq")

countries_p <- readOGR("C:/Users/Porotos/Documents/Desconfio_Covid/custom.geo.json")

countries_p@data <- left_join(countries_p@data, total_x_pais, by=c("name"))

pal <- colorNumeric("Reds", NULL)

state_popup <- paste0("<strong>Pais: </strong>", 
                      countries_p$name,
                      "<br><strong>Cantidad </strong>", 
                      countries_p$Freq)

library(htmlwidgets)
library(htmltools)


tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 14px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Cantidad de reportes por país")
)  

leaflet(countries_p) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(Freq), 
              popup = state_popup) %>%
  addLegend(pal = pal, values = ~Freq, opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(x))) %>%
  addControl(title, position = "topleft",  className = "map-title") %>%
setView(lng = -0, lat = 0, zoom = 1)




#analisis por Continente
total_x_cont <- rbind(as.data.frame(table(base$Continent_Name1)), as.data.frame(table(base$Continent_Name2)))
total_x_cont <- aggregate(total_x_cont$Freq, by=list(Var1 = total_x_cont$Var1), FUN=sum)

colnames(total_x_cont) <- c("continent", "Freq")

graf_cont <- ggplot(total_x_cont, aes(x=continent, y=Freq, fill=Freq)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ggtitle("Cantidad de casos por continente") +
  xlab("Continente")

plotly::ggplotly(graf_cont)


countries <- readOGR("C:/Users/Porotos/Documents/Desconfio_Covid/custom.geo.json")

countries@data <- left_join(countries@data, total_x_cont, by=c("continent"))

pal <- colorNumeric("Blues", NULL)

leaflet(countries) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(Freq)) %>%
  addLegend(pal = pal, values = ~Freq, opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(x)))



####################################
###### SCRAPPING####################
###################################

#Loading the rvest package
library('rvest')

#Specifying the url for desired website to be scraped
url <- 'https://archive.md/Q4cjr'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section y lo convierto a texto
rank_data_html <- html_nodes(webpage,'description') %>% html_text()

rank_data_html <- html_nodes(webpage, "[old-class=article-body]") %>% html_text()

itemprop="description"

