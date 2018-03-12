# Create a leaflet for map
rm(list=ls())

library(leaflet)
library(tidyverse)
library(rgdal)


# Load the data and the shapefiles---------
condes_pars <- read_tsv("./data/condes_parameters.txt")
condes_pars$Code <- as.factor(condes_pars$Code)


# Define icons by color
sps_icons<- awesomeIconList(PIPR = makeAwesomeIcon(icon= 'tree', library= "fa", markerColor = 'blue', iconColor = 'black'),
                     PIHA = makeAwesomeIcon(icon= 'tree', library= "fa", markerColor = 'red', iconColor = 'black'),
                     PIPA = makeAwesomeIcon(icon= 'tree', library= "fa", markerColor = 'orange', iconColor = 'black'),
                     PINI = makeAwesomeIcon(icon= 'tree', library= "fa", markerColor = 'green', iconColor = 'black'),
                     PISY = makeAwesomeIcon(icon= 'tree', library= "fa", markerColor = 'cadetblue', iconColor = 'black'),
                     PIUN = makeAwesomeIcon(icon= 'tree', library= "fa", markerColor = 'purple', iconColor = 'black'))
                     

leaflet(condes_pars) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addAwesomeMarkers(lng = ~Longitude, lat = ~Latitude,
                    popup = paste("Species:", condes_pars$Species, "<br>",
                                  "Climate:", condes_pars$Climate, "<br>",
                                  "Prec:", condes_pars$Prec," mm", "<br>",
                                  "Temp:", condes_pars$Temp, "º C", "<br>",
                                  "Martonne:", condes_pars$Martonne),
                    icon = ~sps_icons[Code]) 


