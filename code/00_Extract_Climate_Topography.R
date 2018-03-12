rm(list=ls())

library(readr)
library(raster)
library(rgdal)
library(tidyverse)
library(ggridges)



# 1) Import position of all IFN Plots ----------------------------------------

ifn3 <- rgdal::readOGR("./data/shapefiles/AllIFN_Points.shp")
ifn3@data$Id <- as.numeric(as.character(ifn3@data$Codi))


# 2) Import MDT and generate aspect and slope --------------------------------

elevation <- raster("//serverdades/fidbosc/Cartografia/Referencia/MDT/MDT30CATb.tif")
slope <- raster("//serverdades/fidbosc/Cartografia/Referencia/MDT/SLOPE30CAT.tif")
aspect <- terrain(elevation, opt = "aspect",unit = "degrees")


# Extract point values from rasters
ifn3@data$elevation <- raster::extract(elevation, ifn3)
ifn3@data$slope <- raster::extract(slope, ifn3)
ifn3@data$aspect <- raster::extract(aspect, ifn3)

# 3) Import historical climatic data (annual) --------------------------------

hist_climate <- read_tsv("./Data/climate/IFN23_climate_1986_2015.txt")

ifn3@data <- ifn3@data %>%
  left_join(hist_climate) %>%
  mutate(Martonne = MAP / (MAT + 10))

# 4) Import Condes Parameters ---------------------------------------------

condes_pars <- read_tsv("./data/condes_parameters.txt")

ifn3@data <- ifn3@data %>%
  left_join(condes_pars) %>%
  arrange(Codi)


write_csv(ifn3@data,  "./data/Plots_IFN3.csv")


# Compute quantiles and plot ridgelines -----------------------------------


# Plot Ridgeline plots
ggplot(ifn3@data, aes(x = Martonne, y = Species, group = Species)) +
  geom_density_ridges(scale = 2, size = 0.75, rel_min_height = 0.005) +
  theme_ridges() +
  scale_x_continuous(limits=c(1, 100), expand = c(0.01, 0))

# Calculate statistics
quantiles <- ifn3@data %>%
  group_by(Species) %>%
  summarise(Q=list(round(quantile(Martonne, c(0.01, 0.05, 0.10, 0.50,
                                              0.90, 0.95, 0.99), na.rm=T), 2)))




