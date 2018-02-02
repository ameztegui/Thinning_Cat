library(maptools)
library(ggridges)
library(tidyverse)
library(raster)


# Import and load data ----------------------------------------------------

## Import shapes with IFN points
ifn <- readShapePoints("./data/shapefiles/AllIFN_Points.shp")

## Load SMC Interpolator
load("//serverdades/fidbosc/Cartografia/Tematica/Clima SMC/SMC_interpolator.Rda")

## Import MDT and generate slope and aspect

elevation <- raster("//serverdades/fidbosc/Cartografia/Referencia/MDT/MDT30CATb.tif")
slope <- terrain(elevation, opt = "slope",unit = "degrees")
aspect <- terrain(elevation, opt = "aspect",unit = "degrees")


# Calculate meteo data ----------------------------------------------------

# Extract point values from rasters
points_elevation <- extract(elevation, ifn)
points_slope <- extract(slope, ifn)
points_aspect <- extract(aspect, ifn)

# Create spatialPointsTopography Object
points_topo <- SpatialPointsTopography(ifn, points_elevation, points_slope, points_aspect)
crs(points_topo) <- "+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +towgs84=0,0,0"

# Define dates of interest
hist_dates = seq.Date(as.Date("2001-01-01"),as.Date("2016-12-31"), by="day")

# Interpolate climatic data
mp = interpolationpoints(SMC_interpolator, points_topo, dates = hist_dates)

yearly_temp <- summarypoints(mp,var = "MeanTemperature", fun = mean, freq= "year")
yearly_prec <- summarypoints(mp,var = "Precipitation", fun = sum, freq= "year")

mean_temp <- rowMeans(yearly_temp@data, na.rm = T)
mean_prec <- rowMeans(yearly_prec@data, na.rm = T)

martonne <- mean_prec / (mean_temp + 10)

ifn@data <- ifn@data %>%
      mutate(Temp = mean_temp,
             Prec = mean_prec,
             Martonne = martonne)

write_csv(ifn@data,  "./data/nuevos_plots.csv")

# Plot Ridgeline plots
ggplot(ifn@data, aes(x = Martonne, y = Species, group = Species)) +
  geom_density_ridges(scale = 2, size = 0.75, rel_min_height = 0.005) +
  theme_ridges() +
  scale_x_continuous(limits=c(1, 100), expand = c(0.01, 0))

# Calculate statistics
quantiles <- ifn@data %>%
  group_by(Species) %>%
  summarise(Q=list(round(quantile(Martonne, c(0.01, 0.05, 0.10, 0.50,
                                        0.90, 0.95, 0.99), na.rm=T), 2)))



