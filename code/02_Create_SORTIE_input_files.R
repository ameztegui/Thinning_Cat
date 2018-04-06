## In this script we prepare all the files to generate the xml code that SORTIE needs to run

rm(list=ls())

library(xml2)
library(tidyverse)

## We first create the initial diameter distributions, and then the climate, plot name, management alternatives...

## Let's load the data
source("./code/01_Generate_Structure.R")



# Extract the desired plots -----------------------------------------------

# Our dataset includes all plots in IFN3 in Catalonia, but we would better filter only thoswe that we want
# Based on species, and some deciles of Martonne index

define_dataset <- function(df, species, quantiles) {
  species <- enquo(species)
  df_sp <- filter(df, Species == !!species)
  
  q <- numeric(length = length(quantiles))
  for (i in seq_along(quantiles)) {
    q[i] <- quantile(df_sp$Martonne, quantiles[i])
  }
  
  df_sp <- df_sp %>%
    slice(c(which.min(abs(Martonne-q[1])),
            which.min(abs(Martonne-q[2])),
            which.min(abs(Martonne-q[3]))))
  
}

## We extract the plots

PIPR_plots <-define_dataset(ifn_plots, "PIPR", c(0.10, 0.5, 0.9))
PIPA_plots <-define_dataset(ifn_plots, "PIPA", c(0.10, 0.5, 0.9))
PIHA_plots <-define_dataset(ifn_plots, "PIHA", c(0.10, 0.5, 0.9))
PINI_plots <-define_dataset(ifn_plots, "PINI", c(0.10, 0.5, 0.9))
PISY_plots <-define_dataset(ifn_plots, "PISY", c(0.10, 0.5, 0.9))
PIUN_plots <-define_dataset(ifn_plots, "PIUN", c(0.10, 0.5, 0.9))
  
# This function takes the list of inventory plots, filters by the defined species of interest
# and extracts the columns specified as field
extract_field <- function (df, species, field) {
  species <- enquo(species)
  field <- enquo(field)
  
  df_sp <- filter(df, Species == !!species)
  var_sp <- df_sp %>% pull(!!field)
  var_sp <- as.list(var_sp)
  names(var_sp) <- df_sp$Codi
  var_sp
}

# Plot Names-------------------------------------------------------

# First,we need to extract the plot names  of all plots for each species
PIPR_names <- extract_field(PIPR_plots, "PIPR", Codi)
PIPA_names <- extract_field(PIPA_plots, "PIPA", Codi)
PIHA_names <- extract_field(PIHA_plots, "PIHA", Codi)
PINI_names <- extract_field(PINI_plots, "PINI", Codi)
PISY_names <- extract_field(PISY_plots, "PISY", Codi)
PIUN_names <- extract_field(PIUN_plots, "PIUN", Codi)

# Initial densities -------------------------------------------------------

# First,we need to extract the diameter distributions of all plots for each species
PIPR_dd <- extract_field(PIPR_plots, "PIPR", dd)
PIPA_dd <- extract_field(PIPA_plots, "PIPA", dd)
PIHA_dd <- extract_field(PIHA_plots, "PIHA", dd)
PINI_dd <- extract_field(PINI_plots, "PINI", dd)
PISY_dd <- extract_field(PISY_plots, "PISY", dd)
PIUN_dd <- extract_field(PIUN_plots, "PIUN", dd)

# This function takes the diameter distribution of each plot and parses it into a text string, 
# so that it can be inserted into the xml file
# The arguments are a dataframe, and the name of the species. Here we take each from a different list

define_densities <- function (df, species)  {
  complete_xml <- as.character()
      
      opening <- 'tr_initialDensities>'
      closing <- "</tr_initialDensities>"
      for (i in 1:length(species)) {
            
            opening_species <- paste0('<tr_idVals whatSpecies="',
                                      species[i],'">')
            
            dens0 <- c("<tr_initialDensity sizeClass=\"s7.5\">0</tr_initialDensity>",
                       '<tr_initialDensity sizeClass="s12.5">0</tr_initialDensity>',
                       '<tr_initialDensity sizeClass="s17.5">0</tr_initialDensity>',
                       '<tr_initialDensity sizeClass="s22.5">0</tr_initialDensity>',
                       '<tr_initialDensity sizeClass="s27.5">0</tr_initialDensity>',
                       '<tr_initialDensity sizeClass="s32.5">0</tr_initialDensity>',
                       '<tr_initialDensity sizeClass="s37.5">0</tr_initialDensity>',
                       '<tr_initialDensity sizeClass="s42.5">0</tr_initialDensity>',
                       '<tr_initialDensity sizeClass="s47.5">0</tr_initialDensity>',
                       '<tr_initialDensity sizeClass="s52.5">0</tr_initialDensity>')
            
            closing_species <- '</tr_idVals>'
            
            dens_numbers <- as.numeric(df[i,])
            dens_string <- str_replace_all(dens0,'0',as.character(dens_numbers))
            dens_char <- paste(dens_string, sep="", collapse="") 
            complete_char <- paste0(opening_species, dens_char, closing_species,collapse = "" )
            complete_xml[i] <- complete_char
      }
      
      if(length(species) == 2) {
            complete_xml <- paste0(complete_xml[1], complete_xml[2])
      } else {
            complete_xml <- complete_xml[1]
      }
      complete_xml <- paste0(opening,complete_xml, closing)
      
      complete_xml
      }

## We now create a list with the xml text
PIPR_densities <- map(PIPR_dd, define_densities, "PIPR")
PIPA_densities <- map(PIPA_dd, define_densities, "PIPA")
PIHA_densities <- map(PIHA_dd, define_densities, "PIHA")
PINI_densities <- map(PINI_dd, define_densities, "PINI")
PISY_densities <- map(PISY_dd, define_densities, "PISY")
PIUN_densities <- map(PIUN_dd, define_densities, "PIUN")


# Latitudes ---------------------------------------------------------------

PIPR_lat <- extract_field(PIPR_plots, "PIPR", Latitude)
PIPA_lat <- extract_field(PIPA_plots, "PIPA", Latitude)
PIHA_lat <- extract_field(PIHA_plots, "PIHA", Latitude)
PINI_lat <- extract_field(PINI_plots, "PINI", Latitude)
PISY_lat <- extract_field(PISY_plots, "PISY", Latitude)
PIUN_lat <- extract_field(PIUN_plots, "PIUN", Latitude)


# Thinning Regimes ---------------------------------------------------------

# In this case, we read the harvest regimes from external files, and we store them as objects

thinning_regimes <- list.files(path="D:/Activity/Thinning_CC/Thinning_Cat/data/thinning_regimes/",full.names = T) %>%
  set_names(.,basename(.)) %>%  map(read_xml)
names(thinning_regimes) <- substr(names(thinning_regimes),1,14)

# Climate files -----------------------------------------------------------

load("./data/climate/climatic_data.Rdata")

PIPR_CCLM_8.5 <- CCLM_8.5[names(CCLM_8.5) %in% PIPR_plots$Codi]
PIPA_CCLM_8.5 <- CCLM_8.5[names(CCLM_8.5) %in% PIPA_plots$Codi]
PIHA_CCLM_8.5 <- CCLM_8.5[names(CCLM_8.5) %in% PIHA_plots$Codi]
PINI_CCLM_8.5 <- CCLM_8.5[names(CCLM_8.5) %in% PINI_plots$Codi]
PISY_CCLM_8.5 <- CCLM_8.5[names(CCLM_8.5) %in% PISY_plots$Codi]
PIUN_CCLM_8.5 <- CCLM_8.5[names(CCLM_8.5) %in% PIUN_plots$Codi]

climate_scenarios <- function(df) {

  intro <- paste0("ClimateImporter2>
                  <sc_ciJanRad>0.0</sc_ciJanRad>
                  <sc_ciFebRad>0.0</sc_ciFebRad>
                  <sc_ciMarRad>0.0</sc_ciMarRad>
                  <sc_ciAprRad>0.0</sc_ciAprRad>
                  <sc_ciMayRad>0.0</sc_ciMayRad>
                  <sc_ciJunRad>0.0</sc_ciJunRad>
                  <sc_ciJulRad>0.0</sc_ciJulRad>
                  <sc_ciAugRad>0.0</sc_ciAugRad>
                  <sc_ciSepRad>0.0</sc_ciSepRad>
                  <sc_ciOctRad>0.0</sc_ciOctRad>
                  <sc_ciNovRad>0.0</sc_ciNovRad>
                  <sc_ciDecRad>0.0</sc_ciDecRad>
                  <sc_ciAWS>0.0</sc_ciAWS>")

  ### Temperature #####

  temp_jan <- list()
  for (year in 1:95) {
    temp_jan[[year]]<- paste0(" <sc_cimtjanVal ts=\"",year,"\">",
                              df$temp[year,1],"</sc_cimtjanVal>") }

  temp_feb <- list()
  for (year in 1:95) {
    temp_feb[[year]]<- paste0(" <sc_cimtfebVal ts=\"",year,"\">",
                              df$temp[year,2],"</sc_cimtfebVal>") }

  temp_mar <- list()
  for (year in 1:95) {
    temp_mar[[year]]<- paste0(" <sc_cimtmarVal ts=\"",year,"\">",
                              df$temp[year,3],"</sc_cimtmarVal>") }

  temp_apr <- list()
  for (year in 1:95) {
    temp_apr[[year]]<- paste0(" <sc_cimtaprVal ts=\"",year,"\">",
                              df$temp[year,4],"</sc_cimtaprVal>") }

  temp_may <- list()
  for (year in 1:95) {
    temp_may[[year]]<- paste0(" <sc_cimtmayVal ts=\"",year,"\">",
                              df$temp[year,5],"</sc_cimtmayVal>") }

  temp_jun <- list()
  for (year in 1:95) {
    temp_jun[[year]]<- paste0(" <sc_cimtjunVal ts=\"",year,"\">",
                              df$temp[year,6],"</sc_cimtjunVal>") }

  temp_jul <- list()
  for (year in 1:95) {
    temp_jul[[year]]<- paste0(" <sc_cimtjulVal ts=\"",year,"\">",
                              df$temp[year,7],"</sc_cimtjulVal>") }

  temp_aug <- list()
  for (year in 1:95) {
    temp_aug[[year]]<- paste0(" <sc_cimtaugVal ts=\"",year,"\">",
                              df$temp[year,8],"</sc_cimtaugVal>") }

  temp_sep <- list()
  for (year in 1:95) {
    temp_sep[[year]]<- paste0(" <sc_cimtsepVal ts=\"",year,"\">",
                              df$temp[year,9],"</sc_cimtsepVal>") }

  temp_oct <- list()
  for (year in 1:95) {
    temp_oct[[year]]<- paste0(" <sc_cimtoctVal ts=\"",year,"\">",
                              df$temp[year,10],"</sc_cimtoctVal>") }

  temp_nov <- list()
  for (year in 1:95) {
    temp_nov[[year]]<- paste0(" <sc_cimtnovVal ts=\"",year,"\">",
                              df$temp[year,11],"</sc_cimtnovVal>") }

  temp_dec <- list()
  for (year in 1:95) {
    temp_dec[[year]]<- paste0(" <sc_cimtdecVal ts=\"",year,"\">",
                              df$temp[year,12], "</sc_cimtdecVal>")
  }

  temp <- paste0("<sc_ciMonthlyTempJan>", paste(temp_jan,collapse=""),"</sc_ciMonthlyTempJan>",
                 "<sc_ciMonthlyTempFeb>", paste(temp_feb,collapse=""),"</sc_ciMonthlyTempFeb>",
                 "<sc_ciMonthlyTempMar>", paste(temp_mar,collapse=""),"</sc_ciMonthlyTempMar>",
                 "<sc_ciMonthlyTempApr>", paste(temp_apr,collapse=""),"</sc_ciMonthlyTempApr>",
                 "<sc_ciMonthlyTempMay>", paste(temp_may,collapse=""),"</sc_ciMonthlyTempMay>",
                 "<sc_ciMonthlyTempJun>", paste(temp_jun,collapse=""),"</sc_ciMonthlyTempJun>",
                 "<sc_ciMonthlyTempJul>", paste(temp_jul,collapse=""),"</sc_ciMonthlyTempJul>",
                 "<sc_ciMonthlyTempAug>", paste(temp_aug,collapse=""),"</sc_ciMonthlyTempAug>",
                 "<sc_ciMonthlyTempSep>", paste(temp_sep,collapse=""),"</sc_ciMonthlyTempSep>",
                 "<sc_ciMonthlyTempOct>", paste(temp_oct,collapse=""),"</sc_ciMonthlyTempOct>",
                 "<sc_ciMonthlyTempNov>", paste(temp_nov,collapse=""),"</sc_ciMonthlyTempNov>",
                 "<sc_ciMonthlyTempDec>", paste(temp_dec,collapse=""),"</sc_ciMonthlyTempDec>")

  ## Precipitation #####

  prec_jan <- list()
  for (year in 1:95) {
    prec_jan[[year]]<- paste0(" <sc_cimpjanVal ts=\"",year,"\">",
                              df$prec[year,1],"</sc_cimpjanVal>") }

  prec_feb <- list()
  for (year in 1:95) {
    prec_feb[[year]]<- paste0(" <sc_cimpfebVal ts=\"",year,"\">",
                              df$prec[year,2],"</sc_cimpfebVal>") }

  prec_mar <- list()
  for (year in 1:95) {
    prec_mar[[year]]<- paste0(" <sc_cimpmarVal ts=\"",year,"\">",
                              df$prec[year,3],"</sc_cimpmarVal>") }

  prec_apr <- list()
  for (year in 1:95) {
    prec_apr[[year]]<- paste0(" <sc_cimpaprVal ts=\"",year,"\">",
                              df$prec[year,4],"</sc_cimpaprVal>") }

  prec_may <- list()
  for (year in 1:95) {
    prec_may[[year]]<- paste0(" <sc_cimpmayVal ts=\"",year,"\">",
                              df$prec[year,5],"</sc_cimpmayVal>") }

  prec_jun <- list()
  for (year in 1:95) {
    prec_jun[[year]]<- paste0(" <sc_cimpjunVal ts=\"",year,"\">",
                              df$prec[year,6],"</sc_cimpjunVal>") }

  prec_jul <- list()
  for (year in 1:95) {
    prec_jul[[year]]<- paste0(" <sc_cimpjulVal ts=\"",year,"\">",
                              df$prec[year,7],"</sc_cimpjulVal>") }

  prec_aug <- list()
  for (year in 1:95) {
    prec_aug[[year]]<- paste0(" <sc_cimpaugVal ts=\"",year,"\">",
                              df$prec[year,8],"</sc_cimpaugVal>") }

  prec_sep <- list()
  for (year in 1:95) {
    prec_sep[[year]]<- paste0(" <sc_cimpsepVal ts=\"",year,"\">",
                              df$prec[year,9],"</sc_cimpsepVal>") }

  prec_oct <- list()
  for (year in 1:95) {
    prec_oct[[year]]<- paste0(" <sc_cimpoctVal ts=\"",year,"\">",
                              df$prec[year,10],"</sc_cimpoctVal>") }

  prec_nov <- list()
  for (year in 1:95) {
    prec_nov[[year]]<- paste0(" <sc_cimpnovVal ts=\"",year,"\">",
                              df$prec[year,11],"</sc_cimpnovVal>") }

  prec_dec <- list()
  for (year in 1:95) {
    prec_dec[[year]]<- paste0(" <sc_cimpdecVal ts=\"",year,"\">",
                              df$prec[year,12], "</sc_cimpdecVal>")
  }

  prec <- paste0("<sc_ciMonthlyPptJan>", paste(prec_jan,collapse=""),"</sc_ciMonthlyPptJan>",
                 "<sc_ciMonthlyPptFeb>", paste(prec_feb,collapse=""),"</sc_ciMonthlyPptFeb>",
                 "<sc_ciMonthlyPptMar>", paste(prec_mar,collapse=""),"</sc_ciMonthlyPptMar>",
                 "<sc_ciMonthlyPptApr>", paste(prec_apr,collapse=""),"</sc_ciMonthlyPptApr>",
                 "<sc_ciMonthlyPptMay>", paste(prec_may,collapse=""),"</sc_ciMonthlyPptMay>",
                 "<sc_ciMonthlyPptJun>", paste(prec_jun,collapse=""),"</sc_ciMonthlyPptJun>",
                 "<sc_ciMonthlyPptJul>", paste(prec_jul,collapse=""),"</sc_ciMonthlyPptJul>",
                 "<sc_ciMonthlyPptAug>", paste(prec_aug,collapse=""),"</sc_ciMonthlyPptAug>",
                 "<sc_ciMonthlyPptSep>", paste(prec_sep,collapse=""),"</sc_ciMonthlyPptSep>",
                 "<sc_ciMonthlyPptOct>", paste(prec_oct,collapse=""),"</sc_ciMonthlyPptOct>",
                 "<sc_ciMonthlyPptNov>", paste(prec_nov,collapse=""),"</sc_ciMonthlyPptNov>",
                 "<sc_ciMonthlyPptDec>", paste(prec_dec,collapse=""),"</sc_ciMonthlyPptDec>")


  close <- "</ClimateImporter2>"

  climate <- paste0(intro, temp, prec, close, collapse = "")

}

PIPR_CCLM_8.5 <- map(PIPR_CCLM_8.5, climate_scenarios)
PIPA_CCLM_8.5 <- map(PIPA_CCLM_8.5, climate_scenarios)
PIHA_CCLM_8.5 <- map(PIHA_CCLM_8.5, climate_scenarios)
PINI_CCLM_8.5 <- map(PINI_CCLM_8.5, climate_scenarios)
PISY_CCLM_8.5 <- map(PISY_CCLM_8.5, climate_scenarios)
PIUN_CCLM_8.5 <- map(PIUN_CCLM_8.5, climate_scenarios)


# Insert xml into SORTIE parameter files ----------------------------------

nharvest <- length(thinning_regimes)

narrative_PIPR <- mapply(list, 
                    densities = rep(PIPR_densities, nharvest),
                    latitudes = rep(PIPR_lat, nharvest),
                    names = apply(expand.grid(PIPR_names,substr(names(thinning_regimes),10,14),
                                              "PIPR"),
                                  1, paste, collapse="_"),
                    harvest = rep(thinning_regimes, length(PIPR_names)),
                    climate = rep (PIPR_CCLM_8.5, nharvest),
                    SIMPLIFY = FALSE)
names(narrative_PIPR) <- map(narrative_PIPR, "names")

narrative_PIPA <- mapply(list, 
                         densities = rep(PIPA_densities, nharvest),
                         latitudes = rep(PIPA_lat, nharvest),
                         names = apply(expand.grid(PIPA_names,substr(names(thinning_regimes),10,14),
                                                   "PIPA"),
                                       1, paste, collapse="_"),
                         harvest = rep(thinning_regimes, length(PIPA_names)),
                         climate = rep (PIPA_CCLM_8.5, nharvest),
                         SIMPLIFY = FALSE)
names(narrative_PIPA) <- map(narrative_PIPA, "names")

narrative_PIHA <- mapply(list, 
                         densities = rep(PIHA_densities, nharvest),
                         latitudes = rep(PIHA_lat, nharvest),
                         names = apply(expand.grid(PIHA_names,substr(names(thinning_regimes),10,14),
                                                   "PIHA"),
                                       1, paste, collapse="_"),
                         harvest = rep(thinning_regimes, length(PIHA_names)),
                         climate = rep (PIHA_CCLM_8.5, nharvest),
                         SIMPLIFY = FALSE)
names(narrative_PIHA) <- map(narrative_PIHA, "names")

narrative_PINI <- mapply(list, 
                         densities = rep(PINI_densities, nharvest),
                         latitudes = rep(PINI_lat, nharvest),
                         names = apply(expand.grid(PINI_names,substr(names(thinning_regimes),10,14),
                                                   "PINI"),
                                       1, paste, collapse="_"),
                         harvest = rep(thinning_regimes, length(PINI_names)),
                         climate = rep (PINI_CCLM_8.5, nharvest),
                         SIMPLIFY = FALSE)
names(narrative_PINI) <- map(narrative_PINI, "names")

narrative_PISY <- mapply(list, 
                         densities = rep(PISY_densities, nharvest),
                         latitudes = rep(PISY_lat, nharvest),
                         names = apply(expand.grid(PISY_names,substr(names(thinning_regimes),10,14),
                                                   "PISY"),
                                       1, paste, collapse="_"),
                         harvest = rep(thinning_regimes, length(PISY_names)),
                         climate = rep (PISY_CCLM_8.5, nharvest),
                         SIMPLIFY = FALSE)
names(narrative_PISY) <- map(narrative_PISY, "names")

narrative_PIUN <- mapply(list, 
                         densities = rep(PIUN_densities, nharvest),
                         latitudes = rep(PIUN_lat, nharvest),
                         names = apply(expand.grid(PIUN_names,substr(names(thinning_regimes),10,14),
                                                   "PIUN"),
                                       1, paste, collapse="_"),
                         harvest = rep(thinning_regimes, length(PIUN_names)),
                         climate = rep (PIUN_CCLM_8.5, nharvest),
                         SIMPLIFY = FALSE)
names(narrative_PIUN) <- map(narrative_PIUN, "names")


parse_xml <- function (narrative, narrative_name, climate_name) {

  densities <- narrative$densities
  plot_ID <- paste0(narrative$names,"_",
                    climate_name,
                   "_",
                   narrative_name)
  latitude <- narrative$latitude
  climate <- narrative$climate
  harvest <- narrative$harvest

y <- read_xml("./data/SORTIE_piloto.xml")
  
  
  ## Name a series of nodes that we want to substitute
  
  ## Plot Definition
  sortie_plot <- xml_children(y)[[1]]
    sortie_plot_title <-  xml_children(sortie_plot)[13][[1]]
    sortie_plot_latitude <-  xml_children(sortie_plot)[7][[1]]
  
  ## Trees
  sortie_trees <- xml_children(y)[[2]]
    sortie_speciesList <- xml_children(sortie_trees)[[1]]
    sortie_sizeClasses <- xml_children(sortie_trees)[[2]]
    sortie_initialDensities <- xml_children(sortie_trees)[[3]]
  
  ## Harvest
  sortie_harvest <- xml_children(y)[[5]]
  
  ## Planting
  #sortie_planting <- xml_children(y)[[16]]
  
  ## Climate Importer
   sortie_climate <-xml_children(y)[[6]]
  
  ## Output
  sortie_output <- xml_children(y)[[17]]
  sortie_output_name <- xml_children(sortie_output)[[1]]
  
  sortie_short_output <- xml_children(y)[[18]]
  sortie_short_output_name <- xml_children(sortie_short_output)[[1]]
  
  ## Replace each node in the xml file by the object created previously
    xml_text(sortie_plot_title) <- plot_ID
    xml_text(sortie_plot_latitude) <- as.character(latitude)
    
    sortie_initialDensities <- xml_replace(sortie_initialDensities, densities)
    sortie_harvest <- xml_replace(sortie_harvest,harvest)
    #sortie_planting <- xml_replace(sortie_planting,planting)
    sortie_climate <- xml_replace(sortie_climate,climate )
    
    xml_text(sortie_output_name) <- paste0("D:\\Activity\\Thinning_CC\\Thinning_Cat\\data\\SORTIE_Outputs\\THINNING_",
                                           plot_ID)
    
    xml_text(sortie_short_output_name) <- paste0("D:\\Activity\\Thinning_CC\\Thinning_Cat\\data\\SORTIE_Outputs\\THINNING_",
                                                 plot_ID)
    y
    


}


# Generate the xml files --------------------------------------------------

## PIPR
 PIPR_xml_files <- pmap(list(narrative_PIPR,"PIPR",  "CCLM_8.5"), parse_xml)
  map(seq_along(PIPR_xml_files), function(i){
    write_xml(PIPR_xml_files[[i]],
              file=paste0("D:/Activity/Thinning_CC/Thinning_Cat/data/SORTIE_Inputs/", 
                          names(PIPR_xml_files)[[i]],"_CCLM_85.xml")) })

## PIPA
PIPA_xml_files <- pmap(list(narrative_PIPA,"PIPA",  "CCLM_8.5"), parse_xml)
map(seq_along(PIPA_xml_files), function(i){
  write_xml(PIPA_xml_files[[i]],
            file=paste0("D:/Activity/Thinning_CC/Thinning_Cat/data/SORTIE_Inputs/", 
                        names(PIPA_xml_files)[[i]],"_CCLM_85.xml")) })

## PIHA
PIHA_xml_files <- pmap(list(narrative_PIHA,"PIHA",  "CCLM_8.5"), parse_xml)
map(seq_along(PIHA_xml_files), function(i){
  write_xml(PIHA_xml_files[[i]],
            file=paste0("D:/Activity/Thinning_CC/Thinning_Cat/data/SORTIE_Inputs/", 
                        names(PIHA_xml_files)[[i]],"_CCLM_85.xml")) })

## PINI
PINI_xml_files <- pmap(list(narrative_PINI,"PINI",  "CCLM_8.5"), parse_xml)
map(seq_along(PINI_xml_files), function(i){
  write_xml(PINI_xml_files[[i]],
            file=paste0("D:/Activity/Thinning_CC/Thinning_Cat/data/SORTIE_Inputs/", 
                        names(PINI_xml_files)[[i]],"_CCLM_85.xml")) })

## PISY
PISY_xml_files <- pmap(list(narrative_PISY,"PISY",  "CCLM_8.5"), parse_xml)
map(seq_along(PISY_xml_files), function(i){
  write_xml(PISY_xml_files[[i]],
            file=paste0("D:/Activity/Thinning_CC/Thinning_Cat/data/SORTIE_Inputs/", 
                        names(PISY_xml_files)[[i]],"_CCLM_85.xml")) })

## PIUN
PIUN_xml_files <- pmap(list(narrative_PIUN,"PIUN",  "CCLM_8.5"), parse_xml)
map(seq_along(PIUN_xml_files), function(i){
  write_xml(PIUN_xml_files[[i]],
            file=paste0("D:/Activity/Thinning_CC/Thinning_Cat/data/SORTIE_Inputs/", 
                        names(PIUN_xml_files)[[i]],"_CCLM_85.xml")) })

