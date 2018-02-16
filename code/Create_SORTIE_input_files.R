## In this script we prepare all the files to generate the xml code that SORTIE needs to run

library(xml2)

## We first create the initial diameter distributions, and then the climate, plot name, management alternatives...

## Let's load the data
source("./code/Create_Initial_Stands.R")

## We can now prepare the lists we need to run next script: one for dd, another for species names, for Code, for Latitude...
stands_dd <-stands$dd
stands_names <- as.list(stands$ID)
stands_species <- as.list(stands$Code)
stands_latitude <- as.list(as.character(stands$Latitude))
stands_prec <- as.list(stands$Prec)
stands_temp <- as.list(stands$Temp)


# Initial densities -------------------------------------------------------

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
      }

## We now create a list with the xml text
sortie_stands <- pmap(list(stands_dd, stands_species),define_densities)



# Latitude ----------------------------------------------------------------
sortie_latitude <- stands_latitude
names(sortie_latitude) <- stands_names


# Thinning Regimes ---------------------------------------------------------

# In this case, we read the harvest regimes from external files, and we store them as objects

sortie_thinning <- list.files(path="D:/Activity/Thinning and CC/Thinning_Cat/data/thinning_regimes/",full.names = T) %>%
  set_names(.,basename(.)) %>%  map(read_xml)


# Climate files -----------------------------------------------------------

load("./Rdata/climatic_data.Rdata")

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
  for (year in 1:100) {
    temp_jan[[year]]<- paste0(" <sc_cimtjanVal ts=\"",year,"\">",
                              df$temp[year,1],"</sc_cimtjanVal>") }
  
  temp_feb <- list()
  for (year in 1:100) {
    temp_feb[[year]]<- paste0(" <sc_cimtfebVal ts=\"",year,"\">",
                              df$temp[year,2],"</sc_cimtfebVal>") }
  
  temp_mar <- list()
  for (year in 1:100) {
    temp_mar[[year]]<- paste0(" <sc_cimtmarVal ts=\"",year,"\">",
                              df$temp[year,3],"</sc_cimtmarVal>") }
  
  temp_apr <- list()
  for (year in 1:100) {
    temp_apr[[year]]<- paste0(" <sc_cimtaprVal ts=\"",year,"\">",
                              df$temp[year,4],"</sc_cimtaprVal>") }
  
  temp_may <- list()
  for (year in 1:100) {
    temp_may[[year]]<- paste0(" <sc_cimtmayVal ts=\"",year,"\">",
                              df$temp[year,5],"</sc_cimtmayVal>") }
  
  temp_jun <- list()
  for (year in 1:100) {
    temp_jun[[year]]<- paste0(" <sc_cimtjunVal ts=\"",year,"\">",
                              df$temp[year,6],"</sc_cimtjunVal>") }
  
  temp_jul <- list()
  for (year in 1:100) {
    temp_jul[[year]]<- paste0(" <sc_cimtjulVal ts=\"",year,"\">",
                              df$temp[year,7],"</sc_cimtjulVal>") }
  
  temp_aug <- list()
  for (year in 1:100) {
    temp_aug[[year]]<- paste0(" <sc_cimtaugVal ts=\"",year,"\">",
                              df$temp[year,8],"</sc_cimtaugVal>") }
  
  temp_sep <- list()
  for (year in 1:100) {
    temp_sep[[year]]<- paste0(" <sc_cimtsepVal ts=\"",year,"\">",
                              df$temp[year,9],"</sc_cimtsepVal>") }
  
  temp_oct <- list()
  for (year in 1:100) {
    temp_oct[[year]]<- paste0(" <sc_cimtoctVal ts=\"",year,"\">",
                              df$temp[year,10],"</sc_cimtoctVal>") }
  
  temp_nov <- list()
  for (year in 1:100) {
    temp_nov[[year]]<- paste0(" <sc_cimtnovVal ts=\"",year,"\">",
                              df$temp[year,11],"</sc_cimtnovVal>") }
  
  temp_dec <- list()
  for (year in 1:100) {
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
  for (year in 1:100) {
    prec_jan[[year]]<- paste0(" <sc_cimpjanVal ts=\"",year,"\">",
                              df$prec[year,1],"</sc_cimpjanVal>") }
  
  prec_feb <- list()
  for (year in 1:100) {
    prec_feb[[year]]<- paste0(" <sc_cimpfebVal ts=\"",year,"\">",
                              df$prec[year,2],"</sc_cimpfebVal>") }
  
  prec_mar <- list()
  for (year in 1:100) {
    prec_mar[[year]]<- paste0(" <sc_cimpmarVal ts=\"",year,"\">",
                              df$prec[year,3],"</sc_cimpmarVal>") }
  
  prec_apr <- list()
  for (year in 1:100) {
    prec_apr[[year]]<- paste0(" <sc_cimpaprVal ts=\"",year,"\">",
                              df$prec[year,4],"</sc_cimpaprVal>") }
  
  prec_may <- list()
  for (year in 1:100) {
    prec_may[[year]]<- paste0(" <sc_cimpmayVal ts=\"",year,"\">",
                              df$prec[year,5],"</sc_cimpmayVal>") }
  
  prec_jun <- list()
  for (year in 1:100) {
    prec_jun[[year]]<- paste0(" <sc_cimpjunVal ts=\"",year,"\">",
                              df$prec[year,6],"</sc_cimpjunVal>") }
  
  prec_jul <- list()
  for (year in 1:100) {
    prec_jul[[year]]<- paste0(" <sc_cimpjulVal ts=\"",year,"\">",
                              df$prec[year,7],"</sc_cimpjulVal>") }
  
  prec_aug <- list()
  for (year in 1:100) {
    prec_aug[[year]]<- paste0(" <sc_cimpaugVal ts=\"",year,"\">",
                              df$prec[year,8],"</sc_cimpaugVal>") }
  
  prec_sep <- list()
  for (year in 1:100) {
    prec_sep[[year]]<- paste0(" <sc_cimpsepVal ts=\"",year,"\">",
                              df$prec[year,9],"</sc_cimpsepVal>") }
  
  prec_oct <- list()
  for (year in 1:100) {
    prec_oct[[year]]<- paste0(" <sc_cimpoctVal ts=\"",year,"\">",
                              df$prec[year,10],"</sc_cimpoctVal>") }
  
  prec_nov <- list()
  for (year in 1:100) {
    prec_nov[[year]]<- paste0(" <sc_cimpnovVal ts=\"",year,"\">",
                              df$prec[year,11],"</sc_cimpnovVal>") }
  
  prec_dec <- list()
  for (year in 1:100) {
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

CCLM_4.5 <- map(CCLM_4.5, climate_scenarios)
CCLM_8.5 <- map(CCLM_8.5, climate_scenarios)
RCA4_4.5 <- map(RCA4_4.5, climate_scenarios)
RCA4_8.5 <- map(RCA4_8.5, climate_scenarios)

CCLM_4.5 <- CCLM_4.5[names(sortie_stands)]
CCLM_8.5 <- CCLM_8.5[names(sortie_stands)]
RCA4_4.5 <- RCA4_4.5[names(sortie_stands)]
RCA4_8.5 <- RCA4_8.5[names(sortie_stands)]


parse_xml <- function (narrative_name, climate, climate_name, names) {
  
  densities <- sortie_stands
  plot_ID <- paste0(names,"_", climate_name)
  latitude <- sortie_latitude
  thinning <- sortie_thinning

  
  y <- read_xml("D:/Aitor/INFORMED_CaseStudy/SORTIE_files/XML_Inputs/Piloto/INFORMED_piloto.xml")
  
  
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
  sortie_planting <- xml_children(y)[[16]]
  
  ## Climate Importer
  sortie_climate <-xml_children(y)[[6]]
  
  ## Output
  sortie_output <- xml_children(y)[[17]]
  sortie_output_name <- xml_children(sortie_output)[[1]]
  
  sortie_short_output <- xml_children(y)[[18]]
  sortie_short_output_name <- xml_children(sortie_short_output)[[1]]
  
  ## Replace each node in the xml file by the object created previously
  xml_text(sortie_plot_title) <- plot_ID
  xml_text(sortie_plot_latitude) <- latitude
  
  sortie_initialDensities <- xml_replace(sortie_initialDensities, densities)
  sortie_harvest <- xml_replace(sortie_harvest,harvest)
  sortie_planting <- xml_replace(sortie_planting,planting)
  sortie_climate <- xml_replace(sortie_climate,climate )
  
  xml_text(sortie_output_name) <- paste0("D:\\Aitor\\INFORMED_CaseStudy\\SORTIE_files\\Results\\INFORMED_",
                                         plot_ID)
  
  xml_text(sortie_short_output_name) <- paste0("D:\\Aitor\\INFORMED_CaseStudy\\SORTIE_files\\Results\\INFORMED_",
                                               plot_ID)
  y
}
