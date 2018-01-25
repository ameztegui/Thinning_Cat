## In this script we prepare all the files to generate the xml code that SORTIE needs to run
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

