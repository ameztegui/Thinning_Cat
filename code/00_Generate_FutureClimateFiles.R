# Import future climatic data ------------------------------------------

rm(list=ls())

ifn_plots <- read.csv("./data/Plots_IFN3.csv", 
                      stringsAsFactors=FALSE) %>%
  arrange(Codi)


# CCLM 8.5 --------------------------------------------------------------------

path <- "//SERVERPROCESS/Miquel/Datasets/Climate/Products/IFN plots/Projections/CCLM4-8-17/rcp8.5/"
temp <- list()
prec <- list()

for (i in seq_along(ifn_plots$Codi)) {
  load(paste0(path,ifn_plots$Codi[i],".rda"))
  temp[[i]] <- data.frame(temp = clim_month$MeanTemperature) %>%
    rownames_to_column("Date") %>%
    mutate(Year = substr(Date,1,4),
           Month = paste0(substr(Date,6,7)))%>%
    select(-Date) %>%
    spread(key = Month, value = temp) %>%
    select(-Year)
  
  
  # write.table(temp[[i]], file = paste0("./data/climate/temp/temp_", ifn3@data$Id[i],".txt"),
  #             sep="\t")
  
  prec[[i]] <- data.frame(prec = clim_month$Precipitation) %>%
    rownames_to_column("Date") %>%
    mutate(Year = substr(Date,1,4),
           Month = paste0(substr(Date,6,7)))%>%
    select(-Date) %>%
    spread(key = Month, value = prec) %>%
    select(-Year)
  
  # write.table(prec[[i]], file = paste0("./data/climate/prec/prec_", ifn3@data$Id[i],".txt"),
  #             sep="\t")  
}

CCLM_8.5 <- mapply(list, temp=temp, prec=prec, SIMPLIFY = FALSE)
names(CCLM_8.5) <- ifn_plots$Codi

save (CCLM_8.5,
      file="./data/climate/climatic_data.Rdata")


