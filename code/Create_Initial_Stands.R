rm(list=ls())

library(tidyverse)
library(truncnorm)


## Load the parameter file
condes_pars <- read_tsv("./data/condes_parameters.txt")

## Define the initial quadratic diameter for the stands 
Dg = 12.5

## Calculate N as a function of species, Dg, and Martonne Index, as specified in Aranda et al. 2018, For Ecol Manag
stands <- condes_pars %>%
      mutate(N = as.integer(exp((C0 + C1 * log(Martonne) + 
                                  (E0 + E1* Martonne)*log(Dg))))) %>%
      # We need to create a list column, that stores the dbhs of the N trees
      group_by(Species, Code, Climate,ID) %>%
      summarise (Latitude = Latitude,
                 Martonne = Martonne,
                 Prec = Prec,
                 Temp = Temp,
                 N=N,
                 dbhs = list(rtruncnorm(N, a= 2.5, b =25, Dg,0.17*Dg)))
      
## We need to convert dbhs into diamater distributions, in order to introduce them into SORTIE. Also, some format tweaks are applied
getdiamdist <- function (x) {
      distr <- data.frame(CD=cut(x, breaks = c(0, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5,
                                               37.5, 42.5, 47.5, 70),
                                 labels = c( "5", "10", "15","20", "25","30",
                                             "35", "40","45","50")))  %>%
            group_by(CD)  %>%
            summarise(n=n()) %>%
            complete(CD)  %>%
            #filter(!CD =="5" ) %>%
            replace_na(list(n= 0)) %>%
            spread(CD,n)
}

## We apply that function to our dataframe, produces a list in each row with the diameter distribution
stands$dd <- map(stands$dbhs,getdiamdist)


