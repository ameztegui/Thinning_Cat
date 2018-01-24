library(tidyverse)


load("./data/condes_pars.Rdata")

initial_stand <- function (species, Dg, Martonne) {
     pars <- condes_pars%>%
            filter(Code==species)  %>% 
             mutate(N = exp((C0 + C1 * log(Martonne) + 
                                   (E0 + E1* Martonne)*log(Dg))))
      return(pars$N)
      
}

initial_stand("PISY", 25, 29)
initial_stand("PISY", 25, 61)
initial_stand("PISY", 25, 93)

initial_stand("PIPI", 25, 16)
initial_stand("PIPI", 25, 27.5)
initial_stand("PIPI", 25, 39)

initial_stand("PIHA", 25, 10)
initial_stand("PIHA", 25, 23.5)
initial_stand("PIHA", 25, 37)

initial_stand("PINI", 25, 21)
initial_stand("PINI", 25, 48)
initial_stand("PINI", 25, 75)

initial_stand("PIPN", 25, 18)
initial_stand("PIPN", 25, 50)
initial_stand("PIPN", 25, 82)

