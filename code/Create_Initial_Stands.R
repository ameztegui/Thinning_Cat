library(tidyverse)
library(truncnorm)


condes_pars <- read_tsv("./data/condes_parameters.txt")

## 
stands <- condes_pars %>%
       mutate(N = as.integer(exp((C0 + C1 * log(Martonne) + 
                                  (E0 + E1* Martonne)*log(Dg))))) %>%
      group_by(Species, Code, Climate, Martonne,ID) %>%
      summarise (N=N,
                 dbhs = list(rtruncnorm(N, a= 2.5, b =25, Dg,0.17*Dg)))
      

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

stands$dd <- map(stands$dbhs,getdiamdist)





