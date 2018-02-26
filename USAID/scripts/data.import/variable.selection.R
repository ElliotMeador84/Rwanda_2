
library(tidyverse)

Rwanda.codebook %>% glimpse()

names(Rwanda.codebook) <- names(Rwanda.codebook) %>% 
  str_to_lower() %>% 
  str_replace_all('[^A-z]',' ') %>% 
  str_replace_all(' ','.')
names(Rwanda.codebook)
finder <- function(x){
  y <- agrepl(x,Rwanda.codebook$variable.description..english.)
 k <-  Rwanda.codebook[y,c(2,3,9,5)]
 k
   
}

finder('community')[[2]]
RWANDA_WEAI_HH_PR.csv %>% 
  select(finder('community')[[2]])





