

library(tidyverse)
library(sjPlot)

options(warn=-1)
setwd('/R/Rwanda_2/')
load(agrep('r2d_df.sig.RData',list.files(recursive = T),value = T))
load('C:/R/Rwanda_2/data/cleaned_files/Rwanda.2.Step_D.RData')
r2d <- Rwanda.2.Step_D
names(r2d) <- make.unique(names(r2d))


# Use --------------------------------------------------------

# Identify variables of interest 

satisfaction <- names(r2d_df_sig)[c(22:25)]


alpha <- r2d_df_sig %>% 
    select(satisfaction,
           coop.works.well = Overall.The.Cooperative.Works.For.My.Best.Interests) %>% 
    mutate_all(funs(ifelse(. %in% c('33','6'),NA,.))) %>% 
    mutate(coop.works.well = ifelse(coop.works.well %in% c(1,2,3),0,1)) %>% 
    mutate_at(vars(satisfaction),funs(ifelse(. %in% c(1,2,3),0,1))) %>% 
    mutate(gender = r2d$gender,
           age = as.numeric(r2d$age),
           coop = r2d$coop)

# dir.create('C:/R/Rwanda_2/markdown/')

# save(alpha,file = 'C:/R/Rwanda_2/markdown/alpha.RData')



