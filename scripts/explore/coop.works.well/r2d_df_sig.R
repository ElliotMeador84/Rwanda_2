

library(tidyverse)
options(warn=-1)
if(.Platform$OS.type == "windows") {
load('C:/R/Rwanda_2/data/cleaned_files/Rwanda.2.Step_D.RData')
} 



r2d <- Rwanda.2.Step_D
names(r2d) <- make.unique(names(r2d))

# coop works for my best interests --------------


sig.coop.works.best <- r2d %>% 
    select(strip_percent_na(.,0)) %>% 
    select(strip_n(.,2,10)) %>% 
    map_df(~chisq.test(.,r2d$Overall.The.Cooperative.Works.For.My.Best.Interests)$p.value) %>% 
    gather(key,value) %>% 
    filter(value <.05) %>% 
    pull(key)

r2d_df_sig <- r2d  %>% 
    select(sig.coop.works.best)

attributes(r2d_df_sig)$info <- 'r2d_df_sig contains variables that have a significant associtation with The cooperative works best for my interests'

options(warn=0)

# save(r2d_df_sig,file = 'C:/R/Rwanda_2/data/cleaned_files/r2d_df.sig.RData')