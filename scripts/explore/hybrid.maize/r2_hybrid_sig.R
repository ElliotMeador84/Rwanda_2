


library(tidyverse)
options(warn=-1)


grep('Rwanda.2.Step_D.RData',list.files(recursive = T),value = T) %>% load(.)



r2d <- Rwanda.2.Step_D
names(r2d) <- make.unique(names(r2d))

# coop works for my best interests --------------


 sig.maize.seed <- r2d %>% 
    select(strip_percent_na(.,5)) %>% 
    select(strip_n(.,2,10)) %>% 
    map_df(possibly(~chisq.test(.,r2d$maize.seed.Hybrid)$p.value,otherwise = NA)) %>% 
    gather(key,value) %>% 
    filter(value <.05) %>% 
    pull(key)

r2_hybrid_sig <- r2d  %>% 
    select(sig.maize.seed)

attributes(r2_hybrid_sig)$info <- 'r2_hybrid_sig contains variables that have a significant associtation with HYBRID maize seed'

options(warn=0)

save(r2_hybrid_sig,file = 'data/cleaned_files/r2_hybrid_sig.RData')











