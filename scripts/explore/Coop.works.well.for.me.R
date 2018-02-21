library(tidyverse)

r2d <- Rwanda.2.Step_D


names(r2d) <- make.unique(names(r2d))


look(r2d,75)



# GLMs --------------------------------------------------------------------

 ## coop works for my best interests
 ## 
 ## by proportion income

strip_n <- function(x = a.data_frame,y = NUMBER.to.filter,z = y){
    
x %>% 
      map_df(~n_distinct(.)) %>% 
      gather(key,value) %>% 
      filter(value >= y) %>% 
      filter(value <= z) %>% 
      pull(key) 


}

r2d %>% 
    map_df(~sum(is.na(.))) %>% 
    gather(key,value) %>% 
    mutate(value = value/(nrow(.))*100) %>% 
    filter(value <= 25) %>% 
    pull(key) 

r2d %>% 
    map_df(~sum(is.na(.))) %>% 
    gather(key,value) %>% 
    mutate(value = value/sum(value)) %>% 
    ggplot(aes(value))+
    geom_histogram()

strip_percent_na <- function(x = a.data_frame,y = DECIMAL.to.filter){
x %>% 
    map_df(~sum(is.na(.))) %>% 
    gather(key,value) %>% 
    mutate(value = value/(nrow(.))*100) %>% 
    filter(value <= y) %>% 
    pull(key) 
}


strip_percent_na(r2d,1) %>% class()


strip_n(r2d,2) 


r2d %>% 
    select(strip_n(.,2)) %>% length()
r2d %>% 
    select(two) %>% length()
r2d %>% 
    select(strip_percent_na(.,0)) %>% length()

sig.coop.works.best <- r2d %>% 
    select(strip_percent_na(.,0)) %>% 
    select(strip_n(.,2,10)) %>% 
    map_df(~chisq.test(.,r2d$Overall.The.Cooperative.Works.For.My.Best.Interests)$p.value) %>% 
    gather(key,value) %>% 
    filter(value <.05) %>% 
    pull(key)








r2d_df_sig <- r2d  %>% 
select(sig.coop.works.best)






helpful <- agrep('helpful',names(r2d_df_sig),value = T)


how.help.training_plots <- r2d_df_sig %>% 
    rename(coop.works.well = Overall.The.Cooperative.Works.For.My.Best.Interests) %>% 
    mutate(coop.works.well = as.numeric(coop.works.well)) %>% 
    select(helpful,coop.works.well) %>% 
    gather(key,value,-coop.works.well) %>% 
    mutate(key = gsub('helpful.','',.$key),
           value = fct_collapse(value,
                              'Helpful' = c('Very helpful','Somewhat helpful','Helpful'),
                              'Not helpful' = c('Not helpful','Not helpful at all'),
                              'Did not attend' = 'did not attend'),
           value = fct_relevel(value,c('Did not attend','Not helpful','Helpful'))) %>% 
    group_by(key,value) %>% 
    summarise(average = mean(coop.works.well,na.rm = T)) %>% 
    ggplot(aes(value,average))+
    geom_bar(stat = 'identity',aes(fill = key,color = key),show.legend = F) +
    facet_wrap(~key,scales = 'free')+
    coord_flip() + 
    ggtitle('How help was training?')+
    ylab('Average:The cooperative works well for my interests, Likert (1-5)')























