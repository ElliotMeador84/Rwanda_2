library(tidyverse)
<<<<<<< HEAD

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






















=======
>>>>>>> 9233161e361ad1a60065f7d6b9cd060e53694d68

# save(r2d_df_sig,file = paste0(.dir,'/data/cleaned_files/r2d_df.sig.RData'))
load('~/Documents/R/Rwanda_2/data/cleaned_files/r2d_df.sig.RData')



# Helpful Training --------------------------------------------------------



helpful <- agrep('helpful', names(r2d_df_sig), value = T)


how.help.training <- r2d_df_sig %>%
  rename(coop.works.well =
           Overall.The.Cooperative.Works.For.My.Best.Interests) %>%
  mutate(coop.works.well = as.numeric(coop.works.well)) %>%
  select(helpful, coop.works.well) %>%
  gather(key, value,-coop.works.well) %>%
  mutate(
    key = gsub('helpful.', '', .$key),
    value = fct_collapse(
      value,
      'Helpful' = c('Very helpful',
                    'Somewhat helpful', 'Helpful'),
      'Not helpful' = c('Not helpful',
                        'Not helpful at all'),
      'Did not attend' = 'did not attend'
    ),
    value = fct_relevel(value, c('Did not attend',
                                 'Not helpful',
                                 'Helpful')),
    key = fct_recode(
      key,
      'Agronomy training\nfrom government' =
        'agronomy.government',
      'Agronomy training\nfrom lead farmers' = 'agronomy.lead.farmers',
      'Agronomy training\nfrom Seed Co.' = 'agronomy.seed.co',
      'Coaching from\nLand o Lakes' = 'coaching.land.o.lakes',
      'Gender training\n Land o Lakes' = 'gender.land.o.lakes'
    )
  )

how.help.training_a <- how.help.training %>%
  group_by(key, value) %>%
  summarise(
    Value_mean = mean(coop.works.well, na.rm = T),
    n = n(),
    sd = sd(coop.works.well, na.rm = T)
  ) %>%
  mutate(
    se = sd / sqrt(n),
    lower.ci = Value_mean - qt(1 - (0.05 / 2), n - 1) * se,
    upper.ci = Value_mean + qt(1 - (0.05 / 2), n - 1) * se
  )


ggplot(how.help.training_a,
       aes(fct_reorder(key, Value_mean, mean), Value_mean, group = value)) +
  geom_line(
    position = position_dodge(.5),
    aes(color = value),
    alpha = .9,
    show.legend = T
  ) +
  geom_errorbar(
    aes(
      ymin = Value_mean - se,
      ymax = Value_mean + se,
      color = value
    ),
    width = .1,
    position = position_dodge(0.5),
    alpha = .9,
    show.legend = F
  ) +
  geom_point(
    position = position_dodge(0.5),
    aes(color = value, fill = value),
    size = 3,
    shape = 21,
    show.legend = F,
    alpha = .9
  ) +
  theme(legend.position = 'bottom') +
  ggtitle('How helpful was training by how well coop works for my interests') +
  ylab('Average:The cooperative works\nwell for my interests, Likert (1-5)') +
  xlab('')







# ggsave(plot = how.help.training.plots,filename = '/Users/ElliotMeador/Documents/R/Rwanda_2/plots/explore/trainings_coop.works.png',width = 11,height = 8)
