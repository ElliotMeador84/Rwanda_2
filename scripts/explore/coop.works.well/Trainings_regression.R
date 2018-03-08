library(tidyverse)
library(knitr)
library(texreg)

options(warn=-1)

load(agrep('r2d_df.sig.RData',list.files(recursive = T),value = T))
# Helpful Training --------------------------------------------------------



helpful <- agrep('helpful', names(r2d_df_sig), value = T)


alpha <- r2d_df_sig %>%
    rename(coop.works.well =
               Overall.The.Cooperative.Works.For.My.Best.Interests) %>%
    mutate(coop.works.well = as.numeric(coop.works.well)) %>%
    select(helpful, coop.works.well) 



beta <- alpha %>% 
    mutate_at(grep('helpful',.),funs(
        ifelse(. %in% c('did not attend','Not helpful','Not helpful at all'),
               0,1))) %>% 
    mutate(coop.works.well = ifelse(coop.works.well %in% c(1,2),0,1)) %>% 
    na.omit(.)

id.vars <- c('Intercept',
             'Gender training by NGO',
             'Coaching by NGO',
             'Agronomy training\nby lead farmers',
             'Agronomy training\nby government',
             'Agronomy training\nby private seller')

options(warn=0)

model1 <- glm(coop.works.well ~ ., data = beta, family = binomial())
title <- 'Logistic regression: Coop works well (dp) and training'


texreg::htmlreg(model1,custom.coef.names = id.vars,caption =title,caption.above = T) %>% print(.)


















