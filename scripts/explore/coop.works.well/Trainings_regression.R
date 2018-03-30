<<<<<<< HEAD
options(warn=-1)
library(tidyverse)
library(sjPlot)
library(kableExtra)
library(knitr)
library(pander)
load("/Users/ElliotMeador/Documents/R/Rwanda_2/data/cleaned_files/r2d_df.sig.RData")
# Helpful Training --------------------------------------------------------
helpful <- agrep('helpful', names(r2d_df_sig), value = T)
alpha <- r2d_df_sig %>%
  rename(coop.works.well =
           Overall.The.Cooperative.Works.For.My.Best.Interests) %>%
  select(helpful, coop.works.well) %>%
  mutate(
    coop.works.well = as.numeric(coop.works.well),
    coop.works.well = ifelse(.$coop.works.well %in% c(1, 2, 3), 0, 1)
  ) %>%
  mutate_at(vars(helpful), funs(ifelse(
    . %in% c('did not attend',
             'Not helpful at all',
             'Not helpful'), 0, 1
  )))

id.lables <- c('Intercept',
  'Gender trainging by NGO',
  'Lead farmer coaching by NGO',
  'Training from lead farmers',
  'Traiing from government agents',
  'Agronomy training from private dealer'
)

model.1.1 <- glm(coop.works.well ~ ., alpha, family = binomial())
texreg::htmlreg(model.1.1,caption = 'Logistic Regression: 1.1',caption.above = T,custom.coef.names = id.lables) %>% print(.) 
=======
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


















>>>>>>> 1b262f434bcf3ed48b2818e4b48f2e4059a50e1d
