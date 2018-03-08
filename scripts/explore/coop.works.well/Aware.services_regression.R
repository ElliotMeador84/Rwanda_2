library(tidyverse)

options(warn=-1)

load(agrep('r2d_df.sig.RData',list.files(recursive = T),value = T))
 
aware <- agrep('^aware.', names(r2d_df_sig), value = T)

alpha <- r2d_df_sig %>%
     select(aware, 
            coop.works.well = Overall.The.Cooperative.Works.For.My.Best.Interests) %>%
     mutate(coop.works.well = ifelse(coop.works.well == c(1,2),0,1)) %>% 
    mutate_at(vars(aware),funs(ifelse(. == 'Yes',1,0)))
 
model2 <- glm(coop.works.well ~ ., data = alpha, family = binomial())
summary(model2)
id.vars <- c('Intercept',names(alpha)[-15] %>% 
    str_remove_all('aware.') %>% 
    str_to_title() %>% 
    str_replace_all('\\.',' ') %>% 
    str_wrap(10))

title <- 'Logistic regression: Coop works well (dp) and aware services'

options(warn = 0)

texreg::htmlreg(model2,custom.coef.names = id.vars,caption =title,caption.above = T) %>% print(.)













