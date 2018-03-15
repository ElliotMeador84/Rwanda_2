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
