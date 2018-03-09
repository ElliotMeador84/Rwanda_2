library(tidyverse)
library(sjPlot)


load('C:/R/Rwanda_2/markdown/alpha.RData')


model_1 <- glm(coop.works.well ~ .,data = alpha[c(1,2,5)],family = binomial())
model_2 <- glm(coop.works.well ~ .,data = alpha[c(1,2,3,4,5)],family = binomial())
model_3 <- glm(coop.works.well ~ .,data = alpha,family = binomial())

dp_labs <- paste('Equation',1:3)

id_labs <- c('Explanation of cooperative rules',
             'Enforcement of cooperative rules',
             'Cooperative communication to members',
             'Members communiation to cooperative',
             'Gender (female is reference)',
             'Age',
             'Coop B',
             'Coop C',
             'Coop D',
             'Coop E')

models <- sjt.lm(list(model_1,model_2,model_3),exp.coef = T,emph.p = T,p.numeric = F,show.r2 = T,show.ci = F,depvar.labels = dp_labs,pred.labels = id_labs)

