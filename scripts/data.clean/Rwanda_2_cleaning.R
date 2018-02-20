library(tidyverse)
library(haven)
file.edit('C:/R/functions.R')


# Read Database -----------------------------------------------------------


Rwanda_2 <-
    haven::read_spss("/Users/ElliotMeador/Downloads/Rwanda2 SPSS File (3).sav")

Rwanda_2[Rwanda_2 == ' 888.00a '] <- NA


Rwanda_2 <- map_df(Rwanda_2, function(x) {
    str_trim(x)
})

Rwanda_2[Rwanda_2 == '888.00a'] <- NA
Rwanda_2[Rwanda_2 == 999] <- NA



# Rwanda_2 list -----------------------------------------------------------


Rwanda_2_data_steps <- list()
Rwanda_2_data_steps$a <- Rwanda_2 %>%
    mutate_at(vars(Q21.1:Q21.4), funs(fct_collapse(
        .,
        satisfied = c('Very satisfied', 'Satisfied'),
        not.satisfied = c('Somewhat satisfied', 'Not satisfied at all', 'Not satisfied')
    )))



# Q22.1:Q22.20 ------------------------------------------------------------


Rwanda_2_data_steps$b <- Rwanda_2_data_steps$a %>%
    mutate_at(vars(q22.1:q22a.20),
              funs(fct_collapse(
                  .,
                  not.trustworthy = c(
                      'Not trustworthy at all',
                      'Not trustworthy',
                      'Somewhat trustworthy'
                  ),
                  trustworthy = c('Trustworthy', 'Very trustworthy')
              )))






# Q44 ---------------------------------------------------------------------


Rwanda_2_data_steps$c <- Rwanda_2_data_steps$b %>%
    mutate_at(vars(contains('q44b')), funs(ifelse(is.na(.), 'did not attend', .)))

Rwanda_2_data_steps$d <- Rwanda_2_data_steps$c


q10s <-
    c('maize',
      'bean',
      'soybean',
      'dairy',
      'other.ag',
      'work.off.farm',
      'out.with.Rwanda')
paste0('proportion.income.', q10s)


names(Rwanda_2_data_steps$d)[69:75] <-
    paste0('proportion.income.', q10s)


# Q11 (maize type) --------------------------------------------------------



maize_type <- c('OPV', 'Hybrid')
names(Rwanda_2_data_steps$d)[77:78] <-
    paste0('maize.seed.', maize_type)






# Q26 - Why hybrid --------------------------------------------------------



why_hybrid <-
    c(
        'demo.plots',
        'increase.income',
        'improve.yield',
        'reduce.time.cost.transportation',
        'reduce.time.with.sellers',
        'reliable.delivery.seeds',
        'gov.subsidies',
        'relatives.participate',
        'others.in.village.do',
        'trust.coop'
    )

names(Rwanda_2_data_steps$d)[171:180] <-
    paste0('why.use.hybrid?_', why_hybrid)


why_no_hybrid <-
    c(
        'lack.information',
        'never.seen.them',
        'do.not.increase.income',
        'waiting.demo.plot',
        'i.like.my.current.way',
        'relatives.dont.participate',
        'others.in.vilage.do.not',
        'i.dont.trust.coop',
        'grow.my.own'
    )



names(Rwanda_2_data_steps$d)[182:190] <-
    paste0('why.no.use.hybrid?_', why_no_hybrid)


names(Rwanda_2_data_steps$d)[81:83] <-
    c('variety.maize.grown',
      'since.grow.maize.income.has',
      'income.all.sources')


q15_q20 <-
    c(
        'responsible.livestock',
        'responsible.crops',
        'decides.where.sell',
        'decides.farm.activities',
        'decides.coop.participation',
        'maize.seeds.RAB.or.private'
    )

names(Rwanda_2_data_steps$d)[112:117] <- q15_q20



q22 <- c(
    ' Seed Co',
    'Kenya Seed',
    'Pannar',
    'Rwanda Agriculture Board (RAB)',
    'Yara Fertilizer',
    'Agro Dealer',
    'My village (Umidugudu) leader',
    'Cooperative Management Committee',
    'Cooperative President',
    'Cooperative Manager',
    'Cooperative Agronomist',
    'Zone Leaders',
    'Cooperative Accountant',
    'District Agronomist',
    'My relatives',
    'Sector Agronomist',
    'Radio',
    'Television',
    'My neighbors',
    'My church or mosque'
    
)




q22a <- q22 %>% str_to_lower(.) %>% str_replace_all(., ' ', '.')







names(Rwanda_2_data_steps$d)[124:143] <- paste0('trust.', q22a)


names(Rwanda_2_data_steps$d)[145:164] <- paste0('important.', q22a)



names(Rwanda_2_data_steps$d)[169] <-
    c('if.use.hybrid.quality.life.has')


names(Rwanda_2_data_steps$d)[287:302]

q44s <- c(
    'Attended gender training by Land o’ Lakes staff
    ',
    '	Attended coaching by Land o’ Lakes staff	'	,
    '	Attended agronomy training by Land o’ Lakes staff	'	,
    '	Attended communication training by Land o’Lakes	'	,
    '	Attended agronomy training from lead farmers	'	,
    '	Attended agronomy training from government	'	,
    '	Attended agronomy training from Seed Co	'	,
    '	Attended agronomy training from Yara	'	,
    '	How helpful was gender training by Land o’ Lakes staff	'	,
    '	How helpful was coaching by Land o’ Lakes staff	'	,
    '	How helpful was agronomy training by Land o’ Lakes staff	'	,
    '	How helpful was communication training by Land o’Lakes	'	,
    '	How helpful was agronomy training from lead farmers	'	,
    '	How helpful was agronomy training from government	'	,
    '	How helpful was agronomy training from Seed Co	'	,
    '	How helpful was agronomy training from Yara	'
)






q44s <- q44s %>%
    str_trim(.) %>%
    str_replace_all(., 'Land o’ Lakes staff', 'land.o.lakes') %>%
    str_replace_all(., 'by ', '') %>%
    str_replace_all(., 'from ', '') %>%
    str_replace_all(., 'training ', '') %>%
    str_replace_all(., 'How helpful was', 'helpful') %>%
    str_to_lower(.) %>%
    str_replace_all(., ' ', '.')







look(Rwanda_2_data_steps$d)



names(Rwanda_2_data_steps$d)[287:302] <- q44s


names(Rwanda_2_data_steps$d)[287:302]



q32_q40 <- c(
    'coops.elections.contested',
    'women.contest.elections?',
    'been.discouraged.from.contesting.election?',
    'yes.why?',
    'women.should.be.included.leadership',
    'nps',
    'compare.villagetrust.lend.borrow',
    'trust.last.3.years',
    'compare.village.outwith.lend.borrow',
    'people.look.out.for.self'
)



names(Rwanda_2_data_steps$d)[220:229] <- q32_q40

services <- c(
    '	Aware of purchase of fertilizer	'	,
    '	Aware of transportation of fertilizer	'	,
    '	Aware of distribution of fertilizer	'	,
    '	Aware of purchase of hybrid maize	'	,
    '	Aware of transportation of hybrid maize seed	'	,
    '	Aware of distribution of hybrid maize seed	'	,
    '	Aware of veterinary services	'	,
    '	Aware of loans to purchase seeds	'	,
    '	Aware of loans to purchase fertilizer	'	,
    '	Aware of general loan	'	,
    '	Aware of school loans	'	,
    '	Aware of medical loans	'	,
    '	Aware of small goods loans	'	,
    '	Aware of agriculture training	'	,
    '	Aware of agriculture machinery	'	,
    '	Aware of other inputs (specify)	'	,
    '	Aware of other service 1	'	,
    '	Aware of other service 2	'	,
    '	Do you use purchase of fertilizer	'	,
    '	Do you use transportation of fertilizer	'	,
    '	Do you use distribution of fertilizer	'	,
    '	Do you use purchase of hybrid maize	'	,
    '	Do you use transportation of hybrid maize seed	'	,
    '	Do you use distribution of hybrid maize seed	'	,
    '	Do you use veterinary services	'	,
    '	Do you use loans to purchase seeds	'	,
    '	Do you use loans to purchase fertilizer	'	,
    '	Do you use general loan	'	,
    '	Do you use school loans	'	,
    '	Do you use medical loans	'	,
    '	Do you use small goods loans	'	,
    '	Do you use agriculture training	'	,
    '	Do you use agriculture machinery	'	,
    '	Do you use other inputs (specify)	'	,
    '	Do you use other service 1	'	,
    '	Do you use other service 2	'
)

length(names(Rwanda_2_data_steps$d)[231:266]) == length(services)

services <- services %>%
    str_trim(.) %>%
    str_replace(., 'Do you use ', 'use.') %>%
    str_replace(., 'Aware of ', 'aware.') %>%
    str_replace_all(., ' ', '.')



names(Rwanda_2_data_steps$d)[231:266] <- services



q42 <- c(
'	Input purchase credit	'	,
'	General credit	'	,
'	Human food	'	,
'	Training	'	,
'	Agriculture machinery	'	,
'	Health insurance	'	,
'	Other service1	'	,
'	Other service2	',
'please specify',
'please specify')



q42 <- clean(q42)
q42 <- str_replace_all(q42,' ','.')


paste0(q42,'_would.like.to.see')


names(Rwanda_2_data_steps$d)[270:279] <- q42

q43 <- c('	Expanding membership	'	,
  '	Improve existing services to members	'	,
  '	Provide new services to members	'	,
  '	Invest in new business ventures	'	,
  '	Increase members income	'	
)



q43 <- clean(q43)
q43 <- str_replace_all(q43,' ','.')


look(Rwanda_2_data_steps$d)


length(q43)
names(Rwanda_2_data_steps$d)[281:285] <- q43

q21 <- c(
'	Price	'	,
'	Quality	'	,
'	Convience of purchase	'	,
'	Trust of the seller	'	
)


names(Rwanda_2_data_steps$d)[119:122] <- paste0(str_replace_all(clean(q21),' ','.'),'how.satisfied')



q28 <- c	(	'	Face-to-face	'	,
    '	Radio	'	,
    '	Newspaper	'	,
    '	Cell phone	'	,
    '	Village leader	'	,
    '	Zone leader	'	,
    '	Special assembly	'	)


q29 <- c	(	'	Explanation of cooperative rules	'	,
           '	Enforcement of cooperative rules	'	,
           '	Cooperative communication to members	'	,
           '	Members communication to the cooperative	'	,
           '	Overall, the cooperative works for my best interests	'	)



q30 <- c	(	'	Management contacting members directly	'	,
           '	Management committee members contacting members directly	'	,
           '	Special General Assembly	'	,
           '	Cell Phone	'	,
           '	General Assembly Meeting	'	)


q31 <- c	(	'	Members going directly to the management of the cooperative	'	,
           '	Members going directly to members of the cooperative management	'	,
           '	General Assembly	'	,
           '	Special General Assembly	'	,
           '	Members going directly to zone leaders	'	,
           '	Cell phone	'	,
           '	Other Organization in the Village	',
           'Please specify'
)


q28_q31 <- list(q28,q29,q30,q31)

q28_q31 <- map(q28_q31,function(x){
    str_replace_all(clean(x),' ','.')}
)

look(Rwanda_2_data_steps$d)
    

indeces <- list(192:198,200:204,206:210,212:219)


map2(indeces,q28_q31,function(x,y){
    names(Rwanda_2_data_steps$d)[x] <- y
})


look(Rwanda_2_data_steps$d)

names(Rwanda_2_data_steps$d)[c(192:198,200:204,206:210,212:219)] <- flatten_chr(q28_q31)

names(Rwanda_2_data_steps$d)[212:219] <- paste0(names(Rwanda_2_data_steps$d)[212:219],'_transmit.concern.to.coop')

k <- list(Rwanda.2.Step_A ,Rwanda.2.Step_B ,Rwanda.2.Step_C ,Rwanda.2.Step_D)
map(Rwanda_2_data_steps,function(x){list2env(x)})

list2env(Rwanda_2_data_steps,.GlobalEnv)


map(k,function(x){
    save(list = k,file = paste0('C:/R/Rwanda_2/data/',k,'RData'))
})


saver <- function(x){save(x,file = 'C:/R/Rwanda_2/data/',names(x),'/.RData')}



dfs <- list(Rwanda.2.Step_A,Rwanda.2.Step_B,Rwanda.2.Step_C,Rwanda.2.Step_D)
dfs.names <- list('Rwanda.2.Step_A','Rwanda.2.Step_B','Rwanda.2.Step_C','Rwanda.2.Step_D')

map2(dfs,dfs.names,function(x,y){
    save(x,file = paste0('C:/R/Rwanda_2/data/cleaned_files/',y,'.RData'))
})



setwd(paste0(getwd(),'/data/'))
setwd('C:/R/Rwanda_2/')


save(Rwanda.2.Step_A,file = paste0(getwd(),'/data/Rwanda.2.Step_A.RData'))




















