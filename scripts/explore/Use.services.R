library(tidyverse)

# save(r2d_df_sig,file = paste0(.dir,'/data/cleaned_files/r2d_df.sig.RData'))
load('~/Documents/R/Rwanda_2/data/cleaned_files/r2d_df.sig.RData')

# Use --------------------------------------------------------
use <- agrep('^use.', names(r2d_df_sig), value = T)

use_coop_services <- r2d_df_sig %>%
  rename(coop.works.well =
           Overall.The.Cooperative.Works.For.My.Best.Interests) %>%
  mutate(coop.works.well = as.numeric(coop.works.well)) %>%
  select(use, coop.works.well) %>%
  gather(key, value, -coop.works.well)

use_coop_services$key <- fct_recode(
  use_coop_services$key,
  'Purchase of\nfertilizer' = 'use.purchase.of.fertilizer',
  'Transportation of\nfertilizer' = 'use.transportation.of.fertilizer',
  'Distribution of\nfertilizer' = 'use.distribution.of.fertilizer',
  'Purchase\nhybrid seed' = 'use.purchase.of.hybrid.maize',
  'Transportation of\nhybrid seed' = 'use.transportation.of.hybrid.maize.seed',
  'Veterinary services' = 'use.veterinary.services',
  'Loans to\npurchase fertilizer' = 'use.loans.to.purchase.fertilizer',
  'General\nloans' = 'use.general.loan',
  'School\nloans' = 'use.school.loans',
  'Medical\nloans' = 'use.medical.loans',
  'Small goods\nloans' = 'use.small.goods.loans'
)

use_coop_services$value <- fct_recode(use_coop_services$value,
                                      'NULL' = '2')

use_coop_services <- use_coop_services %>%
  group_by(key, value) %>%
  summarise(average = mean(coop.works.well, na.rm = T)) %>%
  na.omit(.)

### GGplots
(
  use_coop_services.plots <-
    ggplot(use_coop_services, aes(value, average)) +
    geom_bar(
      stat = 'identity',
      aes(fill = key, color = key),
      show.legend = F
    ) +
    facet_wrap( ~ key) +
    coord_flip() +
    ggtitle('Use services by how well coop works for my interests') +
    ylab(
      'Average:The cooperative works well for my interests, Likert (1-5)'
    )
)

# ggsave(plot = how.help.training.plots,filename = '/Users/ElliotMeador/Documents/R/Rwanda_2/plots/explore/use.services.png',width = 11,height = 8)
