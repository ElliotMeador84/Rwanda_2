library(tidyverse)

options(warn=-1)
load(agrep('r2_hybrid_sig.RData',list.files(recursive = T),value = T))

# Use --------------------------------------------------------

# Identify variables of interest 

use <- agrep('^use.', names(r2d_df_sig), value = T)

use_coop_services <- r2d_df_sig %>%
  rename(coop.works.well =
           Overall.The.Cooperative.Works.For.My.Best.Interests) %>%
  mutate(coop.works.well = as.numeric(coop.works.well)) %>%
  select(use, coop.works.well) %>%
  gather(key, value, -coop.works.well)

# Clean up a bit

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

# Add confidence intervals
use_coop_services <- use_coop_services  %>%
    group_by(key, value) %>%
    na.omit(.) %>% 
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

### GGplots

(
    use_coop_services.plots <-
        ggplot(use_coop_services,
               aes(fct_reorder(key, Value_mean) %>% fct_rev, Value_mean, group = factor(value))) +
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
            show.legend = T
        ) +
        geom_point(
            position = position_dodge(0.5),
            aes(color = value, fill = value),
            size = 3,
            shape = 21,
            show.legend = T,
            alpha = .9
        ) +
        ggtitle('Figure 1.3: Services - Use') +
        ylab('Average:The cooperative works\nwell for my interests, Likert (1-5)') +
        xlab('')+
        theme_bw()+
        scale_color_grey()+
        theme(legend.position = 'bottom',legend.title = element_blank()) +
        scale_fill_grey()+
        theme(axis.text = element_text(angle = 45,hjust = 1),
              panel.grid = element_blank(),
              legend.position = 'bottom',
              legend.title = element_blank())
)

print(use_coop_services.plots)

options(warn = 0)

# ggsave(plot = how.help.training.plots,filename = '/Users/ElliotMeador/Documents/R/Rwanda_2/plots/explore/use.services.png',width = 11,height = 8)
