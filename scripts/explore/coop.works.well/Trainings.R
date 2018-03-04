library(tidyverse)

options(warn=-1)

load(agrep('r2d_df.sig.RData',list.files(recursive = T),value = T))
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

(
how.help.training_a.plot <- ggplot(how.help.training_a,
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
  ggtitle('Figure 1.1: Training') +
  ylab('Average:The cooperative works\nwell for my interests, Likert (1-5)') +
  xlab('')+
    theme_bw()+
    scale_color_grey()+
    theme(legend.position = 'bottom',legend.title = element_blank()) +
    scale_fill_grey()+
        theme(axis.text = element_text(angle = 45,hjust = 1),
              panel.grid = element_blank(),
              legend.position = 'bottom',
              legend.title = element_blank()))

print(how.help.training_a.plot)


options(warn=0)


