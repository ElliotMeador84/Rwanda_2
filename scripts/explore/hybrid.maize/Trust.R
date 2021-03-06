

library(tidyverse)
if (.Platform$OS.type == "windows") {
  load('C:/R/Rwanda_2/data/cleaned_files/r2_hybrid_sig.RData')
} else {
  load(agrep('r2_hybrid_sig.RData', list.files(recursive = T), value = T))
}
source(agrep('dplyr.R', list.files(recursive = T), value = T)[1])


options(warn = -1)

alpha <- r2_hybrid_sig %>%
  select(
    matches('people.look'),
    matches('trust.last.3.years'),
    matches('^maize.seed.Hybrid$')
  ) %>%
  mutate(
    people.look.out.for.self = fct_collapse(
      people.look.out.for.self,
      'Low' = c('Donâ€™t Know','Disagree Strongly', 'Disagree'),
      'High' = c('Agree', 'Strongly Agree')
    ),
    trust.last.3.years = fct_collapse(
      trust.last.3.years,
      'Low' = c('Worsened','Stayed the same', '0'),
      'High' = c('Improved')
    )
  ) %>%
  rename(
    'People look out for themselves' = people.look.out.for.self,
    'Over the last 3 year trust has . . .' = trust.last.3.years
  ) %>%
  na.omit(.) %>%
  gather(key, value,-maize.seed.Hybrid) %>% 
  mutate(maize.seed.Hybrid = ifelse(maize.seed.Hybrid == 'Yes',1,0))

beta <- alpha %>%
  group_by(key, value) %>%
  summarise(
    Value_mean = mean(maize.seed.Hybrid, na.rm = T),
    n = n(),
    sd = sd(maize.seed.Hybrid, na.rm = T)
  ) %>%
  mutate(
    se = sd / sqrt(n),
    lower.ci = Value_mean - qt(1 - (0.05 / 2), n - 1) * se,
    upper.ci = Value_mean + qt(1 - (0.05 / 2), n - 1) * se
  )

(
  beta.plot <- ggplot(beta,
                      aes(
                        fct_reorder(key, Value_mean, mean) %>% fct_rev,
                        Value_mean,
                        group = value
                      )) +
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
    ggtitle('Figure 2.2: Trust') +
    ylab('I use hybrid maize seed\n`Yes` = 1; `No` = 0') +
    xlab('') +
    theme_bw() +
    scale_color_grey() +
    theme(legend.position = 'bottom', legend.title = element_blank()) +
    scale_fill_grey() +
    theme(
      axis.text = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      legend.position = 'bottom',
      legend.title = element_blank()
    )
)

print(beta.plot)

options(warn = 0)
