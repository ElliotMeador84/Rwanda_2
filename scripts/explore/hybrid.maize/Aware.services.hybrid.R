

library(tidyverse)
setwd('Rwanda_2/')
file.edit("~/Aware.services.hybrid.R")


load(agrep('r2_hybrid_sig.RData',list.files(recursive = T),value = T))

source(agrep('Helper_Functions/dplyr_helpers.R',list.files(recursive = T),value = T))


options(warn = -1)

alpha <- r2_hybrid_sig %>%
    select(matches('^aware'), maize.seed.Hybrid) %>%
    mutate_all(funs(ifelse(. == 'Yes', 1, 0))) %>%
    gather(key, value, -maize.seed.Hybrid)


alpha$key <- alpha$key %>%
    str_replace_all('aware.', '') %>%
    str_replace_all('\\.', ' ') %>%
    str_to_title(.) %>%
    str_wrap(20)

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
beta <- beta %>%
    mutate(value = ifelse(value == 1, 'Not aware', 'Aware'))

(
    beta.plot <- ggplot(beta,
                        aes(
                            fct_reorder(key, Value_mean, mean) %>% fct_rev, Value_mean, group = value
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
        ggtitle('Figure 2.1: Services - Aware') +
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
