

library(tidyverse)

options(warn=-1)

load(agrep('r2d_df.sig.RData',list.files(recursive = T),value = T))

# Use --------------------------------------------------------

# Identify variables of interest 

satisfaction <- names(r2d_df_sig)[c(22:25)]


alpha <- r2d_df_sig %>%
    rename(coop.works.well =
               Overall.The.Cooperative.Works.For.My.Best.Interests) %>%
    mutate(coop.works.well = as.numeric(coop.works.well)) %>%
    select(satisfaction, coop.works.well) %>%
    gather(key, value, -coop.works.well) %>% 
    mutate(value = ifelse(.$value %in% c(3,4,5),'Satisfied','Not satisfied'))

alpha$key <- alpha$key %>% 
    str_replace_all(pattern = '\\.',' ') %>% 
    str_to_title() %>% 
    str_wrap(15)


# Add confidence intervals
alpha <- alpha  %>%
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



(
    alpha.plots <-
        ggplot(alpha,
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
        ggtitle('Figure 1.1: Coop communication and rule enforcement') +
        ylab('Average:The cooperative works\nwell for my interests, Likert (1-5)') +
        xlab('')+
        theme_bw()+
        scale_color_grey()+
        scale_fill_grey()+
        theme(axis.text = element_text(angle = 45,hjust = 1),
              panel.grid = element_blank(),
              legend.position = 'bottom',
              legend.title = element_blank())
    
)


print(alpha.plots)

options(warn=0)


































