
# Library -----------------------------------------------------------------

agrep('.RData$',list.files(recursive = T),value = T)

# load(file = 'df_model.RData')
getwd() 

list.files()
load(file = "Rwanda/Various.files/df_model_new.RData")
library(sjPlot)
library(tidyverse)
library(margins)

# Figure_1 ----------------------------------------------------------------
df_model_new

df_model %>%
    rename(Gender = gender) %>%
    gather(key, value,-Net_Promoter, -Gender, -Marital_status, -coop_n) %>%
    group_by(Gender, key) %>%
    summarise(
        Value_mean = mean(value, na.rm = T),
        n = n(),
        sd = sd(value, na.rm = T)
    ) %>%
    mutate(
        se = sd / sqrt(n),
        lower.ci = Value_mean - qt(1 - (0.05 / 2), n - 1) * se,
        upper.ci = Value_mean + qt(1 - (0.05 / 2), n - 1) * se,
        key = str_wrap(key, 20)
    ) %>%
    ggplot(., aes(fct_reorder(key, Value_mean, .desc = T), Value_mean, group  = Gender)) +
    geom_line(
        position = position_dodge(.5),
        aes(color = Gender),
        alpha = .9,
        show.legend = T
    ) +
    geom_errorbar(
        aes(
            ymin = Value_mean - se,
            ymax = Value_mean + se,
            color = Gender
        ),
        width = .1,
        position = position_dodge(0.5),
        alpha = .9,
        show.legend = T
    ) +
    geom_point(
        position = position_dodge(0.5),
        aes(color = Gender, fill = Gender),
        size = 3,
        shape = 21,
        show.legend = T,
        alpha = .9
    ) +
    theme_bw() +
    scale_fill_grey() +
    scale_color_grey() +
    theme(
        axis.text.x = element_text(
            angle = 50,
            hjust = 1,
            size = 9.5,
            face = 'bold'
        ),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(
            color = 'black',
            size = .5,
            linetype = 3
        ),
        legend.position = 'bottom',
        plot.margin = margin(
            t = 10,
            r = 20,
            b = 10,
            l = 20
        )
    ) +
    labs(title = 'Figure 1: Cooperative Members’ Trust of Specific Organization and individuals – Total Sample and by Gender.  \nScale – 1 to 5, with 5 = highest level of trust.',
         caption = 'Error bars are 95% CI') +
    xlab('Trust variable') +
    ylab('Mean value') +
    theme(plot.background = element_rect(fill = 'gray95', colour = 'black'))

ggsave('Rwand_1_Figure_1.png',height = 8,width = 11)
getwd()
# Figure_2 ----------------------------------------------------------------



df_model %>% gather(key, value,-Net_Promoter, -gender, -Marital_status, -coop_n) %>%
    group_by(Net_Promoter, key) %>%
    summarise(
        Value_mean = mean(value, na.rm = T),
        n = n(),
        sd = sd(value, na.rm = T)
    ) %>%
    mutate(
        se = sd / sqrt(n),
        lower.ci = Value_mean - qt(1 - (0.05 / 2), n - 1) * se,
        upper.ci = Value_mean + qt(1 - (0.05 / 2), n - 1) * se
    ) %>%
    ggplot(., aes(fct_reorder(key, Value_mean, .desc = T), Value_mean, group  = Net_Promoter)) +
    geom_line(
        position = position_dodge(.5),
        aes(color = Net_Promoter),
        alpha = .9,
        show.legend = T
    ) +  ## legend
    geom_errorbar(
        aes(
            ymin = Value_mean - se,
            ymax = Value_mean + se,
            color = Net_Promoter
        ),
        width = .1,
        position = position_dodge(0.5),
        alpha = .9,
        show.legend = T ## legend
    ) +
    geom_point(
        position = position_dodge(0.5),
        aes(color = Net_Promoter, fill = Net_Promoter),
        size = 3,
        shape = 21,
        show.legend = T,  ## legend
        alpha = .9
    ) +
    theme_bw() +
    scale_fill_grey(name = "Promoter type") +
    scale_color_grey(name = "Promoter type") +
    theme(legend.title = element_text('Promoter type'),
          axis.text.x = element_text(
              angle = 50,
              hjust = 1,
              size = 9.5,
              face = 'bold'
          ),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(
              color = 'black',
              size = .5,
              linetype = 3
          ),
          legend.position = 'bottom'
    ) +
    # Figure 1: Cooperative Members’ Trust of Specific Organization and individuals – Total Sample and by Gender.  \nScale – 1 to 5, with 5 = highest level of trust.
    #
    labs(title = 'Figure 2: Trust of individuals and organizations by three promoter types\nScale – 1 to 5, with 5 = highest level of trust',
         caption = 'Error bars are 95% CI') +
    xlab('Individuals and organizations') +
    ylab('Mean trust levels on five-point scales') +
    theme(plot.background = element_rect(fill = 'gray95', colour = 'black'))

ggsave('Rwand_1_Figure_2.png',height = 8,width = 11)



# Regressions -------------------------------------------------------------


sjt.glm(glm(Q36_Poisson~.,data = df_model_new[1:7],poisson(link = "log")),
        glm(Q36_Poisson~.,data = df_model_new[c(1:7,9:12,14,13,15,17)],poisson(link = "log")),
        glm(Q36_Poisson~.,data = df_model_new[c(1:7,9:12,14,13,15,17,8,16,20,21)],poisson(link = "log")),
        glm(Q36_Poisson~.,data = df_model_new,poisson(link = "log")),
        exp.coef = T,Are you availabble 
        p.numeric = F,
        emph.p = T,
        show.r2 = T)



# Predicted odds ----------------------------------------------------------

model <- glm(Q36_Poisson~.,data = df_model_new,poisson(link = "log"))
# specify data ======


new.data <- df_model_new %>% 
    map_df(possibly(~mean(.,na.rm = T),'failure'))

new.data <- map_df(new.data,~round(.,0))
glimpse(new.data)




glimpse(new.data)



# odds --------------------------------------------------------------------
new.data <- new.data %>% 
    mutate('Marital status' = 'Married',
           'Cooperative President' = -1,
           `Pa’anal`  = -5,
           Gender = 0) 

10-predict(model, new.data, type="response")

# write_up ----------------------------------------------------------------


Single respondents, who report a mean score on all other variables, is 9.61; whereas, married respondents, who, likewise, report a mean score on all other variables, is 8.46.

Male respondents, who  scored their trust in the coopertive president as a five out of five and reported an average score for all other vairables, is 7.41. Female respondents, who also scored their trust in the cooperative president and reported an average score for all other vairables, is 7.89.

Female respondents, who report low levels of trust in the cooperative president (1 out of 5) and high levels of trust in the American agro-chemical company (5 out of 5),is 7.64; for males, with similar scores, is 7.09.

































