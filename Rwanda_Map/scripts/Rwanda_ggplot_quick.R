# Libraries , dasta and functions -------
library(tidyverse)
library(RColorBrewer)
library(ggrepel)
source('C:/R/all_functions.R')
# coop_location <- read_csv('data/cleaned_files/Coop_location.csv')
# save(coop_location,file = 'data/coop_location.RData')
load('Rwanda_Map/data/rwanda.shape_df.RData')
load('Rwanda_Map/data/coop_location.RData')
# Rwanda base map --------

# Get background map
# ggplot has the map_data geom that
# uses information from the 'maps' package
# 
# IMPORTANT
# this function replaces purrr::map() 
Rwanda <-
    map_data(map = "world",
             # use ?maps to view all map databases
             region = "Rwanda",
             # change to country/region of choice
             interior = T) #


# Plot major cities
# 
Rwanda_cities <- world.cities %>% # world.cities is in maps::
    filter(country.etc == 'Rwanda')


# Combine the two geoms --------

## Make capital city easier to identify
Rwanda_cities <- Rwanda_cities %>%
    mutate(name = ifelse(name == 'Kigali', 'Kigali (capital)', name))


### Grab the cooperative's location
load(file = 'data/coop_location.RData')
#### fix the names

names(coop_location) <- tolower(names(coop_location))

# split to run ggrepel one at a time
coop_location_ls <- split(coop_location, f = coop_location$name)


coop_location_ls <- purrr::map(coop_location_ls,function(x){
    x %>% 
        mutate(name = str_replace_all(name,'_',' '))
    
})


# function to handle repel each point

repel_geom_text <- function(x = tibble,
                            y = 0 ,
                            z = 0) {
    geom_text_repel(
        data = x,
        aes(long, lat, group = name, label = name),
        nudge_x = y,
        nudge_y = z
    )
}
# Rwanda Cooperative Location Plot ====


ggplot(rwanda.shape_df, aes(long, lat, group = group)) +
    geom_polygon(fill = brewer.pal(9, 'Greens')[3],
                 color = brewer.pal(9, 'Greens')[4]) +
    geom_point(data = Rwanda_cities, aes(long, lat, group = name)) +
    geom_point(data = coop_location, aes(long, lat, group = name),
               shape = 15) +
    repel_geom_text(coop_location_ls$Coop_C, .5,-.5) +
    repel_geom_text(coop_location_ls$Coop_D, .35, .15) +
    repel_geom_text(coop_location_ls$Coop_E, .35,-.1) +
    repel_geom_text(coop_location_ls$Coop_A,9,-0.2) +
    repel_geom_text(coop_location_ls$Coop_B,-.5,.5) +
    geom_text(
        data = Rwanda_cities,
        aes(long, lat, label = name, group = name),
        nudge_x = -0.025,
        nudge_y = 0.045,
        color = 'black',
        size  = 3,
        check_overlap = T
    ) +
    coord_map() +
    scale_x_continuous(breaks = seq(29, 31, 0.5)) +
    theme_void() +
    theme(panel.background = element_blank(),
          plot.margin = margin(rep(.5, 4))) +
    labs(title = 'Example',
         caption = 'Locations are approximate')


# ggsave('png/Rwanda_Coop_Location.png',
#        height = 8,
#        width = 11)











