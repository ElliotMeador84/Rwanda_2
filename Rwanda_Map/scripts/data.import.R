library(tidyverse)
library(RColorBrewer)
library(rgdal)
library(maptools)
library(ggrepel)

# import data -------------------------------------------------------------

# Data gathered from here http://mapeastafrica.com/countries/east-africa-shapefiles/rwanda-shapefiles/

rwanda.shape <- readOGR('data/Rwanda_admin_2014_WGS84.shp')
rwanda.points <- fortify(rwanda.shape, region = 'OBJECTID')
rwanda_id <-
    data_frame(id = as.character(length(rwanda.shape$ADM1)),
               label = rwanda.shape$ADM1)
rwanda.shape_df <- left_join(rwanda.points, rwanda_id)

# save(rwanda.shape_df,file = 'data/rwanda.shape_df.RData')

# quick plot, best to add this to the geom_poly
ggplot(rwanda.shape_df, aes(long, lat, group = group)) +
    geom_polygon(fill = brewer.pal(9, 'Greens')[3],
                 color = brewer.pal(9, 'Greens')[7]) +
    coord_map() +
    theme(panel.background = element_blank())


# Import cooperative location data ----------------------------------------

coop_location <- read_csv('data/cleaned_files/Coop_location.csv')

names(coop_location) <- tolower(names(coop_location))

coop_location <- coop_location %>%
    mutate(name = as.factor(ifelse(
        name == 'COACMU',
        'Coop C',
        ifelse(name == 'KABOKU', 'Coop D', 'Coop E')
    )))

# save(coop_location,file = 'data/coop_location.RData')
