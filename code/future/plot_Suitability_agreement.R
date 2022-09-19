###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: Suitablity Agreement plot. part
#   of the suitability agreement data was 
#   calculated in the 12-SuitabilityAnalysis.R
#   script.
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('terra', 'sf', 'tidyverse', 'here',
         'gridExtra', 'cowplot', 'ggnewscale', 'ggpmisc') 

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

###############################################################################
# data
###############################################################################

# relevant directories
dir <- here('Data', 'future', 'gcm', 'Summaries')
fig_dir <- here('Figures', 'future', 'maps')

# ecoregion boundaries
ecoreg <- st_read(here('Data', 'current', 'us_eco_l3_west.shp'))

# state boundaries
states <- st_read(here('Data', 'current', 'states_west_clip.shp'))

# ranked ecoregions
ecoregions <- st_read(file.path(dir, 'Suitable_count_sum.shp')) %>%
  mutate(vulnerability = case_when(su_p_m2 < 29 ~ 10,
                                   (su_p_m2 >= 29) & (su_p_m2 < 111) ~ 20,
                                   (su_p_m2 >= 111) & (su_p_m2 < 229) ~ 30,
                                   (su_p_m2 >= 229) & (su_p_m2 < 483) ~ 40,
                                   (su_p_m2 >= 483) & (su_p_m2 < 623) ~ 50)) %>%
  filter(vulnerability != 10)
ecoregions <- ecoregions %>%
  mutate(area_m2 = st_area(ecoregions))

# pull in ecoregion label information from saved csv created in 'Study_Area_plot.R'
eco_labs <- read.csv(here('Data', 'current', 'Ecoregion_label_table.csv')) %>%
  select(-X)

# get ecoregion centroids for labeling and keep only the largest polygon
eco_centroids <- st_centroid(ecoregions, of_largest_polygon = T) %>%
  group_by(US_L3NA) %>%
  filter(area_m2 == max(area_m2)) %>%
  ungroup() %>%
  mutate(x = st_coordinates(geometry)[,1],
         y = st_coordinates(geometry)[,2]) %>%
  arrange(US_L3NA) %>%
  st_drop_geometry() %>%
  left_join(eco_labs, by = c('US_L3NA' = 'Ecoregion'))

# suitability agreement classes
rcl_suit <- t(matrix(c(c(-1, 5, 1),
                       c(5, 10, 2),
                       c(10, 15, 3),
                       c(15, 20, 4),
                       c(20, 25, 5),
                       c(25, 30, 6)),
                     nrow = 3, ncol = 6))

# suitability agreement
agreement <- rast(file.path(dir, 'Suitable_count_allgcms.tif')) %>%
  mask(vect(states)) %>%
  classify(rcl = rcl_suit,
           right = T,
           include.lowest = T) %>%
  setNames(c('agree')) %>%
  as.data.frame(xy = T, na.rm = T) %>%
  filter(agree != 1)

###############################################################################
# plot
###############################################################################

agreement %>%
  ggplot() +
  geom_sf(data = ecoreg, fill = 'white', color = 'grey25', size = 0.15) +
  geom_sf(data = ecoregions, 
          aes(fill = factor(vulnerability, 
                            labels = c('Low', 'Moderate', 'High', 'Very High'))),
          color = 'grey25',
          size = 0.15,
          alpha = 0.5) +
  scale_fill_grey('Ecoregion Vulnerability', start = 0.8, end = 0.2) +
  new_scale('fill') +
  geom_tile(aes(x = x, y = y, 
                fill = factor(agree,
                              labels = c('6-10', '11-15', '16-20', 
                                         '21-25','26-30')),
                color = factor(agree,
                               labels = c('6-10', '11-15', '16-20', 
                                          '21-25','26-30')))) +
  scale_fill_manual('Suitability Agreement',
                    values = c('#f05f5c', '#b53579', '#711e80',
                               '#2c115e', '#010104')) +
  scale_color_manual('Suitability Agreement',
                     values = c('#f05f5c', '#b53579', '#711e80',
                                '#2c115e', '#010104')) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  scale_x_continuous(expand = c(0.01,0.01)) +
  geom_label(data = eco_centroids,
             aes(x= x, y = y, label = Label),
             fill = 'white',
             label.size = NA,
             label.padding = unit(0.15, "lines")) +
  theme_grey() +
  labs(x = '', y = '', fill = 'Suitability Agreement') +
  theme(panel.background = element_rect(color = 'white', 
                                        fill = 'white'),
        panel.grid.major = element_line(color = 'grey50', 
                                        size = 0.25, 
                                        linetype = 'dashed'), 
        axis.ticks = element_blank(),
        plot.margin = margin(1, -2, -3, -2)) +
  guides(color = 'none')

ggsave(filename = 'Suitability_Agreement.tiff',
       path = fig_dir,
       scale = 1.5,
       width = 4,
       height = 3.5,
       units = 'in',
       dpi = 1200,
       compression = 'lzw',
       bg = 'white')
