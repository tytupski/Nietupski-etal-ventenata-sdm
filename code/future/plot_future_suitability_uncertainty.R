###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: plot of 
#     1. median gcm-based suitability
#     2. gcm prediction variability
#     3. sdm prediction variability
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('terra', 'sf', 
         'tidyverse', 'gridExtra', 'cowplot', 'viridis',
         'here', 'ggnewscale')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

###############################################################################
# data
###############################################################################

# main data directories
dir <- here('Data')
fig_dir <- here('Figures', 'future', 'maps')

# state boundaries
states <- st_read(file.path(dir, 'current', 'states_west_clip.shp'))

# ecoregion boundaries
ecoreg <- st_read(file.path(dir, 'current', 'us_eco_l3_west.shp'))

## brt uncertainty ----
brt_uncert <- rast(file.path(dir, 
                             'future', 
                             'gcm',
                             'Summaries',
                             'Average_BRT_uncertainty.tif')) %>%
  setNames('uncertainty') %>%
  as.data.frame(xy = T, na.rm = T) %>%
  mutate(title = 'SDM-derived Variability')

## gcm uncertainty ----
gcm_uncert <- rast(file.path(dir, 
                             'future',
                             'gcm',
                             'Summaries',
                             'vedu_prob_2069_2099_Std_allGCMS.tif')) %>%
  setNames('uncertainty') %>%
  as.data.frame(xy = T, na.rm = T) %>%
  mutate(title = 'GCM-derived Variability')

## gcm ensemble suitability ----
gcm_suit <- rast(file.path(dir,
                           'future',
                           'gcm', 
                           'Summaries',
                           'vedu_prob_2069_2099_Median_allGCMS.tif')) %>%
  setNames('suitability') %>%
  as.data.frame(xy = T, na.rm = T) %>%
  mutate(title = 'GCM Suitability')

###############################################################################
# plot
###############################################################################

p1 <- gcm_suit %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, 
                  fill = suitability)) +
  scale_fill_viridis_c() +
  scale_y_continuous(expand = c(0.005,0.005)) +
  scale_x_continuous(expand = c(0.005,0.005)) +
  geom_sf(data = ecoreg, fill = NA, color = '#bcbcbc', size = 0.15) +
  theme_grey() +
  labs(x = '', y = '', fill = '') +
  theme(panel.background = element_rect(color = 'white', 
                                        fill = 'white'),
        panel.grid.major = element_line(color = 'grey50', 
                                        size = 0.25, 
                                        linetype = 'dashed'), 
        axis.ticks = element_blank(),
        legend.key.width = unit(2.25, 'line'),
        legend.key.heigh = unit(0.75, 'line'),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        plot.margin = margin(1, 7, 1, 0), 
        legend.margin = margin(-20,0,0,0)) +
  facet_grid(.~title)
  
p2 <- gcm_uncert %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, 
                  fill = uncertainty)) +
  scale_fill_viridis_c(option = 'rocket') +
  scale_y_continuous(expand = c(0.005,0.005)) +
  scale_x_continuous(expand = c(0.005,0.005)) +
  geom_sf(data = ecoreg, fill = NA, color = '#bcbcbc', size = 0.15) +
  theme_grey() +
  labs(x = '', y = '', fill = '') +
  theme(panel.background = element_rect(color = 'white', 
                                        fill = 'white'),
        panel.grid.major = element_line(color = 'grey50', 
                                        size = 0.25, 
                                        linetype = 'dashed'), 
        axis.ticks = element_blank(),
        legend.key.width = unit(2.25, 'line'),
        legend.key.heigh = unit(0.75, 'line'),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        plot.margin = margin(1, 7, 1, 1), 
        legend.margin = margin(-20,0,0,0)) +
  facet_grid(.~title)

p3 <- brt_uncert %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, 
                  fill = uncertainty)) +
  scale_fill_viridis_c(option = 'rocket') +
  scale_y_continuous(expand = c(0.005,0.005)) +
  scale_x_continuous(expand = c(0.005,0.005)) +
  geom_sf(data = ecoreg, fill = NA, color = '#bcbcbc', size = 0.15) +
  theme_grey() +
  labs(x = '', y = '', fill = '') +
  theme(panel.background = element_rect(color = 'white', 
                                        fill = 'white'),
        panel.grid.major = element_line(color = 'grey50', 
                                        size = 0.25, 
                                        linetype = 'dashed'), 
        axis.ticks = element_blank(),
        legend.key.width = unit(2.25, 'line'),
        legend.key.heigh = unit(0.75, 'line'),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        plot.margin = margin(1, 7, 1, 1), 
        legend.margin = margin(-20,0,0,0)) +
  facet_grid(.~title)

composite_plot <- plot_grid(p1, p2, p3, 
                            align = 'hv',
                            axis = 'tblr',
                            nrow = 1)

ggsave(filename = 'gcm_suit_uncert.tiff',
       path = fig_dir,
       plot = composite_plot,
       scale = 1.3,
       width = 6.5,
       height = 2.7,
       units = 'in',
       dpi = 1200,
       compression = 'lzw',
       bg = 'white')
