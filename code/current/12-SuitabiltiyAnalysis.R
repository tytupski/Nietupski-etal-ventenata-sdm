###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: different ways to summarize the 
#   output from the current and future 
#   predictions
#     1. stats of suitability by elevation
#     2. suitability classes by elevation class
#     3. suitability agreement and ecoregion
#     vulnerability
#     4. comparison of suitability in invaded 
#     range vs outside invaded range
#     5. summary of prediction variability
#     (uncertainty)
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('terra', 'sf', 'here',
         'tidyverse', 'gridExtra', 'cowplot')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

###############################################################################
# data
###############################################################################

# main directories
dir <- here('Data')
fig_dir <- here('Figures')

# labels for the gcms
gcm_names <- read.csv(file.path(dir,
                                'future',
                                'gcm',
                                'Summaries',
                                'gcm_medians_delta.csv'))  %>%
  # mutate(gcm = reorder(gcm, delta_mat, max)) %>% # order by change in MAT from low to high
  .$gcm

#### shapefiles for invaded range, ecoregions ####
## convex hull (invaded range) ----
conv_hull <- st_read(file.path(dir, 'current', 'vedu_mcp_50k.shp')) %>%
  st_transform(crs = st_crs(5070))

## ecoregions ----
west_eco <- st_read(file.path(dir, 'current', 'us_eco_l3_west.shp'))

## States ----
states <- st_read(file.path(dir, 'current', 'states_west_clip.shp'))

#### reclassification matrices ####
## binary suitability classes ----
rcl_bin_mat <- t(matrix(c(c(0, 0.72, 0), c(0.72, 1, 1)),
                        nrow = 3, ncol = 2))

## multi-suitability classes ----
# 4: 0-0.05
# 3: 0.05-0.36
# 2: 0.36-0.72
# 1: 0.72-0.1
rcl_prob_mat <- t(matrix(c(c(-Inf, 0.05, 4),
                           c(0.05, 0.36, 3),
                           c(0.36, 0.72, 2),
                           c(0.72, 1, 1)),
                         nrow = 3, ncol = 4))

## elevation classes 200 meter intervals -84 ft to 4139 m ----
rcl_elev_mat <- t(matrix(c(c(-100, 100, 1),
                           c(100, 300, 2),
                           c(300, 500, 3),
                           c(500, 700, 4),
                           c(700, 900, 5),
                           c(900, 1100, 6), 
                           c(1100, 1300, 7),
                           c(1300, 1500, 8),
                           c(1500, 1700, 9),
                           c(1700, 1900, 10),
                           c(1900, 2100, 11),
                           c(2100, 2300, 12),
                           c(2300, 2500, 13),
                           c(2500, 2700, 14),
                           c(2700, 2900, 15),
                           c(2900, 3100, 16),
                           c(3100, 3300, 17),
                           c(3300, 3500, 18),
                           c(3500, 3700, 19),
                           c(3700, 3900, 20),
                           c(3900, 4100, 21),
                           c(4100, 4300, 22)),
                         nrow = 3, ncol = 22))

#### Suitability data ####
## current prediction, projected so we can estimate total area ----
current_pred <- rast(file.path(dir,
                               'current',
                               'predictions',
                               'vedu_prob_1982_2012_median.tif'))

## future predictions, projected so we can estimate total area ----
future_preds <- list.dirs(file.path(dir, 'future', 'gcm'), 
                          full.names = T,
                          recursive = F)[1:30] %>%
  map(~rast(file.path(.x, 'predictions', 'vedu_prob_2069_2099_median.tif'))) %>%
  setNames(gcm_names)

#### Other data to use in summarizing suitability ####
## elevation ----
elev_m <- rast(file.path(dir, 'current', 'DEM_usWest.tif')) %>%
  project("EPSG:5070")  %>%
  classify(rcl_elev_mat)

## current BRT variability ----
brt_uncert <- rast(file.path(dir, 'current', 'predictions', 
                             'vedu_prob_1982_2012_std.tif')) 
## GCM variability ----
gcm_uncert <- rast(file.path(dir, 'future', 'gcm', 'Summaries', 
                             'vedu_prob_2069_2099_Std_allGCMS.tif')) 

## future BRT variability ----
fut_brt_uncert <- rast(file.path(dir, 'future', 'gcm', 'Summaries',
                                 'Average_BRT_uncertainty.tif'))

## median future prediction (for uncertainty summary) ----
gcm_median <- rast(file.path(dir, 'future', 'gcm', 'Summaries',
                             'vedu_prob_2069_2099_Median_allGCMS.tif'))

###############################################################################
# niche shifts with respect to elevation and latitude
###############################################################################

#### suitability by elevation: summary data frames ####
# calculate latitude thirds
low_mid_lat <- as.data.frame(current_pred, na.rm=T, xy=T)$y %>%
  quantile(c(0.33, 0.66))


# reclassify into suitability classes, convert to a data frame, add latitude class
# convert back to SpatRaster, reproject, add elevation, then summarize by 
# elevation class, probability class, and latitude class 
# 22 elevation x 4 prob x 3 lat = 264 for all possible combinations
current_elev_summary <- current_pred %>%
  classify(rcl = rcl_prob_mat) %>%
  as.data.frame(na.rm = F, xy = T) %>%
  mutate(lat_class = case_when(
    y <= low_mid_lat[1] & !is.na(vedu_prob_1982_2012_median) ~ 1,
    (y > low_mid_lat[1] & y <= low_mid_lat[2]) & !is.na(vedu_prob_1982_2012_median) ~ 2,
    y > low_mid_lat[2] & !is.na(vedu_prob_1982_2012_median) ~ 3)) %>%  
  rast(type='xyz', crs='EPSG:4326') %>%
  project('EPSG:5070', method='near') %>%
  c(elev_m) %>%
  as.data.frame(na.rm=T) %>%
  group_by(DEM_usWest, vedu_prob_1982_2012_median, lat_class) %>%
  summarise(class_count = n()) %>%
  mutate(title = 'Current (PRISM)',
         latitude_class = case_when(lat_class == 1 ~ 'Low Lat.',
                                    lat_class == 2 ~ 'Mid Lat.', 
                                    lat_class == 3 ~ 'High Lat.'))

# calculate same summary for future predictions
fut_elev_summary<- future_preds %>%
  imap(function(pred, gcm_name) {
    pred %>%
      classify(rcl = rcl_prob_mat) %>%
      as.data.frame(na.rm = F, xy = T) %>%
      mutate(lat_class = case_when(
        y <= low_mid_lat[1] & !is.na(vedu_prob_2069_2099_median) ~ 1,
        (y > low_mid_lat[1] & y <= low_mid_lat[2]) & !is.na(vedu_prob_2069_2099_median) ~ 2,
        y > low_mid_lat[2] & !is.na(vedu_prob_2069_2099_median) ~ 3)) %>%
      rast(type='xyz', crs='EPSG:4326') %>%
      project('EPSG:5070', method='near') %>%
      c(elev_m) %>%
      as.data.frame(na.rm = T) %>%
      group_by(DEM_usWest, vedu_prob_2069_2099_median, lat_class) %>%
      summarise(class_count = n()) %>%
      mutate(gcm = gcm_name,
             latitude_class = case_when(lat_class == 1 ~ 'Low Lat.',
                                        lat_class == 2 ~ 'Mid Lat.', 
                                        lat_class == 3 ~ 'High Lat.'))
  }) %>%
  bind_rows()

# average elevation for high and moderate suitability by latitude without
# classifying elevation 
current_avg_elev <- current_pred %>%
  classify(rcl = rcl_prob_mat) %>%
  as.data.frame(na.rm = F, xy = T) %>%
  mutate(lat_class = case_when(
    y <= low_mid_lat[1] & !is.na(vedu_prob_1982_2012_median) ~ 1,
    (y > low_mid_lat[1] & y <= low_mid_lat[2]) & !is.na(vedu_prob_1982_2012_median) ~ 2,
    y > low_mid_lat[2] & !is.na(vedu_prob_1982_2012_median) ~ 3)) %>%  
  rast(type='xyz', crs='EPSG:4326') %>%
  project('EPSG:5070', method='near') %>% 
  c(rast(file.path(dir, 'current', 'DEM_usWest.tif')) %>% project("EPSG:5070")) %>%
  as.data.frame(na.rm = T, xy = T) %>%
  mutate(title = 'Current (PRISM)',
         latitude_class = factor(case_when(lat_class == 1 ~ 'Low',
                                    lat_class == 2 ~ 'Medium', 
                                    lat_class == 3 ~ 'High'),
                                 levels = c('High', 'Medium', 'Low'))) %>%
  select(vedu_prob_1982_2012_median, latitude_class, DEM_usWest) %>%
  filter(vedu_prob_1982_2012_median %in% c(2,1)) %>%
  group_by(latitude_class) %>% # just drop the grouping variable for west wide summary
  summarise(value = quantile(DEM_usWest, c(0.25, 0.5, 0.75)), 
            quant = c(0.25, 0.5, 0.75), 
            avg_elev = mean(DEM_usWest))

# average elevation for high and moderate suitability by latitude without
# classifying elevation for 4 selected gcms
# c('MPI-ESM-MR', 'MRI-CGCM3', 'FIO-ESM', 'CNRM-CM5') %>%
fut_avg_elev <- future_preds %>% 
  imap(function(pred, gcm_name) {
    pred %>%
      classify(rcl = rcl_prob_mat) %>%
      as.data.frame(na.rm = F, xy = T) %>%
      mutate(lat_class = case_when(
        y <= low_mid_lat[1] & !is.na(vedu_prob_2069_2099_median) ~ 1,
        (y > low_mid_lat[1] & y <= low_mid_lat[2]) & !is.na(vedu_prob_2069_2099_median) ~ 2,
        y > low_mid_lat[2] & !is.na(vedu_prob_2069_2099_median) ~ 3)) %>%  
      rast(type='xyz', crs='EPSG:4326') %>%
      project('EPSG:5070', method='near') %>% 
      c(rast(file.path(dir, 'current', 'DEM_usWest.tif')) %>% project("EPSG:5070")) %>%
      as.data.frame(na.rm = T, xy = T) %>%
      mutate(gcm = gcm_name,
             latitude_class = factor(case_when(lat_class == 1 ~ 'Low',
                                               lat_class == 2 ~ 'Medium', 
                                               lat_class == 3 ~ 'High'),
                                     levels = c('High', 'Medium', 'Low')))
  }) %>%
  bind_rows() %>%
  select(vedu_prob_2069_2099_median, latitude_class, DEM_usWest, gcm) %>%
  filter(vedu_prob_2069_2099_median %in% c(2,1)) %>%
  group_by(latitude_class, gcm) %>% # just drop the latitude_class grouping variable for west wide summary
  summarise(value = quantile(DEM_usWest, c(0.25, 0.5, 0.75)), 
            quant = c(0.25, 0.5, 0.75), 
            avg_elev = mean(DEM_usWest))


#### plots ####
## current suitability across the elevation gradient ----
p1 <- current_elev_summary %>%
  ggplot(aes(x = (DEM_usWest-1)*200,
             y = (class_count*818.5842^2)/1000000000, # km2 x 1000
             fill = factor(vedu_prob_1982_2012_median,
                           levels = c(4, 3, 2, 1),
                           labels = c('Unsuitable ', 
                                      'Low',
                                      'Moderate',
                                      'High')),
             color = factor(vedu_prob_1982_2012_median,
                           levels = c(4, 3, 2, 1),
                           labels = c('Unsuitable ', 
                                      'Low',
                                      'Moderate',
                                      'High')))) +
  theme_grey() +
  geom_bar(stat = 'identity', width = 200) +
  scale_x_continuous(n.breaks = 11, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d(direction = 1) +
  scale_color_viridis_d(direction = 1) +
  labs(y = '', x = '', fill = '') +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(color = 'white', fill = 'white'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(5, 7, 3, 7)) +
  guides(color = 'none') +
  facet_grid(. ~ title,
             scales = 'free')

## composition of each elevation class by suitability class ----
p2 <- current_elev_summary %>%
  ggplot(aes(x = (DEM_usWest-1)*200,
             y = class_count,
             fill = factor(vedu_prob_1982_2012_median,
                           levels = c(4, 3, 2, 1),
                           labels = c('Unsuitable ', 
                                      'Low',
                                      'Moderate',
                                      'High')),
             color = factor(vedu_prob_1982_2012_median,
                           levels = c(4, 3, 2, 1),
                           labels = c('Unsuitable ', 
                                      'Low',
                                      'Moderate',
                                      'High')))) +
  geom_bar(position = 'fill', stat = 'identity', width = 200) +
  scale_x_continuous(n.breaks = 11, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d(direction = 1) +
  scale_color_viridis_d(direction = 1) +
  theme_grey() +
  labs(y = '', x = '', fill = '') +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 7, 0, 7)) +
  guides(color = 'none') +
  facet_grid(. ~ title)

## same as above but for all gcms ----
p3 <- fut_elev_summary %>%
  ggplot(aes(x = (DEM_usWest-1)*200,
             y = class_count,
             fill = factor(vedu_prob_2069_2099_median,
                           levels = c(4, 3, 2, 1),
                           labels = c('Unsuitable ', 
                                      'Low',
                                      'Moderate',
                                      'High')),
             color = factor(vedu_prob_2069_2099_median,
                           levels = c(4, 3, 2, 1),
                           labels = c('Unsuitable ', 
                                      'Low',
                                      'Moderate',
                                      'High')))) +
  geom_bar(position = 'fill', stat = 'identity', width = 200) +
  scale_x_continuous(n.breaks = 6, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d(direction = 1) +
  scale_color_viridis_d(direction = 1) +
  theme_grey() +
  labs(y = '', x = 'Elevation (m)', fill = '') +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'none',
        plot.margin = margin(5, 0, 5, 5)) +
  guides(color = 'none') +
  facet_wrap(. ~ gcm)

legend <- get_legend(p1 + theme(legend.box.margin = margin(-20, 0, 20, 0)))

p_col <- plot_grid(p1 + theme(legend.position = 'none'),
                   p2,
                   legend,
                   labels = c('B', 'C'),
                   align = 'vh',
                   nrow = 3,
                   axis = 'r')

final_plot <- plot_grid(p3, p_col,
               rel_widths = c(1, 0.3),
               labels = c('A'),
               nrow = 1)

ggsave(filename = 'Suitability_by_elevation.pdf',
       path = file.path(fig_dir, 'future'),
       plot = final_plot,
       scale = 1.5,
       width = 6.5,
       height = 4,
       units = 'in',
       dpi = 1200,
       bg = 'white')

## same as p1 but with gcm data ----
p4 <- current_elev_summary %>%
  filter(vedu_prob_1982_2012_median %in% c(1,2,3)) %>%
  ggplot(aes(x = (DEM_usWest-1)*200,
             y = (class_count*818.5842^2)/1000000000, # thousand km2
             fill = factor(vedu_prob_1982_2012_median,
                           levels = c(4, 3, 2, 1),
                           labels = c('Unsuitable ', 
                                      'Low',
                                      'Moderate',
                                      'High')),
             color = factor(vedu_prob_1982_2012_median,
                           levels = c(4, 3, 2, 1),
                           labels = c('Unsuitable ', 
                                      'Low',
                                      'Moderate',
                                      'High')))) +
  theme_grey() +
  geom_bar(stat = 'identity', width = 200) +
  scale_x_continuous(n.breaks = 11, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c('#30678d', '#34b678', '#fce624')) +
  scale_color_manual(values = c('#30678d', '#34b678', '#fce624')) +
  labs(y = 'Area (thousand sq. km.)', x = 'Elevation (m)', fill = '') +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(color = 'white', fill = 'white'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(5, 7, 3, 7)) +
  guides(color = 'none') +
  facet_grid(. ~ title)

p5 <- fut_elev_summary %>%
  filter(vedu_prob_2069_2099_median %in% c(1,2,3)) %>%
  ggplot(aes(x = (DEM_usWest-1)*200,
             y = (class_count*818.5842^2)/1000000000, # km2
             fill = factor(vedu_prob_2069_2099_median,
                           levels = c(4, 3, 2, 1),
                           labels = c('Unsuitable ', 
                                      'Low',
                                      'Moderate',
                                      'High')),
             color = factor(vedu_prob_2069_2099_median,
                           levels = c(4, 3, 2, 1),
                           labels = c('Unsuitable ', 
                                      'Low',
                                      'Moderate',
                                      'High')))) +
  theme_grey() +
  geom_bar(stat = 'identity', width = 200) +
  scale_x_continuous(n.breaks = 7, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c('#30678d', '#34b678', '#fce624')) +
  scale_color_manual(values = c('#30678d', '#34b678', '#fce624')) +
  labs(y = 'Area (thousand sq. km.)', x = 'Elevation (m)', fill = '') +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(color = 'white', fill = 'white'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none',
        plot.margin = margin(5, 7, 3, 7)) +
  guides(color = 'none') +
  facet_wrap(~gcm)

legend2 <- get_legend(p4 + theme(legend.box.margin = margin(-10, 0, 10, 0)))

p_col2 <- plot_grid(NULL,
                    p4 + theme(legend.position = 'none'),
                    legend2,
                    NULL,
                    labels = c('','B','',''),
                    rel_heights = c(0.5,1.25,1,0.5),
                    align = 'vh',
                    nrow = 4,
                    axis = 'l')

final_plot2 <- plot_grid(p5, p_col2,
                         rel_widths = c(1, 0.3),
                         labels = c('A'),
                         nrow = 1)

ggsave(filename = 'Suitable_only_by_elevation.pdf',
       path = file.path(fig_dir, 'future'),
       plot = final_plot2,
       scale = 1.5,
       width = 6.5,
       height = 5,
       units = 'in',
       dpi = 1200,
       bg = 'white')

## split by latitude ----
# raster with latitude bands
lat_band_rast <- current_pred %>%
  as.data.frame(na.rm = T, xy = T) %>%
  mutate(lat_class = case_when(
    y <= low_mid_lat[1] & !is.na(vedu_prob_1982_2012_median) ~ 'Low Lat.',
    (y > low_mid_lat[1] & y <= low_mid_lat[2]) & !is.na(vedu_prob_1982_2012_median) ~ 'Mid Lat.',
    y > low_mid_lat[2] & !is.na(vedu_prob_1982_2012_median) ~ 'High Lat.'),
    title = 'Latitudinal Divisions') 

# map of latitudinal bands
lat_map <- lat_band_rast %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, 
                  fill = factor(lat_class, 
                                levels = c('High Lat.', 'Mid Lat.', 'Low Lat.')))) +
  scale_fill_grey(start = 0.3, end = 0.7) +
  scale_y_continuous(expand = c(0.005,0.005)) +
  scale_x_continuous(expand = c(0.005,0.005)) +
  geom_sf(data = west_eco, fill = NA, color = 'white', size = 0.15) +
  theme_grey() +
  labs(x = '', y = '', fill = '') +
  theme(panel.background = element_rect(color = 'white', 
                                        fill = 'white'),
        panel.grid.major = element_line(color = 'grey50', 
                                        size = 0.25, 
                                        linetype = 'dashed'), 
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        plot.margin = margin(-10,2,0,-10),
        legend.box.margin = margin(-20,0,0,-30),
        legend.text = element_text(size = 7),
        axis.text = element_text(size = 6)) +
  facet_grid(.~title)

ggsave(filename = 'latitudinal_divisions.tiff',
       path = file.path(fig_dir, 'future'),
       plot = lat_map + theme(plot.margin = margin(2,2,2,2)),
       scale = 1,
       width = 3,
       height = 4,
       units = 'in',
       dpi = 1200,
       compression = 'lzw',
       bg = 'white')

## suitability by latitude and elevation ----
# combine current and future elevation summaries
combined_elev <- current_elev_summary %>%
  rename(suit = vedu_prob_1982_2012_median, 
         gcm = title) %>%
  bind_rows(fut_elev_summary %>% rename(suit = vedu_prob_2069_2099_median))

p6 <- combined_elev %>%
  filter(gcm %in% c('Current (PRISM)','MPI-ESM-MR', 'MRI-CGCM3', 'FIO-ESM', 'CNRM-CM5')) %>%
  ggplot(aes(x = (DEM_usWest-1)*200,
             y = class_count,
             fill = factor(suit,
                           levels = c(4, 3, 2, 1),
                           labels = c('Unsuitable ', 
                                      'Low',
                                      'Moderate',
                                      'High')),
             color = factor(suit,
                           levels = c(4, 3, 2, 1),
                           labels = c('Unsuitable ', 
                                      'Low',
                                      'Moderate',
                                      'High')))) +
  geom_bar(position = 'fill', stat = 'identity', width = 200) +
  scale_x_continuous(n.breaks = 6, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(0.25, 0.75)) +
  scale_fill_viridis_d(direction = 1) +
  scale_color_viridis_d(direction = 1) +
  theme_grey() +
  labs(y = 'Proportion', x = 'Elevation (m)', fill = '') +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background.y = element_blank(),
        strip.text.y = element_blank(),
        axis.text = element_text(size = 7),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.margin = margin(-8, 0, 1, 0),
        plot.margin = margin(2, 0, 2, 2)) +
  guides(color = 'none') +
  facet_grid(factor(latitude_class, 
                    levels = c('High Lat.', 'Mid Lat.', 'Low Lat.'))
             ~ factor(gcm, 
                      levels = c('Current (PRISM)', 'FIO-ESM','CNRM-CM5',
                                 'MRI-CGCM3',  'MPI-ESM-MR')))

# elevation profile of western us broken down by latitude
elev_profile <- current_elev_summary %>%
  mutate(facet_title = 'Elevation Profile') %>%
  ggplot(aes(x = (DEM_usWest-1)*200,
             y = (class_count*818.5842^2)/1000000000)) + # convert pixel count to 1000 x km2
  geom_bar(stat = 'identity', width = 200, 
           fill = 'grey25', color = 'grey25') +
  scale_x_continuous(n.breaks = 6, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_grey() +
  labs(y = 'Area (km² x 1000)', x = '') +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 7),
        legend.position = 'none',
        plot.margin = margin(2, 2, 21, 4)) +
  facet_grid(factor(latitude_class, 
                    levels = c('High Lat.', 'Mid Lat.', 'Low Lat.'))
             ~ facet_title)

combined_plot <- plot_grid(p6 , elev_profile,
                           rel_widths = c(1, 0.3),
                           labels = c('A', 'B'),
                           align = 'v', 
                           axis = 't',
                           ncol = 2)

ggsave(filename = 'Suitability_by_lat_elev.pdf',
       path = file.path(fig_dir, 'future'),
       plot = combined_plot,
       scale = 1.5,
       width = 6.5,
       height = 3,
       units = 'in',
       dpi = 1200,
       bg = 'white')

## same as p5 and p6 but only for selected gcms and low,mod,high suitability ----
combined_elev %>%
  filter(gcm %in% c('Current (PRISM)','MPI-ESM-MR', 'MRI-CGCM3', 'FIO-ESM', 'CNRM-CM5')) %>%
  filter(suit %in% c(1,2,3)) %>%
  ggplot(aes(x = (DEM_usWest-1)*200,
             y = (class_count*818.5842^2)/1000000000, # thousand km2 X 1000
             fill = factor(suit,
                           levels = c(4, 3, 2, 1),
                           labels = c('Unsuitable ', 
                                      'Low',
                                      'Moderate',
                                      'High')),
             color = factor(suit,
                           levels = c(4, 3, 2, 1),
                           labels = c('Unsuitable ', 
                                      'Low',
                                      'Moderate',
                                      'High')))) +
  theme_grey() +
  geom_bar(stat = 'identity', width = 200) +
  scale_x_continuous(n.breaks = 7, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c('#30678d', '#34b678', '#fce624')) +
  scale_color_manual(values = c('#30678d', '#34b678', '#fce624')) +
  labs(y = 'Area (thousand sq. km.)', x = 'Elevation (m)', fill = '') +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(color = 'white', fill = 'white'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.margin = margin(-8, 0, 1, 0),
        plot.margin = margin(2, 0, 2, 2)) +
  guides(color = 'none') +
  facet_grid(factor(latitude_class, 
                    levels = c('High Lat.', 'Mid Lat.', 'Low Lat.'))
             ~ factor(gcm, 
                      levels = c('Current (PRISM)', 'FIO-ESM','CNRM-CM5',
                                 'MRI-CGCM3',  'MPI-ESM-MR')))

ggsave(filename = 'Suitable_area_by_lat_elev.pdf',
       path = file.path(fig_dir, 'future'),
       plot = last_plot(),
       scale = 1.5,
       width = 4.5,
       height = 3,
       units = 'in',
       dpi = 1200,
       bg = 'white')

###############################################################################
# suitability agreement and ecoregion vulnerability
###############################################################################

## count the number of times a pixel is predicted to be suitable ----
suit_freq <- list.dirs(file.path(dir, 'future', 'gcm'), 
                       full.names = T, 
                       recursive = F)[1:30] %>%
  map(~raster(file.path(.x, 'predictions', 'vedu_prob_2069_2099_median.tif')) %>%
        reclassify(rcl = rcl_bin_mat)) %>%
  stack() %>%
  sum()

writeRaster(suit_freq,
            filename = file.path(dir, 'future', 'gcm', 'Summaries',
                                 'Suitable_count_allgcms.tif'),
            format = 'GTiff',
            options = c('COMPRESS=LZW', 'TILED=YES',
                        'BLOCKXSIZE=256', 'BLOCKYSIZE=256'),
            overwrite = T)

## which ecoregions have the greatest sum of future suitability per unit area? ----
west_eco <- west_eco %>%
  mutate(suit_per_area = rep(NA,))

# count the sum of suitability and divide by ecoregion area
# have to call the raster package explicitly because dplyr masks extract
suit_sum_per_eco <- raster::extract(suit_freq, 
                                    west_eco,
                                    fun = function(x,...){sum(x)},
                                    na.rm = T,
                                    df = T, 
                                    exact = F)

## append to spatial data frame ----
west_eco <- west_eco %>% 
  mutate(suit_weight = suit_sum_per_eco$layer) %>%
  select(suit_sum_per_eco, US_L3NAME, geometry)

# save to disk
st_write(west_eco, file.path(dir, 
                             'future', 
                             'gcm',
                             'Summaries', 
                             'Suitable_count_sum.shp'))

###############################################################################
# suitability invaded range vs west
###############################################################################

#### current ####

# classify and mask the current suitability raster
suit_in_range <- current_pred %>%
  project('EPSG:5070') %>%
  classify(rcl = rcl_prob_mat) %>%
  setNames('prob_class') %>%
  mask(vect(conv_hull), touches = F) %>% 
  as.data.frame(na.rm = T) %>% 
  group_by(prob_class) %>%
  summarise(area_km2 = (n()*(818.5842^2))/1000000) %>% # class area in km2; 818.5842 is resolution in meters 
  mutate(class_name = case_when(prob_class == 1 ~ 'high',
                                prob_class == 2 ~ 'moderate',
                                prob_class == 3 ~ 'low',
                                prob_class == 4 ~ 'unsuitable'),
         percent = (area_km2/(sum(across(area_km2)))*100),
         range = 'inside') 


suit_outside_range <- current_pred %>%
  project('EPSG:5070') %>% 
  classify(rcl = rcl_prob_mat) %>%
  setNames('prob_class') %>%
  mask(vect(conv_hull), inverse = T, touches = F) %>%
  as.data.frame(na.rm = T) %>%
  group_by(prob_class) %>%
  summarise(area_km2 = (n()*(818.5842^2))/1000000) %>% # class area in km2; 818.5842 is resolution in meters 
  mutate(class_name = case_when(prob_class == 1 ~ 'high',
                                prob_class == 2 ~ 'moderate',
                                prob_class == 3 ~ 'low',
                                prob_class == 4 ~ 'unsuitable'),
         percent = (area_km2/(sum(across(area_km2)))*100),
         range = 'outside')

current_suit_summary <- bind_rows(suit_in_range, suit_outside_range) %>%
  pivot_wider(names_from =  range, values_from = c(area_km2, percent)) %>%
  mutate(percent_of_west = ((area_km2_inside + area_km2_outside)/sum(area_km2_inside, area_km2_outside))*100)

write.csv(current_suit_summary, file.path(dir, 'current/results_summaries',
                                          'suitability_summary_current.csv'))

#### future ####
future_suit_summary <- future_preds %>%
  imap(function(pred, gcm_name) {
    suit_in_range <- pred %>%
      project('EPSG:5070') %>%
      classify(rcl = rcl_prob_mat) %>%
      setNames('prob_class') %>%
      mask(vect(conv_hull), touches = F) %>%
      as.data.frame(na.rm = T) %>%
      group_by(prob_class) %>%
      summarise(area_km2 = (n()*(818.5842^2))/1000000) %>% # class area in km2; 818.5842 is resolution in meters 
      mutate(class_name = case_when(prob_class == 1 ~ 'high',
                                    prob_class == 2 ~ 'moderate',
                                    prob_class == 3 ~ 'low',
                                    prob_class == 4 ~ 'unsuitable'),
             percent = (area_km2/(sum(across(area_km2)))*100),
             range = 'inside') 
    
    
    suit_outside_range <- pred %>%
      project('EPSG:5070') %>%
      classify(rcl = rcl_prob_mat) %>%
      setNames('prob_class') %>%
      mask(vect(conv_hull), inverse = T, touches = F) %>%
      as.data.frame(na.rm = T) %>%
      group_by(prob_class) %>%
      summarise(area_km2 = (n()*(818.5842^2))/1000000) %>% # class area in km2; 818.5842 is resolution in meters 
      mutate(class_name = case_when(prob_class == 1 ~ 'high',
                                    prob_class == 2 ~ 'moderate',
                                    prob_class == 3 ~ 'low',
                                    prob_class == 4 ~ 'unsuitable'),
             percent = (area_km2/(sum(across(area_km2)))*100),
             range = 'outside')
    
    bind_rows(suit_in_range, suit_outside_range) %>%
      pivot_wider(names_from =  range, values_from = c(area_km2, percent)) %>%
      mutate(percent_of_west = ((area_km2_inside + area_km2_outside)/sum(area_km2_inside, area_km2_outside))*100,
             gcm = gcm_name)
    
  }) %>%
  bind_rows()

write.csv(future_suit_summary, file.path(dir, 'future/gcm/Summaries',
                                          'suitability_summary_future.csv'))

###############################################################################
# suitability per ecoregion and associated elevation stats
###############################################################################

## some prep code ----
# read in original elevation (before it was reclassified)
elev_m <- rast(file.path(dir, 'current', 'DEM_usWest.tif')) %>%
  project("EPSG:5070")

# matrix to classify suitability into 2 classes based on marginal suitability 
# or greater
rcl_suit_mat <- t(matrix(c(c(0, 0.36, NA), c(0.36, 1, 1)),
                        nrow = 3, ncol = 2))

# names of each ecoregion to iterate over
eco_names <- west_eco$US_L3NAME %>% unique()

# reporject ecoregions to epsg 5070
west_eco <- west_eco %>%
  st_transform(crs = st_crs(5070)) %>%
  select(US_L3NAME)

# reclassify prob to classes
suit_and_marg_suit <- current_pred %>%
  project("EPSG:5070") %>%
  classify(rcl = rcl_suit_mat)

## count the number of suitable classes in each ecoregion (area) ----
# calculate summary stats for maringally suitable and suitable areas
suit_summary_by_ecoregion <- eco_names %>%
  map(function(eco_name) {
    # filter to only this ecoregion
    ecoreg <- west_eco %>% filter(US_L3NAME == eco_name) %>% vect()
    
    # mask the binary suitability and maringal suitability layer
    suit_masked <- suit_and_marg_suit %>%
      mask(ecoreg, touches = F)
    
    # count the number of cells that are suitable or marginally suitable
    suit_sum <- suit_masked %>%
      global(fun = 'sum', na.rm = T) %>%
      .$sum
    
    # elevation masked by ecoregion and suitability
    elev_masked <- elev_m %>%
      mask(suit_masked)
    
    # mean elevation of suitable or marginally suitable cells
    elev_mean <- elev_masked %>%
      global(fun = 'mean', na.rm = T) %>%
      .$mean
    
    # mean elevation of suitable or marginally suitable cells
    elev_max <- elev_masked %>%
      global(fun = 'max', na.rm = T) %>%
      .$max
    
    # mean elevation of suitable or marginally suitable cells
    elev_min <- elev_masked %>%
      global(fun = 'min', na.rm = T) %>%
      .$min
    
    # return dataframe with these values
    data.frame(US_L3NAME = eco_name,
               suit_count = suit_sum,
               suit_area_km = (suit_sum*(818.5842^2))/1000000,
               elev_mean_suit = elev_mean,
               elev_min_suit = elev_min,
               elev_max_suit = elev_max)
  }) %>%
  bind_rows() 

# make NA's consistent
suit_summary_by_ecoregion <- suit_summary_by_ecoregion %>%
  na_if(Inf) %>%
  na_if(-Inf) %>%
  na_if('NaN')

write.csv(suit_summary_by_ecoregion, 
          file.path(dir, 'current', 'results_summaries',
                    'suit_summary_by_ecoregion.csv'))

###############################################################################
# uncertainty summary
###############################################################################

#### current ####
# how does the range/uncertainty associate with different predictions ----
# make sure current_pred isnt projected to 5070 before this code is run
current_uncert <- current_pred %>%
  classify(rcl = rcl_prob_mat) %>%
  c(brt_uncert) %>%
  as.data.frame(na.rm = T) %>%
  group_by(vedu_prob_1982_2012_median) %>%
  summarise(class_avg_uncert = mean(vedu_prob_1982_2012_std),
            class_min_uncert = min(vedu_prob_1982_2012_std),
            class_max_uncert = max(vedu_prob_1982_2012_std))

#### future ####
## how does the uncertainty from gcms associate with different predictions ----
fut_gcm_uncert_summary <- gcm_median %>%
  classify(rcl = rcl_prob_mat) %>%
  c(gcm_uncert) %>%
  as.data.frame(na.rm = T) %>%
  group_by(vedu_prob_2069_2099_Median_allGCMS) %>%
  summarise(class_avg_uncert = mean(vedu_prob_2069_2099_Std_allGCMS),
            class_min_uncert = min(vedu_prob_2069_2099_Std_allGCMS),
            class_max_uncert = max(vedu_prob_2069_2099_Std_allGCMS))

# how does the uncertainty from brt associate with different predictions ----
# when summarised by the future median prediction across gcms
fut_brt_uncert_summary <- gcm_median %>%
  classify(rcl = rcl_prob_mat) %>%
  c(fut_brt_uncert) %>%
  as.data.frame(na.rm = T) %>%
  group_by(vedu_prob_2069_2099_Median_allGCMS) %>%
  summarise(class_avg_uncert = mean(Average_BRT_uncertainty),
            class_min_uncert = min(Average_BRT_uncertainty),
            class_max_uncert = max(Average_BRT_uncertainty))
