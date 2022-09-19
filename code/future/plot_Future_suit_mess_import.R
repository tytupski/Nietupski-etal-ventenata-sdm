###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: future plots
#     1. climatic suitablity
#     2. novel climate
#     3. importance
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('terra', 'sf', 'tidyverse', 'gridExtra', 'cowplot', 'ggrepel', 'here')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

###############################################################################
# data
###############################################################################

# relevant directories
dir <- here('Data', 'future', 'gcm')
fig_dir <- here('Figures', 'future', 'maps')

# gcms of interest
gcms <- c('MPI-ESM-MR', 'MRI-CGCM3', 'FIO-ESM', 'CNRM-CM5') 

# state boundaries
states <- st_read(here('Data', 'current', 'states_west_clip.shp'))

# ecoregion boundaries
ecoreg <- st_read(here('Data', 'current', 'us_eco_l3_west.shp'))

#### vedu prediction ####
vedu_pred <- gcms %>%
  map(function(gcm) {
    rast(file.path(dir, tolower(gcm), 'predictions','vedu_prob_2069_2099_median_classes.tif')) %>%
      setNames(c('prob')) %>%
      as.data.frame(xy = T, na.rm = T) %>%
      mutate(title = gcm)
  }) %>%
  bind_rows()

#### environmental novelty ####
mess <- gcms %>%
  map(function(gcm) {
    rast(file.path(dir, tolower(gcm),
                   'novel_climate',
                   'MESS_MoD_vedu_vs_future_masked.tif')) %>%
      setNames(c('mess')) %>%
      as.data.frame(xy = T, na.rm = T) %>%
      mutate(title = gcm)
  }) %>%
  bind_rows()

#### spatial importance ####
importance <- gcms %>%
  map(function(gcm) {
    list.files(file.path(dir, tolower(gcm), 'spatial_importance'),
               pattern = '.*imp.*.tif$',
               recursive = F, 
               full.names = T) %>%
      rast() %>%
      modal(ties='first') %>%
      setNames(c('imp')) %>%
      as.data.frame(xy = T, na.rm = T) %>%
      mutate(title = gcm)
  }) %>%
  bind_rows()

###############################################################################
# plots
###############################################################################

#### Separate plots ####
# probability plot
p1 <- vedu_pred %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, 
                  fill = factor(prob, levels = c(4, 3, 2, 1),
                                labels = c('Unsuitable ', 
                                           'Low',
                                           'Moderate',
                                           'High')))) +
  scale_fill_viridis_d() +
  scale_y_continuous(expand = c(0.005,0.005)) +
  scale_x_continuous(expand = c(0.005,0.005)) +
  geom_sf(data = ecoreg, fill = NA, color = '#d3d3d3', size = 0.15) +
  theme_grey() +
  labs(x = '', y = '', fill = '') +
  theme(panel.background = element_rect(color = 'white', 
                                        fill = 'white'),
        panel.grid.major = element_line(color = 'grey50', 
                                        size = 0.25, 
                                        linetype = 'dashed'), 
        axis.ticks = element_blank(),
        legend.position = 'bottom', 
        legend.direction = 'horizontal') +
  facet_wrap(.~factor(title, levels = c('MPI-ESM-MR', 'MRI-CGCM3',
                                        'FIO-ESM', 'CNRM-CM5')))

ggsave(filename = 'Future_Suit.png',
       path = fig_dir,
       plot = p1,
       device = 'png',
       scale = 1.4,
       width = 4,
       height = 5,
       units = 'in',
       dpi = 300,
       bg = 'white')

p2 <- mess %>%
  ggplot() +
  geom_sf(data = states, fill = 'white', color = 'white', size = 0.2) +
  geom_raster(aes(x = x, y = y, 
                  fill = factor(mess, levels = c(1,2,3,4,5,6),
                                labels = c('bio2 ', 
                                           'bio4',
                                           'bio10',
                                           'bio11', 
                                           'bio15',
                                           'csppt')))) +
  # scale_fill_brewer(palette = 'Set2') +
  scale_fill_manual(values = c('#e69f00', '#d55e00', '#cc79a7',
                               '#0072b2', '#009e73', '#f0e442')) +
  scale_y_continuous(expand = c(0.005,0.005)) +
  scale_x_continuous(expand = c(0.005,0.005)) +
  geom_sf(data = ecoreg, fill = NA, color = '#d3d3d3', size = 0.2) +
  theme_grey() +
  labs(x = '', y = '', fill = '') +
  theme(panel.background = element_rect(color = 'white', 
                                        fill = 'white'),
        panel.grid.major = element_line(color = 'grey50', 
                                        size = 0.25, 
                                        linetype = 'dashed'), 
        axis.ticks = element_blank(),
        legend.position = 'bottom', 
        legend.direction = 'horizontal') +
  facet_wrap(.~factor(title, levels = c('MPI-ESM-MR', 'MRI-CGCM3',
                                        'FIO-ESM', 'CNRM-CM5')))

ggsave(filename = 'Future_MESS.png',
       path = fig_dir,
       plot = p2,
       device = 'png',
       scale = 1.4,
       width = 4,
       height = 5,
       units = 'in',
       dpi = 300,
       bg = 'white')

p3 <- importance %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, 
                  fill = factor(imp, levels = c(1,2,3,4,5,6),
                                labels = c('bio2 ', 
                                           'bio4',
                                           'bio10',
                                           'bio11', 
                                           'bio15',
                                           'csppt')))) +
  # scale_fill_brewer(palette = 'Set2') +
  scale_fill_manual(values = c('#e69f00', '#d55e00', '#cc79a7',
                               '#0072b2', '#009e73', '#f0e442')) +
  scale_y_continuous(expand = c(0.005,0.005)) +
  scale_x_continuous(expand = c(0.005,0.005)) +
  geom_sf(data = ecoreg, fill = NA, color = '#797979', size = 0.2) +
  theme_grey() +
  labs(x = '', y = '', fill = '') +
  theme(panel.background = element_rect(color = 'white', 
                                        fill = 'white'),
        panel.grid.major = element_line(color = 'grey50', 
                                        size = 0.25, 
                                        linetype = 'dashed'), 
        axis.ticks = element_blank(),
        legend.position = 'bottom', 
        legend.direction = 'horizontal') +
  facet_wrap(.~factor(title, levels = c('MPI-ESM-MR', 'MRI-CGCM3',
                                        'FIO-ESM', 'CNRM-CM5')))


ggsave(filename = 'Future_Imp.png',
       path = fig_dir,
       plot = p3,
       device = 'png',
       scale = 1.4,
       width = 4,
       height = 5,
       units = 'in',
       dpi = 300,
       bg = 'white')


#### 4-panel plot ####

### schematic ####
quad_labels <- data.frame(x = c(1, 1, -1, -1),
                          y = c(-1, 1, -1, 1),
                          quad = c('High, Low', 
                                   'High, High\n(Least Similar to\n Current Climate)',
                                   'Low, Low\n(Most Similar to\n Current Climate)',
                                   'Low, High'))
arrowx <- data.frame(x1 = 2,
                     y1 = c(-2, -2),
                     x2 = -1.95,
                     y2 = c(-2, -2))
arrowy <- data.frame(x1 = c(-2, -2),
                     y1 = 2,
                     x2 = c(-2, -2),
                     y2 = -1.95)

quadx <- data.frame(x1 = 1.95,
                    y1 = c(0, 0),
                    x2 = -1.95,
                    y2 = c(0, 0))
quady <- data.frame(x1 = c(0, 0),
                    y1 = 1.95,
                    x2 = c(0, 0),
                    y2 = -1.95)

schematic <- quad_labels %>%
  ggplot(aes(x=x, y=y)) +
  geom_point(color = NA) +
  geom_text(aes(label = quad)) +
  geom_segment(data = quadx, aes(x1,y1, xend = x2, yend = y2),
               linetype = 'dashed') +
  geom_segment(data = quady, aes(x1,y1, xend = x2, yend = y2),
               linetype = 'dashed') +
  geom_segment(data = arrowx, aes(x1,y1, xend = x2, yend = y2), 
               arrow = arrow(type = 'open', ends = 'first'),
               size = 1.25, lineend = 'round', linejoin = 'round') +
  geom_segment(data = arrowy, aes(x1,y1, xend = x2, yend = y2), 
               arrow = arrow(type = 'open', ends = 'first'),
               size = 1.25, lineend = 'round', linejoin = 'round') +
  scale_y_continuous(limits = c(-2.1,2.1), expand = c(0.04, 0.04)) +
  scale_x_continuous(limits = c(-2.1,2.1), expand = c(0.03, 0.03)) +
  theme_minimal() +
  labs(x = 'Cool Season Precip', 
       y = 'Temp Seasonality',
       title = 'GCM Layout') +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(), 
        panel.grid = element_blank(),
        plot.title = element_text(hjust=0.5))

#### main plots ####
p1 <- vedu_pred %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, 
                  fill = factor(prob, levels = c(4, 3, 2, 1),
                                labels = c('Unsuitable ', 
                                           'Low',
                                           'Moderate',
                                           'High')))) +
  scale_fill_viridis_d() +
  scale_y_continuous(expand = c(0.005,0.005)) +
  scale_x_continuous(expand = c(0.005,0.005)) +
  geom_sf(data = ecoreg, fill = NA, color = '#d3d3d3', size = 0.15) +
  theme_grey() +
  labs(x = '', y = '', fill = 'Climatic Suitability') +
  theme(panel.background = element_rect(color = 'white', 
                                        fill = 'white'),
        panel.grid.major = element_line(color = 'grey50', 
                                        size = 0.25, 
                                        linetype = 'dashed'), 
        axis.ticks = element_blank(),
        legend.position = 'bottom', 
        legend.direction = 'horizontal') +
  guides(fill = guide_legend(title.position = 'top', 
                             title.hjust = 0.5)) +
  facet_wrap(.~factor(title, levels = c('MPI-ESM-MR', 'MRI-CGCM3',
                                        'FIO-ESM', 'CNRM-CM5')))

p2 <- mess %>%
  ggplot() +
  geom_sf(data = states, fill = 'white', color = 'white', size = 0.2) +
  geom_raster(aes(x = x, y = y, 
                  fill = factor(mess, levels = c(1,2,3,4,5,6),
                                labels = c('Mean Diurnal Range', 
                                           'Temperature Seasonality',
                                           'Mean Temperature of Warmest Quarter',
                                           'Mean Temperature of Coldest Quarter', 
                                           'Precipitation Seasonality',
                                           'Cool Season Precipitation')))) +
  # scale_fill_brewer(palette = 'Set2') +
  scale_fill_manual(values = c('#e69f00', '#d55e00', '#cc79a7',
                               '#0072b2', '#009e73', '#f0e442')) +
  scale_y_continuous(expand = c(0.005,0.005)) +
  scale_x_continuous(expand = c(0.005,0.005)) +
  geom_sf(data = ecoreg, fill = NA, color = 'grey25', size = 0.15) +
  theme_grey() +
  labs(x = '', y = '', fill = 'Climate Novelty and Importance') +
  theme(panel.background = element_rect(color = 'white', 
                                        fill = 'white'),
        panel.grid.major = element_line(color = 'grey50', 
                                        size = 0.25, 
                                        linetype = 'dashed'), 
        axis.ticks = element_blank()) +
  facet_wrap(.~factor(title, levels = c('MPI-ESM-MR', 'MRI-CGCM3',
                                        'FIO-ESM', 'CNRM-CM5')))

p3 <- importance %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, 
                  fill = factor(imp, levels = c(1,2,3,4,5,6),
                                labels = c('Mean Diurnal Range', 
                                           'Temperature Seasonality',
                                           'Mean Temperature of Warmest Quarter',
                                           'Mean Temperature of Coldest Quarter', 
                                           'Precipitation Seasonality',
                                           'Cool Season Precipitation')))) +
  # scale_fill_brewer(palette = 'Set2') +
  scale_fill_manual(values = c('#e69f00', '#d55e00', '#cc79a7',
                               '#0072b2', '#009e73', '#f0e442')) +
  scale_y_continuous(expand = c(0.005,0.005)) +
  scale_x_continuous(expand = c(0.005,0.005)) +
  geom_sf(data = ecoreg, fill = NA, color = 'grey25', size = 0.15) +
  theme_grey() +
  labs(x = '', y = '', fill = '') +
  theme(panel.background = element_rect(color = 'white', 
                                        fill = 'white'),
        panel.grid.major = element_line(color = 'grey50', 
                                        size = 0.25, 
                                        linetype = 'dashed'), 
        axis.ticks = element_blank(),
        legend.position = 'none') +
  facet_wrap(.~factor(title, levels = c('MPI-ESM-MR', 'MRI-CGCM3',
                                        'FIO-ESM', 'CNRM-CM5')))

suit_legend <- get_legend(p1 + theme(legend.box.margin = margin(50, 0, -100, 0)))
mess_legend <- get_legend(p2 + theme(legend.box.margin = margin(-100, 0, 50, 0)))

legends <- plot_grid(NULL, suit_legend, mess_legend, NULL,
                     rel_heights = c(0.9,1,1,0.7),
                     align = 'h',
                     nrow = 4,
                     axis = 'l')

legends_w_buffer <- plot_grid(NULL, legends,
                              rel_widths = c(0.15, 1),
                              align = 'hv',
                              nrow = 1)

schematic_w_buffer <- plot_grid(NULL, schematic, NULL,
                                rel_widths = c(0.15, 1, 0.15),
                                align = 'hv', 
                                nrow = 1)

legend_plus_schematic <- plot_grid(legends_w_buffer, schematic_w_buffer, NULL, 
                                   rel_heights = c(1,0.95, 0.1),
                                   align = 'v',
                                   nrow = 3)

top_row <- plot_grid(p1 + theme(legend.position = 'none'), legend_plus_schematic, 
                     rel_widths = c(1, 1),
                     labels = c('A'),
                     nrow = 1)  

bottom_row <- plot_grid(p2 + theme(legend.position = 'none'), p3,
                        rel_widths = c(1,1),
                        labels = c('B', 'C'),
                        nrow = 1)

all_together_now <- plot_grid(top_row, bottom_row,
                              rel_heights = c(1,1),
                              align = 'v',
                              nrow = 2,
                              axis = 'b')

ggsave(filename = 'Future_Suit_MESS_Imp.tiff',
       path = fig_dir,
       plot = all_together_now,
       scale = 1.35,
       width = 6.5,
       height = 8,
       units = 'in',
       dpi = 1200,
       compression = 'lzw',
       bg = 'white')

