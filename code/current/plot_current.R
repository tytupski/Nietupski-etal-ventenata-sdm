###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: current plots
#     1. climatic suitablity
#     2. novel climate
#     3. spatial importance
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('tidyverse', 'terra', 'sf',
         'ggpattern', 'gridExtra', 'cowplot', 'here')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

###############################################################################
# data
###############################################################################

# relevant directories
dir <- here('Data', 'current')
fig_dir <- here('Figures','current', 'maps')


# state boundaries
states <- st_read(file.path(dir, 'states_west_clip.shp'))

# ecoregion boundaries
ecoreg <- st_read(file.path(dir, 'us_eco_l3_west.shp'))

## vedu suitability ----
# to classify probablity into classes
rcl_prob_mat <- t(matrix(c(c(0, 0.05, 4),
                           c(0.05, 0.36, 3),
                           c(0.36, 0.72, 2),
                           c(0.72, 1, 1)),
                         nrow = 3, ncol = 4))
# raw suitability classified into 4 classes
vedu_pred <- rast(file.path(dir,'predictions','vedu_prob_1982_2012_median.tif')) %>%
  setNames(c('prob')) %>%
  classify(rcl = rcl_prob_mat) %>%
  as.data.frame(xy = T, na.rm = T) %>%
  mutate(title = 'Climatic Suitability')

## sdm uncertainty ----
mod_uncert <- rast(file.path(dir, 'predictions', 'vedu_prob_1982_2012_std.tif')) %>%
  setNames('uncertainty') %>%
  as.data.frame(xy = T, na.rm = T) %>%
  mutate(title = 'SDM-derived Variability')

## environmental novelty ----
mess <- rast(file.path(dir, 
                       'PRISM/Nietupski/novel_climate',
                       'MESS_MoD_vedu_vs_west_masked.tif')) %>%
  setNames(c('mess'))

# convert mess data to polygons to use as hashed layer in map
mess_poly <- mess %>%
  as.polygons() %>%
  st_as_sf()

# convert mess data to a dataframe
mess <- mess %>%
  as.data.frame(xy = T, na.rm = T) %>%
  mutate(title = 'Climate Novelty')

## spatial importance ----
importance <- list.files(file.path(dir, 'modeling/spatial_importance'),
                         pattern = '.*imp.*.tif$',
                         recursive = F, 
                         full.names = T) %>%
  rast() %>%
  modal(ties='first') %>%
  setNames(c('imp')) %>%
  as.data.frame(xy = T, na.rm = T) %>%
  mutate(title = 'Importance')

###############################################################################
# plots
###############################################################################

#### current suitability and BRT uncertainty ####
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
  geom_sf(data = ecoreg, fill = NA, color = '#d3d3d3', size = 0.15) + #d3d3d3
  geom_sf_pattern(data = mess_poly,
                  fill = NA, color = NA, size = 0.1, alpha = 0.3,
                  pattern = 'crosshatch',
                  pattern_density = 0.04,
                  pattern_fill = NA,
                  pattern_color = 'white',
                  pattern_size = 0.4,
                  pattern_spacing = 0.01,
                  pattern_alpha = 0.3) +
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
        plot.margin = margin(1,1,0,1),
        legend.box.margin = margin(-20,0,0,0)) +
  guides(fill = guide_legend(nrow = 2, byrow=T)) +
  facet_grid(.~title)

p2 <- mod_uncert %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, 
                  fill = uncertainty)) +
  scale_fill_viridis_c(option = 'rocket') +
  scale_y_continuous(expand = c(0.005,0.005)) +
  scale_x_continuous(expand = c(0.005,0.005)) +
  geom_sf(data = ecoreg, fill = NA, color = '#d3d3d3', size = 0.15) +
  geom_sf_pattern(data = mess_poly,
                  fill = NA, color = NA, size = 0.05, alpha = 0.3,
                  pattern = 'crosshatch',
                  pattern_density = 0.04,
                  pattern_fill = NA,
                  pattern_color = 'white',
                  pattern_size = 0.4,
                  pattern_spacing = 0.01,
                  pattern_alpha = 0.3) +
  theme_grey() +
  labs(x = '', y = '', fill = '') +
  theme(panel.background = element_rect(color = 'white', 
                                        fill = 'white'),
        panel.grid.major = element_line(color = 'grey50', 
                                        size = 0.25, 
                                        linetype = 'dashed'), 
        axis.ticks = element_blank(),
        legend.key.width = unit(2.5, 'line'),
        legend.key.heigh = unit(0.75, 'line'),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.title.align = 0.5,
        plot.margin = margin(1, 0, 0, 0),
        legend.box.margin = margin(-20,0,0,0)) +
  guides(fill = guide_colorbar(title.position = 'top')) +
  facet_grid(.~title)

composite_plot <- top_row <- plot_grid(p1, p2, 
                                       rel_widths = c(1, 1),
                                       align = 'hv',
                                       axis = 'tblr',
                                       nrow = 1)  

ggsave(filename = 'Current_Suit_Uncert.tiff',
       path = fig_dir,
       plot = composite_plot,
       scale = 1.2,
       width = 5.6,
       height = 3.7,
       units = 'in',
       dpi = 1200,
       compression = 'lzw',
       bg = 'white')

#### novelty and SHAP importance ####
p3 <- mess %>%
  ggplot() +
  geom_sf(data = states, fill = 'white', color = 'white', size = 0.15) +
  geom_tile(aes(x = x, y = y, 
                fill = factor(mess, levels = c(1,2,3,4,5,6),
                              labels = c('Mean Diurnal Range', 
                                         'Temperature Seasonality',
                                         'Mean Temperature of Warmest Quarter',
                                         'Mean Temperature of Coldest Quarter', 
                                         'Precipitation Seasonality',
                                         'Cool Season Precipitation')),
                color = factor(mess, levels = c(1,2,3,4,5,6),
                              labels = c('Mean Diurnal Range', 
                                         'Temperature Seasonality',
                                         'Mean Temperature of Warmest Quarter',
                                         'Mean Temperature of Coldest Quarter', 
                                         'Precipitation Seasonality',
                                         'Cool Season Precipitation')))) +
  scale_fill_manual(values = c('#e69f00', '#d55e00', '#cc79a7',
                               '#0072b2', '#009e73', '#f0e442')) + 
  scale_color_manual(values = c('#e69f00', '#d55e00', '#cc79a7',
                               '#0072b2', '#009e73', '#f0e442')) + # these colors are supposed to be colorblind friendly according to Masataka Okabe and Kei Ito 
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
        plot.margin = margin(1,1,0,1), 
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.spacing.x = unit(0.3, 'cm')) +
  guides(color = 'none') +
  facet_grid(.~title)

p4 <- importance %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, 
                  fill = factor(imp, levels = c(1,2,3,4,5,6),
                                labels = c('bio2 ', 
                                           'bio4',
                                           'bio10',
                                           'bio11', 
                                           'bio15',
                                           'csppt')))) +
  scale_fill_manual(values = c('#e69f00', '#d55e00', '#cc79a7',
                               '#0072b2', '#009e73', '#f0e442')) + # colorblind friendly according to Masataka Okabe and Kei Ito 
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
        legend.position = 'none',
        plot.margin = margin(1,1,0,1)) +
  facet_grid(.~title)

mess_legend <- get_legend(p3 + theme(legend.box.margin = margin(0, 0, 0, 0)))

top <- plot_grid(p3 + theme(legend.position = 'none'), p4,
                 rel_widths = c(1, 1), 
                 nrow = 1)  
add_legend <- plot_grid(top, mess_legend, 
                        rel_heights = c(1, 0.2),
                        axis = 'tb',
                        align = 'h',
                        nrow = 2)


ggsave(filename = 'Current_MESS_Imp.tiff',
       path = fig_dir,
       plot = add_legend,
       scale = 1.25,
       width = 5.6,
       height = 3.7,
       units = 'in',
       dpi = 1200,
       compression = 'lzw',
       bg = 'white')


