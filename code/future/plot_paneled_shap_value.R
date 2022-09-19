###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: plot of shap values (effect size)
# under current and 4 future scenarios (GCMs)
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('terra', 'sf', 'tidyverse', 'cowplot', 'here')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

###############################################################################
# data
###############################################################################

# relevant directories
dir <- here('Data')
fig_dir <- here('Figures', 'future','maps')

#### shapefiles for plotting ####
# states
states <- st_read(here('Data', 'current', 'states_west_clip.shp'))

# ecoregion boundaries
ecoreg <- st_read(file.path(dir, 'current', 'us_eco_l3_west.shp'))

#### read in current and future shap dataframe ####
if(file.exists(file.path(dir, 'future', 'gcm', 'Summaries', 'all_shaps.rds'))) {
  all_shaps <- readRDS(file.path(dir, 'future', 'gcm', 'Summaries', 'all_shaps.rds'))
} else {
  ## future shap values ----
  if(file.exists(file.path(dir, 'future', 'gcm', 'Summaries', 'gcm_shaps.rds'))) {
    gcm_shaps <- readRDS(file.path(dir, 'future', 'gcm', 'Summaries', 'gcm_shaps.rds'))
  }else {
    # current shapely values for each predictor separately
    gcm_shaps <- c('MPI-ESM-MR', 'MRI-CGCM3', 'FIO-ESM', 'CNRM-CM5') %>%
      map(function(gcm_name) {
        # convert to lower case for filenames
        gcm <- tolower(gcm_name)
        
        # file names of shap values
        fns <- list.files(file.path(dir, 'future', 'gcm', gcm, 'spatial_importance'),
                          pattern = 'Shap_v.*.tif$',
                          recursive = F,
                          full.names = T)
        
        # calculate mean shap value for each of the predictors
        cool_season_shap <-  fns %>%
          rast(lyrs = seq(6, 60, 6)) %>%
          app(mean)
        temp_seas_shap <- fns %>%
          rast(lyrs = seq(2, 60, 6)) %>%
          app(mean)
        coldest_temp_shap <- fns %>%
          rast(lyrs = seq(4, 60, 6)) %>%
          app(mean)
        warmest_temp_shap <- fns %>%
          rast(lyrs = seq(3, 60, 6)) %>%
          app(mean)
        precip_seas_shap <- fns %>%
          rast(lyrs = seq(5, 60, 6)) %>%
          app(mean)
        diurnal_range_shap <- fns %>%
          rast(lyrs = seq(1, 60, 6)) %>%
          app(mean)
        
        # combine into single raster, reduce resolution, and convert to dataframe
        rast(list(cool_season_shap, temp_seas_shap, coldest_temp_shap, 
                  warmest_temp_shap, precip_seas_shap, diurnal_range_shap)) %>%
          setNames(c('Cool Season Precipitation', 
                     'Temperature Seasonality',
                     'Mean Temperature of Coldest Quarter',
                     'Mean Temperature of Warmest Quarter',
                     'Precipitation Seasonality',
                     'Mean Diurnal Range')) %>%
          aggregate(fact = 2) %>%
          as.data.frame(xy = T, na.rm = T) %>%
          pivot_longer(cols = 3:8, names_to = 'var', values_to = 'value') %>%
          mutate(climate_dat = gcm_name)
      }) %>%
      bind_rows()
    
    saveRDS(gcm_shaps, file.path(dir, 'future', 'gcm',
                                 'Summaries', 'gcm_shaps.rds'), compress=T)
  }
  
  ## current shap values ----
  # current shapely values for each predictor separately
  if(file.exists(file.path(dir, 'current', 
                           'modeling', 'spatial_importance',
                           'current_shaps.rds'))) {
    current_shaps <- readRDS(file.path(dir, 'current', 
                                       'modeling', 'spatial_importance',
                                       'current_shaps.rds'))
  } else {
    # file names of shap values
    current_shap_val_fns <-  list.files(file.path(dir, 'current', 
                                                  'modeling', 'spatial_importance'),
                                        pattern = 'Shap_v.*.tif$',
                                        recursive = F,
                                        full.names = T)
    
    # calculate mean shap value for each of the predictors
    cool_season_shap <- current_shap_val_fns %>%
      rast(lyrs = seq(6, 60, 6)) %>%
      app(mean)
    temp_seas_shap <- current_shap_val_fns %>%
      rast(lyrs = seq(2, 60, 6)) %>%
      app(mean)
    coldest_temp_shap <- current_shap_val_fns %>%
      rast(lyrs = seq(4, 60, 6)) %>%
      app(mean)
    warmest_temp_shap <- current_shap_val_fns %>%
      rast(lyrs = seq(3, 60, 6)) %>%
      app(mean)
    precip_seas_shap <- current_shap_val_fns %>%
      rast(lyrs = seq(5, 60, 6)) %>%
      app(mean)
    diurnal_range_shap <- current_shap_val_fns %>%
      rast(lyrs = seq(1, 60, 6)) %>%
      app(mean)
    
    # combine into shap values into single raster, rename bands, convert to df
    current_shaps <- rast(list(cool_season_shap, temp_seas_shap, coldest_temp_shap, 
                               warmest_temp_shap, precip_seas_shap, diurnal_range_shap)) %>%
      setNames(c('Cool Season Precipitation', 
                 'Temperature Seasonality',
                 'Mean Temperature of Coldest Quarter',
                 'Mean Temperature of Warmest Quarter',
                 'Precipitation Seasonality',
                 'Mean Diurnal Range')) %>%
      aggregate(fact = 2) %>% # reduce resolution by half
      as.data.frame(xy = T, na.rm = T) %>%
      pivot_longer(cols = 3:8, names_to = 'var', values_to = 'value') %>%
      mutate(climate_dat = 'Current (PRISM)')
    
    saveRDS(current_shaps, file.path(dir, 'current', 
                                     'modeling', 'spatial_importance',
                                     'current_shaps.rds'), compress=T) 
  }
  
  ## combine and save shaps from current and future  ----
  all_shaps <- gcm_shaps %>% 
    rbind(current_shaps) %>%
    mutate(climate_dat = factor(climate_dat,
                                levels = c('Current (PRISM)',
                                           'FIO-ESM', 'CNRM-CM5', 
                                           'MRI-CGCM3', 'MPI-ESM-MR'),
                                labels = c('Current (PRISM)',
                                           'FIO-ESM', 'CNRM-CM5', 
                                           'MRI-CGCM3', 'MPI-ESM-MR')),
           var = factor(var, 
                        levels = c('Cool Season Precipitation', 
                                   'Temperature Seasonality',
                                   'Mean Temperature of Coldest Quarter',
                                   'Mean Temperature of Warmest Quarter',
                                   'Precipitation Seasonality',
                                   'Mean Diurnal Range'),
                        labels = c('Cool Season Precipitation', 
                                   'Temperature Seasonality',
                                   'Mean Temperature of\n Coldest Quarter',
                                   'Mean Temperature of\n Warmest Quarter',
                                   'Precipitation Seasonality',
                                   'Mean Diurnal Range')))
  
  saveRDS(all_shaps, file.path(dir, 'future', 'gcm', 'Summaries',
                               'all_shaps.rds'),
          compress = T) 
}
###############################################################################
# plot shap values for each raster with label
###############################################################################

# facet grid of the shap value for each predictor and climate dataset across
# the western US
all_shaps %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, 
                  fill = value)) +
  scale_fill_gradient2(low = '#d93629',
                       mid = '#fffebe',
                       high = '#4978b6',
                       midpoint = 0,
                       guide = 'colourbar',
                       aesthetics = 'fill') +
  scale_y_continuous(expand = c(0.005,0.005)) +
  scale_x_continuous(expand = c(0.005,0.005)) +
  geom_sf(data = ecoreg, fill = NA, color = 'grey25', size = 0.15) +
  theme_grey() +
  labs(x = '', y = '', fill = 'Effect size (SHAP value)') +
  theme(panel.background = element_rect(color = 'white', 
                                        fill = 'white'),
        panel.grid.major = element_line(color = 'grey50', 
                                        size = 0.25, 
                                        linetype = 'dashed'), 
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(50, 'points'),
        legend.key.heigh = unit(10, 'points'),
        plot.margin = margin(1, 0, 0, 0),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(-12,-10,1,-10)) +
  guides(fill = guide_colorbar(title.position = 'top',
                               title.hjust = 0.5)) +
  facet_grid(var ~ climate_dat)

ggsave(filename = 'paneled_shap_values_current_future.tiff',
       path = fig_dir,
       scale = 1.9,
       width = 5,
       height = 7,
       units = 'in',
       dpi = 1200,
       compression = 'lzw',
       bg = 'white')
