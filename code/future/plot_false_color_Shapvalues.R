###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: shapley value false color (rgb)
#   composite maps of the top 3 predictors
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('terra', 'sf', 'here', 'tidyverse', 'cowplot')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

###############################################################################
# data
###############################################################################

# relevant directories
dir <- here('Data')
fig_dir <- here('Figures')

# current shapely metric files
current_shap <- list.files(file.path(dir, 'current', 'modeling', 'spatial_importance'),
                            pattern = 'Shap_v.*.tif$',
                            recursive = F,
                            full.names = T) %>%
  rast()

# future shapely metric files
gcms <- list.dirs(file.path(dir, 'future', 'gcm'), 
                  recursive = F, 
                  full.names = F)[1:30] 
future_shap <- list.dirs(file.path(dir, 'future', 'gcm'), 
                          recursive = F,
                          full.names = T)[1:30] %>%
  map(~list.files(file.path(.x, 'spatial_importance'), 
                  pattern = 'Shap_v.*.tif$',
                  recursive = F, 
                  full.names = T) %>%
        rast()) %>%
  setNames(c('ACCESS1.0', 'BCC-CSM1.1', 'BCC-CSM1.1-m',
                        'CanESM2', 'CCSM4', 'CESM1-BGC', 
                        'CESM1-CAM5', 'CMCC-CM', 'CNRM-CM5', 
                        'CSIRO-Mk3-6-0', 'FGOALS-g2', 'FIO-ESM',
                        'GFDL-CM3', 'GFDL-ESM2G', 'GFDL-ESM2M',
                        'GISS-E2-R', 'HadGEM2-AO', 'HadGEM2-CC',
                        'HadGEM2-ES', 'INMCM4', 'IPSL-CM5A-LR',
                        'IPSL-CM5A-MR', 'IPSL-CM5B-LR', 
                        'MIROC-ESM', 'MIROC-ESM-CHEM', 'MIROC5',
                        'MPI-ESM-LR', 'MPI-ESM-MR', 'MRI-CGCM3',
                        'NorESM1-M'))

# states
states <- st_read(file.path(dir, 'current', 'states_west_clip.shp'))

###############################################################################
# plot each raster 
###############################################################################

# rescale values between 0 and 1
range_01 <- function(x) {
  (x-min(x))/(max(x)-min(x))
}

#### current ####
# top 3 predictor shap means in order 6,4,2 (i.e, csppt, bio4, bio11)
current_means <- c(6,2,4) %>%
  map(function(i) {
    mn <- current_shap %>%
      subset(seq(i,nlyr(current_shap),6)) %>%
      app(fun = mean, na.rm = T)
  }) %>%
  rast() %>%
  setNames(c('r', 'g', 'b')) %>%
  as.data.frame(xy = T, na.rm = T) %>%
  mutate(r = range_01(r),
         g = range_01(g),
         b = range_01(b),
         title = 'Current (PRISM)')

current_means %>%
  ggplot() +
  geom_raster(aes(x=x, y=y, fill=rgb(r,g,b))) +
  scale_fill_identity() +
  scale_y_continuous(expand = c(0.005,0.005)) +
  scale_x_continuous(expand = c(0.005,0.005)) +
  geom_sf(data = states, fill = NA, color = 'white', size = 0.2) +
  labs(x = '', y = '', fill = '') +
  theme_grey() +
  theme(panel.background = element_rect(color = 'white', 
                                        fill = 'white'),
        panel.grid.major = element_line(color = 'grey50', 
                                        size = 0.25, 
                                        linetype = 'dashed'), 
        axis.ticks = element_blank()) +
  facet_grid(.~title)

ggsave(filename = 'Shap_false_color_composite.png',
       path = file.path(fig_dir, 'current/maps'),
       device = 'png',
       scale = 1.25,
       width = 3,
       height = 4,
       units = 'in',
       dpi = 500,
       bg = 'white')

#### future ####
# dont have enough memory to make facet map
future_shap %>%
  imap(function(rast, gcm_name) {
    dat <-c(6,2,4) %>%
      map(function(i) {
        mn <- rast %>%
          subset(seq(i,nlyr(rast),6)) %>%
          app(fun = mean, na.rm = T)
      }) %>%
      rast() %>%
      setNames(c('r', 'g', 'b')) %>%
      as.data.frame(xy = T, na.rm = T) %>%
      mutate(r = range_01(r),
             g = range_01(g),
             b = range_01(b),
             title = gcm_name)
    
    dat %>%
      ggplot() +
      geom_raster(aes(x=x, y=y, fill=rgb(r,g,b))) +
      scale_fill_identity() +
      scale_y_continuous(expand = c(0.005,0.005)) +
      scale_x_continuous(expand = c(0.005,0.005)) +
      geom_sf(data = states, fill = NA, color = 'white', size = 0.2) +
      labs(x = '', y = '', fill = '') +
      theme_grey() +
      theme(panel.background = element_rect(color = 'white', 
                                            fill = 'white'),
            panel.grid.major = element_line(color = 'grey50', 
                                            size = 0.25, 
                                            linetype = 'dashed'), 
            axis.ticks = element_blank(),
            legend.position = 'none', 
            strip.text = element_text(size = 14)) +
      facet_grid(.~title)
    
    ggsave(filename = paste0('Shap_false_color_composite_', gcm_name, '.png'),
           path = file.path(fig_dir, 'future/maps'),
           device = 'png',
           scale = 2.4,
           width = 1.1,
           height = 1.25,
           units = 'in',
           dpi = 1200,
           bg = 'white')
  })
  
  