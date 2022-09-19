###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: future climatic suitability
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
dir <- here('Data', 'future', 'gcm')
fig_dir <- here('Figures', 'future', 'maps')

# gcms of interest
gcms <- list.dirs(dir, full.names = F, recursive = F)[1:30] %>%
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

# state boundaries
states <- st_read(here('Data', 'current', 'states_west_clip.shp'))

# climatic suitabilty plots
## NOTE: These figures are pulled into GIMP to make the composite plot.
gcms %>%
  imap(function(gcm, gcm_name) {
    vedu_prob <- rast(file.path(dir, gcm, 'predictions','vedu_prob_2069_2099_median_classes.tif')) %>%
      setNames(c('prob')) %>%
      as.data.frame(xy = T, na.rm = T) %>%
      mutate(title = gcm_name)
    
    vedu_prob %>%
      ggplot() +
      geom_raster(aes(x = x, y = y, 
                      fill = factor(prob, levels = c(4, 3, 2, 1),
                                    labels = c('unsuitable ', 
                                               'marginally unsuitable',
                                               'marginally suitable',
                                               'suitable')))) +
      scale_fill_viridis_d() +
      scale_y_continuous(expand = c(0.005,0.005)) +
      scale_x_continuous(expand = c(0.005,0.005)) +
      geom_sf(data = states, fill = NA, color = '#d3d3d3', size = 0.2) +
      theme_grey() +
      labs(x = '', y = '', fill = '') +
      theme(panel.background = element_rect(color = 'white', 
                                            fill = 'white'),
            panel.grid.major = element_line(color = 'grey50', 
                                            size = 0.25, 
                                            linetype = 'dashed'), 
            axis.ticks = element_blank(),
            legend.position = 'none', 
            strip.text = element_text(size = 14)) +
      facet_wrap(.~title)
    
    ggsave(filename = paste0('Climatic_suitability_', gcm, '.png'),
           path = fig_dir,
           device = 'png',
           scale = 2.4,
           width = 1.1,
           height = 1.25,
           units = 'in',
           dpi = 1200,
           bg = 'white')
  })

# export legend only to add to composite plot
gcm <- 'access1-0'
vedu_prob <- raster(file.path(dir, gcm, 'predictions','vedu_prob_2069_2099_median_classes.tif')) %>%
  setNames(c('prob')) %>%
  as.data.frame(xy = T, na.rm = T) %>%
  mutate(title = gcm)

p <- vedu_prob %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, 
                  fill = factor(prob, levels = c(4, 3, 2, 1),
                                labels = c('Unsuitable ', 
                                           'Low',
                                           'Moderate',
                                           'High')))) +
  scale_fill_viridis_d() +
  labs(x = '', y = '', fill = '') +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal') 

# get legend
l <- get_legend(p + theme(legend.box.margin = margin(5, 5, 5, 5)))

# export 
ggsave(filename = 'Climatic_suitability_legend.png',
       path = fig_dir,
       plot = ggdraw(l),
       device = 'png',
       scale = 1.5,
       width = 4,
       height = 0.3,
       units = 'in',
       dpi = 1200,
       bg = 'white')
