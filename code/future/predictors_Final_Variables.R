###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: Create single raster for final 
#   variables
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('raster', 'rgdal', 'tidyverse', 'here')
vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

###############################################################################
# data
###############################################################################

# main directories
dirs <- list(current_dir = here('Data', 'current', 'PRISM', 'Nietupski'),
             future_dir = here('Data','Future', 'gcm'))

###############################################################################
# subset final variables and create single raster will all final variables
###############################################################################

dirs %>%
  imap(function(dir, dir_name) {
    if(dir_name == 'current_dir') {
      # read in bioclim data, subset and append cool season precip
      clim_vars <- file.path(dir, 'biovars_1982_2012.tif') %>%
        brick() %>%
        dropLayer(c(1,3,5,6,7,8,9,12,13,14,16,17,18,19)) %>%
        addLayer(brick(file.path(dir, 'cool_season_ppt.tif')))
      
      writeRaster(clim_vars, 
                  filename = file.path(dir, 'final_variables_1982_2012.tif'),
                  format = 'GTiff',
                  options = c('COMPRESS=LZW', 'TILED=YES',
                              'BLOCKXSIZE=256', 'BLOCKYSIZE=256'))
        
    } else {
      gcms <- list.dirs(dir, full.names = F, recursive = F)[1:30]
      time_periods <- c('1982_2012', '2069_2099')
      
      time_periods %>%
        map(function(time_period) {
          gcms %>%
            map(function(gcm) {
              clim_vars <- file.path(dir, gcm, 
                                     paste0('biovars_', time_period, '.tif')) %>%
                brick() %>%
                dropLayer(c(1,3,5,6,7,8,9,12,13,14,16,17,18,19)) %>%
                addLayer(brick(file.path(dir, gcm, 
                                         paste0('cool_season_ppt_', 
                                                time_period, '.tif'))))
              
              writeRaster(clim_vars,
                          filename = file.path(dir, gcm, 
                                               paste0('final_variables_',
                                                      time_period, '.tif')),
                          format = 'GTiff',
                          options = c('COMPRESS=LZW', 'TILED=YES',
                                      'BLOCKXSIZE=256', 'BLOCKYSIZE=256'))
              })
        })
      }
    })
