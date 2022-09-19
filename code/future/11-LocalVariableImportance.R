###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective:  local variable importance from 
#   shap values
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

dir <- here('Data')

current <- file.path(dir, 'current', 'modeling', 'spatial_importance')
future <- list.dirs(file.path(dir, 'future', 'gcm'),
                    full.names = T,
                    recursive = F)[1:30]

###############################################################################
# processing
###############################################################################

# importance for current climate
c(1:10) %>%
  map(function(i) {
    rast <- brick(file.path(current, 
                            paste0('Shap_values_', 
                                   i,
                                   '_1982_2012.tif')))
    writeRaster(which.max(abs(rast)),
                filename = file.path(current,
                                     paste0('Shap_importance_', i,
                                            '_1982_2012.tif')),
                format = 'GTiff',
                options = c('COMPRESS=LZW', 'TILED=YES',
                            'BLOCKXSIZE=256', 'BLOCKYSIZE=256'),
                overwrite = TRUE)
    
  })

# importance for future climate
future %>%
  map(function(gcm_dir) {
    c(1:10) %>%
      map(function(i) {
        if (file.exists(file.path(gcm_dir, 'spatial_importance',
                                  paste0('Shap_importance_', i,
                                         '_2069_2099.tif')))) {
          return(NULL)
        }
        
        rast <- brick(file.path(gcm_dir, 'spatial_importance', 
                                paste0('Shap_values_', 
                                       i,
                                       '_2069_2099.tif')))
        writeRaster(which.max(abs(rast)),
                    filename = file.path(gcm_dir, 'spatial_importance',
                                         paste0('Shap_importance_', i,
                                                '_2069_2099.tif')),
                    format = 'GTiff',
                    options = c('COMPRESS=LZW', 'TILED=YES',
                                'BLOCKXSIZE=256', 'BLOCKYSIZE=256'),
                    overwrite = TRUE)
      })
  })
