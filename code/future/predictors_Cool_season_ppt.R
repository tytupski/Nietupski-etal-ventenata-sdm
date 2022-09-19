###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: calculate cool season 
#   (winter-spring) ppt
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('tidyverse', 'raster', 'here')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

rm(pkgs)

###############################################################################
# data and globals
###############################################################################

dir <- here('Data', 'future', 'gcm')
# names of gcm directories (drop the Summaries directory)
gcms <- list.dirs(dir, full.names = F, recursive = F)[1:30]

# time periods for each gcm
time_periods <- c('2069_2099', '1982_2012')

###############################################################################
# calc winter-spring ppt
###############################################################################

gcms %>%
  map(function(gcm) {
    time_periods %>%
      map(function(this_time) {
        # get the coldest and qtr after coldest ppt
        fns <- list.files(path = file.path(dir, gcm),
                          pattern = "(biovars_.*.tif$)|(qacq_ppt_.*.tif$)",
                          recursive = F)
        # filter to this time period
        fns <- fns[grepl(paste0('.*', this_time, '.tif'), fns)]
        
        # read in rasters
        coldest_ppt <- brick(file.path(dir, gcm, fns[1])) %>%
          subset(19)
        qac_ppt <- raster(file.path(dir, gcm, fns[2]))
        
        # total winter-spring ppt
        ws_ppt <- coldest_ppt + qac_ppt
        
        writeRaster(ws_ppt,
                    filename = file.path(dir, gcm, 
                                         paste0('cool_season_ppt_', this_time, '.tif')),
                    format = 'GTiff',
                    options = c('COMPRESS=LZW', 'TILED=YES',
                                'BLOCKXSIZE=256', 'BLOCKYSIZE=256'), 
                    overwrite = TRUE)
      })
  })
