###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: Prediction of current vedu
# climate suitability
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('raster', 'rgdal', 'tidyverse', 'here', 'gbm', 'dismo', 'furrr')                                    

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

# set up session for furr map
plan(multisession, workers = 12)

###############################################################################
# data
###############################################################################

# main data directory 
dir <- here('Data', 'current')
fig_dir <- here('Figure', 'current')

#### current climate data ####
prism_climate <- brick(
  file.path(dir, 'PRISM', 'Nietupski', 'final_variables_1982_2012.tif')
  ) %>%
  setNames(c('bio2', 'bio4', 'bio10', 'bio11', 'bio15', 'csppt'))

#### models ####
final_mods <- readRDS(file.path(dir, 'modeling', 'final_models.rds'))

###############################################################################
# prediction
###############################################################################

#### Current ####
# predict present day distribution
if(file.exists(file.path(dir, 'predictions', paste0('vedu_prob_1982_2012_1.tif')))){
  # read in the 100 predictions
  current_preds <- list.files(
    file.path(dir, 'predictions'),
    pattern = ".*.tif$",
    full.names = TRUE,
    recursive = FALSE) %>%
    stack()
} else {
  final_mods %>%
    future_imap(function(mod, mod_idx){
      raster::predict(
        object = prism_climate,
        model = mod,
        filename = file.path(dir, 'predictions',
                             paste0('vedu_prob_1982_2012_', mod_idx, '.tif')),
        fun = predict.gbm,
        format = 'GTiff',
        options = c('COMPRESS=LZW', 'TILED=YES',
                    'BLOCKXSIZE=256', 'BLOCKYSIZE=256'),
        progress = 'text',
        type = 'response'
      )
    })
}

# look at mean, median, and std of 100 predictions
functs <- list(mean = mean, median = median, std = sd)
functs %>%
  future_imap(~ current_preds %>%
                calc(fun = .x, na.rm = T) %>%
                writeRaster(
                  filename = file.path(dir, 'predictions',
                                       paste0('vedu_prob_1982_2012_', .y, '.tif')),
                  format = 'GTiff',
                  options = c('COMPRESS=LZW', 'TILED=YES',
                              'BLOCKXSIZE=256', 'BLOCKYSIZE=256'))
  )
