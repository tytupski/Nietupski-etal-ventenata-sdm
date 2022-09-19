###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: suitability prediction for 1 gcm 
#   at a time. this script is called within a
#   python script named Predict_all_GCMS.py so
#   that we can run each gcm from the terminal
#   so that the ram clears between each gcm
#   terminal command is as follows:
#   Rscript 7-Prediction_GCM.R <gcm> <time>
#   example format for gcm is the lowercase name
#   of the gcm (e.g., "access1-0") and an example
#   format for time is "2069_2099" or "1982_2012"
###############################################

###############################################################################
# libraries
###############################################################################

suppressMessages(library(tidyverse))
suppressMessages(library(raster))
suppressMessages(library(gbm))
suppressMessages(library(furrr))
suppressMessages(library(here))

# set up session for furr map
plan(multisession, workers = 15)

###############################################################################
# globals
###############################################################################

dir <- here('Data', 'future', 'gcm')
pres_dir <- here('Data', 'current')
gcm <- toString(commandArgs(trailingOnly = TRUE)[1])
time <- toString(commandArgs(trailingOnly = TRUE)[2])

###############################################################################
# prediction and prediction summary
###############################################################################

# generate future predictions
if(file.exists(file.path(dir, 
                         gcm, 
                         'predictions', 
                         paste0('vedu_prob_',time,'_1.tif')))) {
  patt <- paste0('vedu_prob_', time, '_[[:alnum:]]*.tif$')
  # read in the 100 predictions
  fut_preds <- list.files(
    file.path(dir, gcm, 'predictions'),
    pattern = patt,
    full.names = TRUE,
    recursive = FALSE) %>%
    stack()
  
  # mean, median, and std of predictions
  functs <- list(mean = mean, median = median, std = sd)
  
  # calc mean, med, std for the prediction files
  functs %>%
    future_imap(function(funk, funk_name){
      fut_preds %>%
        calc(fun = funk, na.rm = T) %>%
        writeRaster(filename = file.path(dir, gcm,
                                         'predictions',
                                         paste0('vedu_prob_', time,
                                                '_', funk_name, '.tif')),
                    format = 'GTiff',
                    options = c('COMPRESS=LZW', 'TILED=YES',
                                'BLOCKXSIZE=256', 'BLOCKYSIZE=256'),
                    overwrite = TRUE)
    }) 
} else {
  # 100 models
  final_mods <- readRDS(file.path(pres_dir, 'modeling', 'final_models.rds'))
  
  fut_biovars <- brick(file.path(dir, gcm, paste0('final_variables_', time, '.tif'))) %>%
    setNames(c('bio2', 'bio4', 'bio10', 'bio11', 'bio15', 'csppt'))
  
  final_mods %>%
    future_imap(~ raster::predict(
      object = fut_biovars,
      model = .x,
      filename = file.path(dir, gcm,
                           'predictions',
                           paste0('vedu_prob_', time, '_', .y, '.tif')),
      fun = predict.gbm,
      format = 'GTiff',
      options = c('COMPRESS=LZW', 'TILED=YES',
                  'BLOCKXSIZE=256', 'BLOCKYSIZE=256'),
      progress = 'text',
      type = 'response'
      )
    )
}