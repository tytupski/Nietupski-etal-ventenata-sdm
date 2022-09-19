###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: new climate variables for the gcms
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('tidyverse', 'raster', 'furrr', 'here')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

rm(pkgs)

###############################################################################
# data and globals
###############################################################################

dir <- here('Data', 'future', 'gcm')
# names of gcm directories (drop the Summaries directory)
gcms <- list.dirs(dir, full.names = F, recursive = F)[1:30]

# files this script creates
fns <- c('coldest_q_1982_2012.tif',
         'coldest_q_2069_2099.tif',
         'qacq_ppt_1982_2012.tif',
         'qacq_ppt_2069_2099.tif',
         'qacq_tavg_1982_2012.tif',
         'qacq_tavg_2069_2099.tif',
         'sosq_ppt_1982_2012.tif',
         'sosq_ppt_2069_2099.tif',
         'sosq_tavg_1982_2012.tif',
         'sosq_tavg_2069_2099.tif')

# drop any gcms that have all the files generated already
has_files <- gcms %>%
  map_lgl(function(gcm){
    all(file.exists(map_chr(fns, function(file){file.path(dir, gcm, file)})))
    })
gcms <- gcms[!has_files]

# time periods for each gcm
time_periods <- c('2069_2099', '1982_2012')

# indices for each quarter
q_idxs <- list(q1 = c(1, 2, 3),
               q2 = c(2, 3, 4),
               q3 = c(3, 4, 5),
               q4 = c(4, 5, 6),
               q5 = c(5, 6, 7),
               q6 = c(6, 7, 8),
               q7 = c(7, 8, 9),
               q8 = c(8, 9, 10),
               q9 = c(9, 10, 11),
               q10 = c(10, 11, 12),
               q11 = c(11, 12, 1),
               q12 = c(12, 1, 2))

rm(has_files, fns)
###############################################################################
# functions
###############################################################################

# 5.0 deg C is commonly used in the literature to define growing season
# usually this is determined as the first 5 day period above 5C but we 
# don't have daily data so we're going with the first month that exceeds 5c
season_start <- function(tavg) {
  tavg[tavg <= 5.0] <- 0
  tavg[tavg > 5.0] <- 1
  r <- rle(c(tavg))
  first4 <- r$lengths >= 4 & r$values == 1
  n <- NA
  if(any(first4)) {
    idx <- min(which(first4))
    n <- sum(r$lengths[1:(idx)]) - r$lengths[idx]+1
  }
  return(n)
}

# find the index (start month) for the 'est' quarter (wettest, driest...etc)
est <- function(q_idxs, clim_var, metric) {
  # first calculate the sum of the climate variable for each quarter
  q_climvar <- q_idxs %>%
    map(function(q) {
      clim_var %>%
        subset(q) %>%
        sum()           # all of the *est months are determined from the sum
    }) %>% 
    brick()
  
  # then determine the index of the 'est' quarter 
  if(metric %in% c('warmest', 'wettest')) {
    # calc the max index for wettest/warmest
    the_est <- q_climvar %>% which.max()
  } else{
    # calc the min index for driest/coldest
    the_est <- q_climvar %>% which.min()
  }
  
  # return the est raster
  return(the_est) 
}

# find the quarterly climate associated with the 'est' quarter
est_clim <- function(q_idxs, the_est, est_name, clim_var, clim_var_name, gcm, t) {
  if(clim_var_name != 'ppt'){
    q_climvar <- q_idxs %>%
      map(function(q) {
        clim_var %>%
          subset(q) %>%
          mean()           
      }) %>% 
      brick()
    
    q_clim_at_est <- q_climvar %>%
      stackSelect(the_est,
                  filename = file.path(dir, gcm,
                                       paste0(est_name, 'q_', 
                                              clim_var_name, '_', t, '.tif')),
                  options = c('COMPRESS=LZW', 'TILED=YES',
                              'BLOCKXSIZE=256', 'BLOCKYSIZE=256'),
                  overwrite = T)
  } else {
    # first calculate the sum of the climate variable for each quarter
    q_climvar <- q_idxs %>%
      map(function(q) {
        clim_var %>%
          subset(q) %>%
          sum()          
      }) %>% 
      brick()
    
    q_clim_at_est <- q_climvar %>%
      stackSelect(the_est,
                  filename = file.path(dir, gcm,
                                       paste0(est_name, 'q_', 
                                              clim_var_name, '_', t, '.tif')),
                  options = c('COMPRESS=LZW', 'TILED=YES',
                              'BLOCKXSIZE=256', 'BLOCKYSIZE=256'),
                  overwrite = T)
  }
  return(q_clim_at_est)
}

###############################################################################
# create climate variables
###############################################################################

# create new climate variables for each gcm
gcms %>%
  # iterate over gcm directories
  map(function(gcm) {
    # iterate over time periods
    time_periods %>%
      map(function(t) {
        # open the norm data
        ppt <- brick(file.path(dir, gcm, 
                               paste0('ppt_norm_', t, '.tif')))
        tmax <- brick(file.path(dir, gcm, 
                                paste0('tmax_norm_', t, '.tif')))
        tmin <- brick(file.path(dir, gcm, 
                                paste0('tmin_norm_', t, '.tif')))
        tavg <- (tmax + tmin)/2
        names(tavg) <- names(tmax)
        
        # list of climate variables to mask with the est layer
        clim_vars <- list(ppt = ppt, 
                          tavg = tavg)
        
        # determine coldest quarter
        coldest_q <- est(q_idxs, tavg, 'coldest')
        writeRaster(coldest_q, 
                    filename = file.path(dir, gcm, 
                                         paste0('coldest_q_', t, '.tif')),
                    format = 'GTiff',
                    options = c('COMPRESS=LZW', 'TILED=YES',
                                'BLOCKXSIZE=256', 'BLOCKYSIZE=256'), 
                    overwrite = TRUE)
        # quarter after the coldes quarter
        q_after_coldest <- coldest_q + 3
        # determine climate for this quarter and write to disk
        q_after_coldest_climate <- clim_vars %>%
          imap(function(clim_var, clim_var_name){
            est_clim(q_idxs,
                     q_after_coldest, 'qac',
                     clim_var, clim_var_name,
                     gcm, t)
          })

        # determine the start of season based on 5c threshold
        start_of_season <- tavg %>%
          calc(season_start)
        writeRaster(start_of_season,
                    filename = file.path(dir, gcm,
                                         paste0('sos_', t, '.tif')),
                    format = 'GTiff',
                    options = c('COMPRESS=LZW', 'TILED=YES',
                                'BLOCKXSIZE=256', 'BLOCKYSIZE=256'),
                    overwrite = TRUE)
        # climate at the start of season
        start_of_season_climate <- clim_vars %>%
          imap(function(clim_var, clim_var_name){
            est_clim(q_idxs,
                     start_of_season, 'sos',
                     clim_var, clim_var_name,
                     gcm, t)
          })
      })
  })
