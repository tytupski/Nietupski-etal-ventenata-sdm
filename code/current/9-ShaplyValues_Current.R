###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: local shapely additive explanations
#   to show the variable driving predictions 
#   across the west (spatial importance) and 
#   how this variable impacts the prediction
#   (effect size)
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('terra', 'sf', 'rgdal',
         'treeshap',
         'tidyverse', 'here')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

###############################################################################
# data
###############################################################################

dir <- here('Data')
set.seed(256)
mod_and_df_idxs <- sample.int(100,10)

#### load vedu data used to fit the models ####
# presence, pseudo-absence, and weights
weights_fn <- 'vedu_presence_rdDist_origDist.shp'
pres_dir <- 'thinned-presence'
abs_dir <- 'pseudo-absence'

# load weights and calculate final weight
# round lat and lon for comparisons later
pres_wts <- st_read(file.path(dir, 'current', weights_fn)) %>%
  mutate(wt_init = rd_dist * orig_dist,
         lon = as.character(round(st_coordinates(.)[,1], 9)),
         lat = as.character(round(st_coordinates(.)[,2], 9))) %>%
  select(wt_init, lat, lon) %>%
  st_drop_geometry()

# the thinned presence files
pres_fns <- list.files(file.path(dir, 'current', pres_dir), 
                       pattern = '[[:alnum:]]*_[[:alnum:]]*_5k_[[:alnum:]]*_bioclim.csv',
                       recursive = F)[mod_and_df_idxs] 

# sample 20 % of the absence files
abs_fns <- list.files(file.path(dir, 'current', abs_dir),
                      pattern = '[[:alnum:]]*-[[:alnum:]]*-[[:alnum:]]*_5k-[[:alnum:]]*_bioclim.csv', 
                      recursive = F)[mod_and_df_idxs]

# read in presence data and name each dataframe with the file name
# there are 578 presence points in each dataframe
pres_dfs <- pres_fns %>%
  map(function(fn) {
    read.csv(file.path(dir, 'current', pres_dir, fn)) %>%
      mutate(lat = as.character(round(lat, 9)),
             lon = as.character(round(lon, 9)),
             presence = 1) %>%
      left_join(pres_wts, by=c('lat', 'lon'), keep = F) %>% # add the weights for each location
      mutate(wt = (wt_init/sum(wt_init))/2) %>% # relativize weights and make sure sum is equal to absence
      select(presence, wt, bio2, bio4, bio10, bio11, bio15, csppt)
  }) %>%
  setNames(pres_fns)

# read in absence data and name each dataframe with the file name
# there are 10,000 pseudo-absence points in each dataframe
abs_dfs <- abs_fns %>%
  map(function(fn) {
    read.csv(file.path(dir, 'current', abs_dir, fn)) %>%
      mutate(presence = 0, 
             wt = (1/10000)/2) %>% # sum of weights equal to presence
      select(presence, wt, bio2, bio4, bio10, bio11, bio15, csppt)
  }) %>%
  setNames(abs_fns)

# combine presence and absence dataframes
dfs <- c(1:10) %>%
  map(function(i) {
    pres_dfs[[i]] %>%
      rbind(abs_dfs[[i]])
  })

#### load models object and 'unify' models ####
mods <- readRDS(file.path(dir,
                          'current',
                          'modeling',
                          'final_models.rds'))[mod_and_df_idxs] %>%
  imap(~ gbm.unify(.x, dfs[[.y]]))

#### load current climate ####
current_clim <- rast(file.path(dir, 'current', 'PRISM', 'Nietupski',
                               'final_variables_1982_2012.tif')) %>%
  setNames(c('bio2', 'bio4', 'bio10', 'bio11', 'bio15', 'csppt'))

rm(pres_dfs, abs_dfs, pres_fns, abs_fns, dfs,
   pres_wts, abs_dir, pres_dir, weights_fn, 
   mod_and_df_idxs)
###############################################################################
# calculate shaps and shap based importance for current climate
###############################################################################

#### current climate raster terra approach ####

# wrapper function around 'treeshap' so that the first argument supplied to the
# function is the climate data (a requirement of the app function in terra)
treeshap_wrap <- function(mat.x, mod.x) {
  if (all(is.na(mat.x))) {
    return(as.matrix(t(rep(NA,6))))
  } else {
    ts <- treeshap::treeshap(unified_model = mod.x, 
                             x = `colnames<-`(as.matrix(t(mat.x)),
                                              c('bio2', 'bio4', 'bio10',
                                                'bio11', 'bio15', 'csppt')),
                             interactions = F, 
                             verbose = F)
    
    return(as.matrix(ts$shaps))
    }
}

# loop through the sample of 10 models, calculate shap values, and save to disk
for (i in 1:10) {
  mod <- mods[[i]]
  app(current_clim, 
      fun = function(x, ff, m.x) ff(x, m.x),
      m.x=mod,
      ff = treeshap_wrap,
      cores = 30, 
      filename = file.path(dir, 
                           'current/modeling/spatial_importance',
                           paste0('Shap_values_',  i,
                                  '_1982_2012.tif')),
      wopt = list(filetype = 'GTiff',
                  gdal = c('COMPRESS=LZW', 'TILED=YES',
                           'BLOCKXSIZE=256', 'BLOCKYSIZE=256')),
      overwrite = T)
  
}