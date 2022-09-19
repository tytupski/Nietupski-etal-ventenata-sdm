###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: spatial thinning, pseudo-absence
#   generation
###############################################

# This script loads the full vedu dataset of 2148 observations.
# Presence data was then spatially thinned using the spThin package at 
# 5k buffer distance. Finally, pseudo-absences were randomly generated with  
# the convex hull around the presence points, excluding the locations of 
# presence points. 

###############################################################################
# libraries
###############################################################################

pkgs = c('spThin', 'tidyverse', 'here',
         'raster', 'sf')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)
###############################################################################
# data
###############################################################################

# directory of all the relevant data
dir <- here('Data','current')

# vedu presence data
presence <- st_read(file.path(dir, 'vedu_presence.shp'),
                    quiet = TRUE) %>%
  mutate(across(everything(), ~na_if(., -999)))

# convert spatial object to dataframe with lat lon as columns
pres_df <- presence %>%
  mutate(lat = st_coordinates(presence)[,2],
         lon = st_coordinates(presence)[,1]) %>%
  st_drop_geometry()

# open the buffered mask around presence points to use in generating 
# pseudo-absence
mask_5k = raster(file.path(dir,
                           'pseudo-absence',
                           'pseudo-absence-region-mask-5k.tif'))

###############################################################################
# spatially thin presence data 
###############################################################################

# run spatial thinning on presence data with 5 km min distance between points
pres_thinned_5k <- pres_df %>%
  thin(lat.col = 'lat', long.col = 'lon', spec.col = 'Species',
       thin.par = 5, reps = 10000,
       locs.thinned.list.return = FALSE,
       write.files = TRUE, max.files = 100,
       out.dir = dir, out.base = 'thinned-presence/vedu_pres_5k',
       write.log.file = TRUE, log.file = 'spatial_thin_log.txt')

###############################################################################
# generate pseudo-absence data within convex hull around presence points
###############################################################################

# for each mask raster, create and save 100 pseudo-absence data sets
for(i in 1:100){
  # generate random points within the mask
  pseudo_abs <- randomPoints(mask_5k,
                             n = 10000,
                             lonlatCorrection = TRUE) %>%
        as.data.frame()
  
  write.csv(pseudo_abs,
            file.path(dir,
                      'pseudo-absence',
                      'pseudo-absence-mask_5k-',
                       as.character(i),
                       '.csv'))
  }

