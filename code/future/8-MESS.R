###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: Multivariate Environmental 
#   Suitability Surfaces (MESS) plots for 
#   extrapolation current and future climate
#   space.
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('raster', 'sf','rgdal', 'dismo', 'tidyverse', 'here')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

###############################################################################
# data
###############################################################################

dir <- here('Data')
fig_dir <- here('Figures')

#### vedu observed climate ####

# We need to know the characteristics of the space we have observations for in 
# order to know whether climates outside the vedu range are similar or different. 
# To do this, we combine the presence data with all of the absence data used in
# the modeling n_presence = 2083 and n_absence = 10,000 (points) * 100 (replicates). 
ref_df <- st_read(file.path(dir, 'current', 'vedu_presence.shp')) %>%
  st_drop_geometry() %>%
  select(bio2, bio4, bio10, bio11, bio15, csppt) %>%
  bind_rows(list.files(file.path(dir, 'current', 'pseudo-absence'),
                       pattern = '.*_5k-[[:alnum:]]*_bioclim.csv',
                       recursive = F, full.names = T) %>%
              map(function(fn) {
                read.csv(fn) %>%
                  select(bio2, bio4, bio10, bio11, bio15, csppt)
              }) %>%
              bind_rows()) %>%
  distinct() # drop any duplicates

#### current climate data for the western US ####
current_clim <- brick(file.path(dir, 'current', 
                                'PRISM', 'Nietupski', 
                                'final_variables_1982_2012.tif')) %>%
  setNames(c('bio2', 'bio4', 'bio10', 'bio11', 'bio15', 'csppt'))

#### future climate data for the western US ####
gcms <- list.dirs(file.path(dir, 'future','gcm'),
                  full.names = F,
                  recursive = F)[1:30]
future_clim <- gcms %>%
  map(~ file.path(dir, 'future', 'gcm', 
                  .x, 'final_variables_2069_2099.tif') %>%
        brick() %>%
        setNames(c('bio2', 'bio4', 'bio10', 'bio11', 'bio15', 'csppt'))) %>%
  setNames(gcms)

###############################################################################
# calc similarity surfaces and determine variable contributing to dissimilarity
###############################################################################

# calculate mess with function from the dismo package
current_mess <- current_clim %>% mess(ref_df, full = T)
# write out to file
writeRaster(current_mess[[7]],
            filename = file.path(dir, 
                                 'current',
                                 'PRISM',
                                 'Nietupski',
                                 'novel_climate',
                                 'MESS_vedu_vs_west.tif'),
            format = 'GTiff',
            options = c('COMPRESS=LZW', 'TILED=YES',
                        'BLOCKXSIZE=256', 'BLOCKYSIZE=256'),
            overwrite = TRUE)

# export all mess values
writeRaster(current_mess[[1:6]],
            filename = file.path(dir, 
                                 'current',
                                 'PRISM',
                                 'Nietupski',
                                 'novel_climate',
                                 'MESS_values_vedu_vs_west.tif'),
            format = 'GTiff',
            options = c('COMPRESS=LZW', 'TILED=YES',
                        'BLOCKXSIZE=256', 'BLOCKYSIZE=256'),
            overwrite = TRUE)

# determine which variable was most dissimilar to the reference set
most_dissim <- which.min(current_mess[[1:6]])
writeRaster(most_dissim,
            filename = file.path(dir, 
                                 'current',
                                 'PRISM',
                                 'Nietupski',
                                 'novel_climate',
                                 'MESS_MoD_vedu_vs_west.tif'),
            format = 'GTiff',
            options = c('COMPRESS=LZW', 'TILED=YES',
                        'BLOCKXSIZE=256', 'BLOCKYSIZE=256'),
            overwrite = TRUE)

# calculate mess for future climate
future_mess <- future_clim %>%
  imap(function(rast, gcm_name) {
    
    # calculate mess
    fut_mess <- rast %>% mess(ref_df, full = T)
    # writeRaster(fut_mess[[7]],
    #             filename = file.path(dir, 'future', 'gcm',
    #                                  gcm_name, 'novel_climate',
    #                                  'MESS_vedu_vs_future.tif'),
    #             format = 'GTiff',
    #             options = c('COMPRESS=LZW', 'TILED=YES',
    #                         'BLOCKXSIZE=256', 'BLOCKYSIZE=256'),
    #             overwrite = TRUE)
    
    writeRaster(fut_mess[[1:6]],
                filename = file.path(dir, 'future', 'gcm',
                                     gcm_name, 'novel_climate',
                                     'MESS_values_vedu_vs_future.tif'),
                format = 'GTiff',
                options = c('COMPRESS=LZW', 'TILED=YES',
                            'BLOCKXSIZE=256', 'BLOCKYSIZE=256'),
                overwrite = TRUE)
    
    # variable leading to greatest dissimilarity
    # most_dissim <- which.min(fut_mess[[1:6]])
    # writeRaster(most_dissim,
    #             filename = file.path(dir, 'future', 'gcm',
    #                                  gcm_name, 'novel_climate',
    #                                  'MESS_MoD_vedu_vs_future.tif'),
    #             format = 'GTiff',
    #             options = c('COMPRESS=LZW', 'TILED=YES',
    #                         'BLOCKXSIZE=256', 'BLOCKYSIZE=256'),
    #             overwrite = TRUE)
  })
