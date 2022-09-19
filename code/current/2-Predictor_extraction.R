###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: extract bioclim vars to presence
#   and absence
###############################################

# load bioclim data, presence, thinned presence, and pseudo-absence. 
# extract the pixel values associated with each plot to the data frame and then 
# export as a shapefile (presence) or csv (thinned presence, pseudo-absence)

###############################################################################
# libraries
###############################################################################

pkgs = c('raster', 'sf', 'rgdal','tidyverse', 'here')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

###############################################################################
# Data
###############################################################################

# parent directory with project data
dir <- here('Data', 'current')

#### PRISM climate ####
## bioclimatic variables
# read in bioclim variables
bioclim_vars <- brick(file.path(dir, 
                                'PRISM',
                                'Nietupski',
                                'biovars_1982_2012.tif')) 
names(bioclim_vars) <- read.csv(file.path(dir, 
                                          'PRISM', 
                                          'Nietupski', 
                                          'biovar_names.csv'))$x

## New Variables
new_vars <- list.files(
  file.path(dir, 'PRISM','Nietupski'),
  pattern = '(sosq_ppt.tif$)|(sosq_tavg.tif$)|(qacq_ppt.tif$)|(qacq_tavg.tif$)|(cool.*.tif$)',
  full.names = T) %>%
  stack()
names(new_vars) <- c('csppt', 'qacqppt', 'qacqtavg', 'sosqppt','sosqtavg')

# combined
all_vars <- bioclim_vars %>% addLayer(new_vars) %>% brick()

#### Original presence ####
orig_pres <- st_read(file.path(dir, 'vedu_presence.shp')) %>%
  select(!names(all_vars))

#### Thinned Presence and Pseudo-absence ####
# get file names for presence and pseudo-absence csv's 
# excluding any files with different file extensions
pres_fns <- list.files(file.path(dir, 'thinned-presence'), 
                       pattern = '^vedu_pres_5k')
pres_fns <- pres_fns %>%
  setNames(pres_fns %>% unlist %>% gsub('.csv', '', .))

abs_fns <- list.files(file.path(dir, 'pseudo-absence'), 
                      pattern = '^pseudo-absence-mask_5k') 
abs_fns <- abs_fns %>%
  setNames(abs_fns %>% unlist %>% gsub('.csv', '', .))

# if rerunning this script we might have new csv files in the directory
# remove these files from the list
pres_fns <- pres_fns[str_detect(pres_fns, 'bioclim', negate = TRUE)]
abs_fns <- abs_fns[str_detect(abs_fns, 'bioclim', negate = TRUE)]

# read in pres and abs files
pres <- pres_fns %>%
  map(~ read.csv(file.path(dir, 'thinned-presence', .x))) %>%
  map(function(x){x %>% select(!Species)})
abs <- abs_fns %>%
  map(~ read.csv(file.path(dir, 'pseudo-absence', .x))) %>%
  map(function(x){x %>% select(!X) %>% rename(lon = x, lat = y)})

rm(bioclim_vars, new_vars, pres_fns, abs_fns)
###############################################################################
# Extract Climate
###############################################################################

# extract PRISM current climate to entire presence data set
orig_pres_bioclim <- raster::extract(all_vars, orig_pres,
                                     method = 'simple',
                                     layer = 1, 
                                     nl = 24, 
                                     sp = TRUE)
st_write(st_as_sf(orig_pres_bioclim), 
         file.path(dir, 'vedu_presence.shp'),
         append = F,
         overwrite = T)

# extract PRISM current climate to thinned presence and pseudo-absence
pres %>%
  imap(function(x, y) {
    extracted <- x %>%
      cbind(raster::extract(all_vars, x,
                            method = 'simple',
                            layer = 1,
                            nl = 24))
    write.csv(extracted, 
              file.path(dir,'thinned-presence', paste0(y, '_bioclim.csv')),
              row.names = FALSE)
  })

abs %>%
  imap(function(x, y) {
    extracted <- x %>%
      cbind(raster::extract(all_vars, x,
                            method = 'simple',
                            layer = 1,
                            nl = 24))
    write.csv(extracted, 
              file.path(dir,'pseudo-absence', paste0(y, '_bioclim.csv')),
              row.names = FALSE)
  })

