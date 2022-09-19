###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: create biovars from the 
#   2069-2099 norm data
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('dismo', 'tidyverse', 'raster', 'furrr', 'here')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

# set up session for furr map
plan(multisession, workers = 15)

###############################################################################
# data
###############################################################################

# directory that houses all of the climate norm data in gcm labeled subdirectories
dir <- here('Data', 'future', 'gcm')

# read norm data and house in nested list where the first list level is the gcm
# and the second level is the climate variable
files <- list.dirs(dir, recursive = FALSE) %>%
  map(function(x) {
    list.files(x, pattern = "12.tif$",full.names = TRUE, recursive = FALSE) # change for historical
    }) %>%
  setNames(c("access1-0",'bcc-csm1-1','bcc-csm1-1-m','canesm2','ccsm4',
             'cesm1-bgc','cesm1-cam5','cmcc-cm','cnrm-cm5','csiro-mk3-6-0',
             'fgoals-g2','fio-esm','gfdl-cm3','gfdl-esm2g','gfdl-esm2m',
             'giss-e2-r','hadgem2-ao','hadgem2-cc','hadgem2-es','inmcm4',
             'ipsl-cm5a-lr','ipsl-cm5a-mr','ipsl-cm5b-lr','miroc-esm',
             'miroc-esm-chem','miroc5','mpi-esm-lr','mpi-esm-mr',
             'mri-cgcm3','noresm1-m')) %>%
  map(function(gcm_dir){
    gcm_dir %>%
      map(function(clim_var_fn){
        brick(clim_var_fn)
      }) %>%
      setNames(c('ppt', 'tmax', 'tmin'))
  })

# if rerunning this script for failed processes then we will drop the gcms
# that ran successfully
files_to_keep <- files %>%
  imap(function(sublist, name) {
    if(length(sublist) == 3){
      name
    }
  }) %>%
  compact()

# new files list
files <- files[which(names(files) %in% names(files_to_keep))]

rm(files_to_keep)
###############################################################################
# calc biovars and write output to the corresponding gcm folder
###############################################################################

## NOTE: The following process can run okay for about half the gcms at once.
##       Then R needs to be restarted to free the memory. So I had to run this
##       a total of 4 times (30 gcms, current&future). 

# for each gcm in the list calculate the bioclimatic variables
files %>%
  future_imap(function(gcm, name){
    out<- tryCatch(
      {
        biovars(gcm[['ppt']], gcm[['tmin']], gcm[['tmax']]) %>%
          writeRaster(filename = file.path(dir, name, 'biovars_1982_2012.tif'),
                      format='GTiff',
                      options=c('COMPRESS=LZW', 'TILED=YES',
                                'BLOCKXSIZE=256', 'BLOCKYSIZE=256'),
                      overwrite=TRUE)
      },
      error = function(cond){
        message(paste0('Error in: ', name))
        message('The error that occured was:')
        message(cond)
        return(NA)
      },
      warning = function(cond){
        message(paste0('Warning from: ', name))
        message('The warning that occured was:')
        message(cond)
        return(NULL)
      },
      finally = {message('Processing finished.')}
    )
    return(out)
  })