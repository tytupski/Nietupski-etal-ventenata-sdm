###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: Final models based on results from
# 4-ParameterTuning_PredictorSelection.R
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('raster', 'sf', 'rgdal',          
         'tidyverse', 'here',                                 
         'gbm', 'dismo', 'caret', 'PresenceAbsence',         
         'ggmap', 'cowplot', 'ggpmisc', 'pdp',               
         'doMC', 'furrr')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

# set the number of cores
registerDoMC(cores=24)

###############################################################################
# data
###############################################################################

# main data directory 
dir <- here('Data', 'current')
fig_dir <- here('Figures', 'current')

# names and symbols of bioclim variables
bio_names <- data.frame(symbol = tolower(c("BIO1", "BIO2", "BIO3", "BIO4", 
                                           "BIO5", "BIO6", "BIO7", "BIO8",
                                           "BIO9", "BIO10", "BIO11", "BIO12",
                                           "BIO13", "BIO14", "BIO15", "BIO16",
                                           "BIO17", "BIO18", "BIO19", "qacqppt",
                                           "qacqtavg", "sosqppt", "sosqtavg",
                                           "csppt")),
                        description = c("Annual Mean Temperature (°C)",
                                        "Mean Diurnal Range\n (°C)",
                                        "Isothermality",
                                        "Temperature Seasonality\n (°C * 100)",
                                        "Max Temperature of Warmest Month (°C)",
                                        "Min Temperature of Coldest Month (°C)",
                                        "Temperature Annual Range (°C)",
                                        "Mean Temperature of Wettest Quarter (°C)",
                                        "Mean Temperature of Driest Quarter (°C)",
                                        "Mean Temperature of\n Warmest Quarter (°C)",
                                        "Mean Temperature of\n Coldest Quarter (°C)",
                                        "Annual Precipitation (mm)",
                                        "Precipitation of Wettest Month (mm)",
                                        "Precipitation of Driest Month (mm)",
                                        "Precipitation Seasonality\n (mm)",
                                        "Precipitation of Wettest Quarter (mm)",
                                        "Precipitation of Driest Quarter (mm)",
                                        "Precipitation of Warmest Quarter (mm)",
                                        "Precipitation of Coldest Quarter (mm)",
                                        "Precipitation of Quarter After Coldest Quarter (mm)",
                                        "Mean Temperature of Quarter After Coldest Quarter (°C)",
                                        "Precipitation of the Start of Season (mm)",
                                        "Mean Temperature of the Start of Season (°C)",
                                        "Cool Season Precipitation\n (mm)"))

#### vedu data ####
# presence, pseudo-absence, and weights
weights_fn <- 'vedu_presence_rdDist_origDist.shp'
pres_dir <- 'thinned-presence'
abs_dir <- 'pseudo-absence'

# load weights and calculate final weight
# round lat and lon for comparisons later
pres_wts <- st_read(file.path(dir, weights_fn)) %>%
  mutate(wt_init = rd_dist * orig_dist,
         lon = as.character(round(st_coordinates(.)[,1], 9)),
         lat = as.character(round(st_coordinates(.)[,2], 9))) %>%
  select(wt_init, lat, lon) %>%
  st_drop_geometry()

## NOTES: After examination of the correlation structure of bioclim variables we
##        decided to drop the following from further analysis:
##
##        -bio1, -bio5, -bio6, -bio7, -bio8, -bio9, -bio12, -bio13, -bio14,
##        -bio16, -bio17, -bio18, -bio19, -sosqppt, -sosqtavg, -qacqppt, -qacqtavg
##
##        For more detail see 3-Predictor_examination.R
##
##        Results from the parameter tuning and model simplification process also
##        suggested we drop -bio3 and use 1500 trees in the final model. For 
##        more details 4-ParameterTuning_PredictorSelection.R
##
##        The final model includes:
##        bio2, bio4, bio10, bio11, bio15, csppt


# the thinned presence files
pres_fns <- list.files(file.path(dir, pres_dir), 
                       pattern = '[[:alnum:]]*_[[:alnum:]]*_5k_[[:alnum:]]*_bioclim.csv',
                       recursive = F) 

# read in presence data and name each dataframe with the file name
# there are 578 presence points in each dataframe
pres_dfs <- pres_fns %>%
  map(function(fn) {
    read.csv(file.path(dir, pres_dir, fn)) %>%
      mutate(lat = as.character(round(lat, 9)),
             lon = as.character(round(lon, 9)),
             presence = 1) %>%
      left_join(pres_wts, by=c('lat', 'lon'), keep = F) %>% # add the weights for each location
      mutate(wt = (wt_init/sum(wt_init))/2) %>% # relativize weights and make sure sum is equal to absence
      select(presence, wt, bio2, bio4, bio10, bio11, bio15, csppt)
  }) %>%
  setNames(pres_fns)

# check all dataframes for nans
pres_nans <- pres_dfs %>% 
  map(~ .x %>%
        select_if(function(x) any(is.na(x))) %>%
        summarise_each(funs(sum(is.na(.))))
      ) %>%
  bind_rows()

# print to screen if there are any nans
print(paste0("Dataframe with nans is empty: ", is_empty(pres_nans)))

# sample 20 % of the absence files
abs_fns <- list.files(file.path(dir, abs_dir),
                      pattern = '[[:alnum:]]*-[[:alnum:]]*-[[:alnum:]]*_5k-[[:alnum:]]*_bioclim.csv', 
                      recursive = F)

# read in absence data and name each dataframe with the file name
# there are 10,000 pseudo-absence points in each dataframe
abs_dfs <- abs_fns %>%
  map(function(fn) {
    read.csv(file.path(dir, abs_dir, fn)) %>%
      mutate(presence = 0, 
             wt = (1/10000)/2) %>% # sum of weights equal to presence
      select(presence, wt, bio2, bio4, bio10, bio11, bio15, csppt)
  }) %>%
  setNames(abs_fns)

# check all dataframes for nans
abs_nans <- abs_dfs %>% 
  map(~ .x %>%
        select_if(function(x) any(is.na(x))) %>%
        summarise_each(funs(sum(is.na(.))))
  ) %>%
  bind_rows()

# print to screen if there are any nans
print(paste0("Dataframe with nans is empty: ", is_empty(abs_nans)))

# combine presence and absence data frames
dfs <- c(1:100) %>%
  map(function(i) {
    pres_dfs[[i]] %>%
      rbind(abs_dfs[[i]])
  })

rm(pres_dfs, abs_dfs, pres_fns, abs_fns, 
   pres_wts, abs_dir, pres_dir, weights_fn,
   abs_nans, pres_nans)
###############################################################################
# model evaluation
###############################################################################

# use caret to calculate evaluation stats based on our final hyperparameters
set.seed(256)

# set the characteristics of the train method
# since we only have ~500 presence points per thinned dataset we are going 
# to go with k=5 so that we have ~100 pres in each model

# need to specify seeds so that process is repeatable
seedsss <- c(1:15) %>% 
  map(function(i){c(1:150) * i}) %>%
  append(256)

control <- trainControl(
  method = 'cv',
  number = 5,                 # number of folds
  search = 'grid',            # search method (grid =  all combinations instead of a random subset)
  returnData = FALSE,         # only give us the models. toss the data
  savePredictions = 'none',   # don't need to save predictions used to eval hyperparams
  classProbs = TRUE,          # need this for 2 class models
  summaryFunction = twoClassSummary,    # use roc to assess best performing model
  sampling = 'down',          # make sure samples have equal number of least prevalent class (presence)
  seeds = seedsss,            # set seeds so parallel iterations are consistent
  allowParallel = TRUE        # allow parallel backend
)

# set the parameter grid based on recommendations from the Elith paper on BRT
grid <- expand.grid(n.trees = 1500, 
                    interaction.depth = 5,   
                    n.minobsinnode = 40,
                    shrinkage = 0.005)

cv_eval_results <- dfs %>%
  map(function(df) {
    # for readability, separate predictor, response, and weights
    vedu_pres <- if_else(df$presence == 1, 'presence', 'absence') %>%
      as.factor()
    bio_var <- df %>% select(-wt, -presence)
    weight <- df$wt
    
    # perform 5-fold cross-validation
    train(bio_var, vedu_pres,
          method = 'gbm',
          weights = weight,
          metric = 'ROC',
          maximize = TRUE,
          trControl = control,
          tuneGrid = grid,
          bag.fraction = 0.75
    )
  }) %>%
  imap(~ .x$results %>%
        select(ROC, Sens, Spec, ROCSD, SensSD, SpecSD) %>%
        mutate(folds = 5, iter = .y)) %>%
  bind_rows()


# save tuning results as R dataset
saveRDS(cv_eval_results, 
        file.path(dir, 'modeling', 'cv_eval_results.rds'),
        compress = TRUE)

rm(control, grid, seedsss)
###############################################################################
# final model fitting
###############################################################################

# fit gbm model for each of the 100 data sets
#
# Final hyperparameters from 4-ParameterTuning_PredictorSelection.R script:
#
#   n.trees: 1500
#   interaction.depth: 5
#   n.minobsinnode: 40
#   shrinkage: 0.005
#   bag.fraction: 0.75
#   cv.folds: 0
#

if(file.exists(file.path(dir, 'modeling', 'final_models.rds'))) {
  final_mods <- readRDS(file.path(dir, 'modeling', 'final_models.rds'))
} else{
  # fit a final model for each replicate
  final_mods <- dfs %>%
    map(~ gbm(
      formula = presence~bio2+bio4+bio10+bio11+bio15+csppt,
      distribution = 'bernoulli',
      data = .x,
      weights = .x$wt,
      n.trees = 1500,
      interaction.depth = 5,
      n.minobsinnode = 40,
      shrinkage = 0.005,
      bag.fraction = 0.75
    ))
  
  # save final models to disk
  saveRDS(final_mods,
          file.path(dir, 'modeling', 'final_models.rds'),
          compress = TRUE)
}

###############################################################################
# determine optimal threshold from models
###############################################################################

# our presence data with one of the 100 pseudo-absence datasets
obs <- st_read(file.path(dir, 
                          'vedu_presence.shp')) %>%
  select(bio2, bio4, bio10, bio11, bio15, csppt) %>%
  st_drop_geometry() %>%
  rbind(read.csv(file.path(dir, 
                           'pseudo-absence',
                           'pseudo-absence-mask_5k-1_bioclim.csv')) %>%
          select(bio2, bio4, bio10, bio11, bio15, csppt))

# prep data into format expected by the 'optimal.thresholds' function
preds <- data.frame(plotID = c(1:12083), Observed = c(rep(1, 2083), rep(0, 10000))) %>%
  cbind(final_mods %>% 
          map(~ predict(.x, obs, n.trees = 1500, type = 'response')) %>%
          bind_cols() %>%
          rename_with(~gsub('...', 'Predicted', .x, fixed = T))
        )

# use the PresenceAbsence package to find the optimal threshold based on kappa
# and the maximization of the sum of sensitivity and specificity
# see Freeeman and Moisen 2008
opt_thrsh <- optimal.thresholds(preds, threshold = 101, opt.methods = 4) %>%
  pivot_longer(cols = 2:101, names_to = 'model', values_to = 'threshold') %>%
  group_by(Method) %>%
  summarize(mean_thresh = mean(threshold),
            sd_thresh = sd(threshold))


###############################################################################
# model importance and pdp
###############################################################################
# set up session for furr map
plan(multisession, workers = 12)

# predictors of interest (all 9 in model)
pred_names <- bio_names[c(2,4,10,11,15,24),]$symbol

#### importance values ####
final_import <- final_mods %>%
  map(~ summary(.x) %>% 
        select(rel.inf) %>%
        t() %>%
        as.data.frame()) %>%
  bind_rows() %>%
  pivot_longer(cols = everything(),
               names_to = 'biovar',
               values_to = 'importance') %>%
  group_by(biovar) %>%
  summarise(quant = c('05', '50', '95'),
            value = quantile(importance, c(0.05, 0.5, 0.95))) %>%
  ungroup() %>%
  pivot_wider(names_from = quant,
              values_from = value,
              names_prefix = 'p')

# variable descriptions to add to importance for interpretation
var_desc <- bio_names[c(2,4,10,11,15,24),] %>%
  arrange(symbol) %>%
  cbind(p50 = final_import %>% arrange(biovar) %>% .$p50) 

# plot final median importance with 5th and 95th percentiles  
# we modified this to make a combined plot- would need to comment out the
# geom_text call and remove the axis.text and axis.ticks settings in the theme
import_plot <- final_import %>%
  ggplot(aes(y = factor(reorder(biovar, p50),
                        labels = c("Mean\n Diurnal Range",
                                   "Precipitation\n Seasonality",
                                   "Mean Temperature\n of Warmest Quarter",
                                   "Mean Temperature\n of Coldest Quarter",
                                   "Temperature\n Seasonality",
                                   "Cool Season\n Precipitation")),
             x = p50)) +
  geom_col() +
  geom_errorbar(aes(xmin = p05, xmax = p95), width = 0.35) +
  geom_text(aes(label = factor(reorder(biovar, p50),
                           labels = c("6",
                                      "5",
                                      "4",
                                      "3",
                                      "2",
                                      "1"))),
            position=position_stack(vjust = 0.5),
            color = 'white') +
  # scale_y_discrete(position = 'right') +
  # annotate(geom = 'table',
  #          x =  dim(final_import)[1]*1.05,
  #          y = max(final_import$p95)*0.95,
  #          label = list(var_desc %>% arrange(desc(p50)) %>% select(-p50))) +
  theme_light() +
  labs(y = '', x = 'Relative Importance') +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(1,1,1,2))

if(!file.exists(file.path(fig_dir, 'final_model', 'vedu_final_mod_importance.png'))){
  ggsave(filename = 'final_model/vedu_final_mod_importance.png',
         path = fig_dir,
         plot = import_plot,
         device = 'png',
         scale = 1.75,
         width = 3,
         height = 2,
         units = 'in',
         dpi = 300,
         bg = 'white')
}

#### partial dependence plot ####

# calculate partial dependence for each model and return as a single dataframe
pdp_df <- final_mods %>%
  future_imap(function(mod, idx) {
    pred_names %>%
      map(~ mod %>%
            partial(pred.var = .x, grid.resolution = 101, plot = F,
                    train = dfs[[idx]], n.trees = 1500, prob = T, 
                    recursive = T) %>%
            data.frame() %>%
            rename(x_loc = as.name(.x)) %>%
            mutate(bio_var = .x, mod_num = idx)
            ) 
  }) %>%
  bind_rows()

# set up facet labels
pred_labs <- bio_names[c(2,4,10,11,15,24),]$description
names(pred_labs) <- pred_names

# create partial dependence plot of mean partial dependence with 90% percentile
# interval around the mean line
pdp_plot <- pdp_df %>%
  group_by(mod_num) %>%
  arrange(x_loc) %>%
  ggplot(aes(x = x_loc, y = yhat)) +
  geom_line(aes(group = mod_num), alpha = 0.05) +
  geom_smooth(method = 'loess', span = 0.1, color = 'red', size = 0.75) +
  facet_wrap(. ~ factor(bio_var, 
                        levels = c('csppt', 'bio4', 'bio11',
                                   'bio10', 'bio15', 'bio2'),
                        labels = c("1 - Cool Season Precipitation\n (mm)",
                                   "2 - Temperature Seasonality\n (°C * 100)",
                                   "3 - Mean Temperature of\n Coldest Quarter (°C)",
                                   "4 - Mean Temperature of\n Warmest Quarter (°C)",
                                   "5 - Precipitation Seasonality\n (mm)",
                                   "6 - Mean Diurnal Range\n (°C)")),
             scales = 'free_x') +
  theme_light() +
  theme(strip.text = element_text(colour = 'black'),
        strip.background = element_rect(fill = '#d9d9d9', colour = '#d9d9d9'),
        plot.margin = margin(1,1,1,5)) +
  labs(y = 'Probability', x = '')

if(!file.exists(file.path(fig_dir, 'final_model', 'vedu_final_mod_pdp.png'))){
  ggsave(filename = 'final_model/vedu_final_mod_pdp.png',
         path = fig_dir,
         plot = last_plot(),
         device = 'png',
         scale = 1.0,
         width = 6.5,
         height = 4.5,
         units = 'in',
         dpi = 500,
         bg = 'white')
}

#### combined importance and pdp plots ####

pdp_import <- plot_grid(import_plot, pdp_plot,
                        align = 'v',
                        axis = 'tb',
                        ncol = 2,
                        rel_widths = c(0.45,1),
                        labels = c('A', "B"))

# GCB requires eps or pdf but eps doesnt support transparency
ggsave(filename = 'final_model/vedu_final_mod_pdp_plus_import.pdf',
       path = fig_dir,
       plot = pdp_import,
       scale = 1.3,
       width = 6.5,
       height = 3.5,
       units = 'in',
       dpi = 1200,
       bg = 'white')
