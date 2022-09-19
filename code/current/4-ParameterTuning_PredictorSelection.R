###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: parameter tuning and model 
#   simplification
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('raster', 'sf', 'rgdal', 
         'tidyverse', 'reshape2',                            
         'gbm', 'dismo', 'caret',                            
         'ggmap', 'cowplot',                                 
         'doMC')                                             

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

# set the number of cores
registerDoMC(cores=28)

###############################################################################
# data
###############################################################################

# main data directory 
dir <- here('Data', 'current')
fig_dir <- here('Figures', 'current', 'tuning')

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
##        -bio1, -bio5, -bio6, -bio7, -bio8, -bio9, -bio12, -bio13, -bio14,
##        -bio16, -bio17, -bio18, -bio19, -sosqppt, -sosqtavg, -qacqppt, 
##        -qacqtavg
##
##        For more detail see 3-Predictor_examination.R


# set the seed so that our following random samples come back the same each 
# time (make sure to run at the same time as the sampling below to ensure
# the sample is exactly the same every time)
set.seed(256)

# sample 20 % of the thinned presence files (to reduce the amount of processing)
pres_fns <- list.files(file.path(dir, pres_dir), 
                       pattern = '[[:alnum:]]*_[[:alnum:]]*_5k_[[:alnum:]]*_bioclim.csv',
                       recursive = F) %>% 
  sample(size = 20, replace = F, prob = NULL) 

# read in presence data and name each dataframe with the file name
# there are 578 presence points in each dataframe
pres_dfs <- pres_fns %>%
  map(function(fn) {
    read.csv(file.path(dir, pres_dir, fn)) %>%
      mutate(lat = as.character(round(lat, 9)),
             lon = as.character(round(lon, 9)),
             presence = factor('presence')) %>%
      left_join(pres_wts, by=c('lat', 'lon'), keep = F) %>% # add the weights for each location
      mutate(wt = (wt_init/sum(wt_init))/2) %>% # relativize weights and make sure sum is equal to absence
      select(-bio1, -bio5, -bio6, -bio7, -bio8, -bio9, -bio12, -bio13, -bio14,
             -bio16, -bio17, -bio18, -bio19, -sosqppt, -sosqtavg, -qacqppt,
             -qacqtavg, -wt_init, -lat, -lon)
    }) %>%
  setNames(pres_fns)

# sample 20 % of the absence files
abs_fns <- list.files(file.path(dir, abs_dir),
                      pattern = '[[:alnum:]]*-[[:alnum:]]*-[[:alnum:]]*_5k-[[:alnum:]]*_bioclim.csv', 
                      recursive = F) %>%
  sample(size = 20, replace = F, prob = NULL)

# read in absence data and name each dataframe with the file name
# there are 10,000 pseudo-absence points in each dataframe
abs_dfs <- abs_fns %>%
  map(function(fn) {
    read.csv(file.path(dir, abs_dir, fn)) %>%
      mutate(presence = factor('absence'), 
             wt = (1/10000)/2) %>% # sum of weights equal to presence
      select(-bio1, -bio5, -bio6, -bio7, -bio8, -bio9, -bio12, -bio13, -bio14,
             -bio16, -bio17, -bio18, -bio19, -sosqppt, -sosqtavg, -qacqppt,
             -qacqtavg, -lat, -lon)
    }) %>%
  setNames(abs_fns)

# combine presence and absence data frames
dfs <- c(1:20) %>%
  map(function(i) {
    pres_dfs[[i]] %>%
      rbind(abs_dfs[[i]])
  })

rm(pres_dfs, abs_dfs, pres_fns, abs_fns, 
   pres_wts, abs_dir, pres_dir, weights_fn)
###############################################################################
# hyperparameter tuning
###############################################################################

# hyperparameters that need to be specified in a gbm model
# 1. n.trees: number of trees
# 2. interaction.depth: max depth of each tree (highest level of interaction 
#    allowed) (default is 1 but 2 is 2 way interaction) Doesn't mean that this
#    level of interaction will occur but it allows for it. Also referred to as
#    tree size.
# 3. n.minobsinnode: min number of observations in the terminal nodes of trees 
# 4. shrinkage: shrinkage parameter applied to each tree. also known as learning
#    rate (0.001 to 0.1). This controls the 'speed' of the descent. Generally, 
#    slower descents improve the resulting model. Fitted values are computed as 
#    the sum of all trees multiplied by the learning rate.
# 5. bag.fraction: fraction of training set obs selected to propose next tree 
#    (< 1 will return different fits, default = 0.5)
# 6. train.fraction: first train.fraction * nrows(data) are used to fit the gbm.
#    remainder are used for calculating loss function
# 7. cv.folds: number of cv folds to perform. used to calculate cv.error

# NOTES: dismo has gbm.step that can be used to select the optimal number of trees.
# We can also use the caret package to do a parallel version of cv grid search
# https://machinelearningmastery.com/tuning-machine-learning-models-using-the-caret-r-package/
# https://stats.stackexchange.com/questions/229356/gbm-package-vs-caret-using-gbm

set.seed(256)

# set the characteristics of the train method
# since we only have ~500 presence points per thinned dataset we are going 
# to go with k=5 so that we have ~100 pres in each model

# need to specify seeds so that process is repeatable
seedsss <- c(1:15) %>% 
  map(function(i){c(1:150) * i}) %>%
  append(256)

control <- trainControl(
  method = 'repeatedcv',
  number = 5,                 # number of folds
  repeats = 3,                # number of iterations (we will keep this low for now since we are doing this for 20 datasets)
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
grid <- expand.grid(n.trees = c(1:50)*100, # 100-3000 trees
                    interaction.depth = c(1,2,3,5,7,10),   
                    n.minobsinnode = c(1:5)*10,
                    shrinkage = c(0.1, 0.05, 0.01, 0.005, 0.001))


if (file.exists(file.path(dir, 'modeling', 'gbm_tuning_results.rds'))) {
  # read in tuning results file
  gbm_tuning_results <- readRDS(file.path(dir, 
                                          'modeling', 
                                          'gbm_tuning_results.rds'))

} else {
  gbm_tuning_results <- dfs %>%
    map(function(df) {
      # for readability, separate predictor, response, and weights
      vedu_pres <- df$presence
      bio_var <- df %>% 
        select(bio2, bio3, bio4, bio10, bio11, bio15, csppt)
      weight <- df$wt
      
      # perform tuning
      train(bio_var, vedu_pres,
            method = 'gbm',
            weights = weight,
            metric = 'ROC',
            maximize = TRUE,
            trControl = control,
            tuneGrid = grid,
            bag.fraction = 0.75
      )
    })
  
  # save tuning results as R dataset
  saveRDS(gbm_tuning_results, 
          file.path(dir, 'modeling', 'gbm_tuning_results.rds'),
          compress = TRUE)
}

rm(seedsss, grid, control)
###############################################################################
# examine tuning results
###############################################################################

####  Examine the parameters for the best performing models
## NOTE: It's important to remember that caret places greater penalty on models
##       with more trees than models with higher interaction depth (tree size).
##       According to the Elith BRT paper she was suggesting at least 1000 trees
##       possibly indicating that she places higher penalty on models with higher
##       interaction depth. 
##       Generally, higher shrinkage is desired because it allows for a slower 
##       fitting process and 'better' resulting model. However, as shrinkage
##       increases we also see an increase in the total number of trees.

# plot of ROC by # trees, tree depth, min obs per node, and shrinkage
summary_plots <- gbm_tuning_results %>%
  imap(function(results, index) {
    # create plot of performance by each of the tuning vars
    p <- ggplot(results) +
      labs(x = '# of Trees')
    # save plots to figure folder
    ggsave(file.path(fig_dir, paste0('tuning_results_', index, '.png')),
           plot = p,
           scale = 2.5, 
           width = 1920,
           height = 1080,
           units = 'px')
    p
  })

# lets look at a subset of the facets in the previous figure
sub_figs <- gbm_tuning_results %>%
  imap(function(data, index) {
    # subset the tuning results just to lr 0.005 and 3-7 max tree size
    subset <- data$results %>%
      filter((shrinkage == 0.005) & 
               ((interaction.depth >= 3) & (interaction.depth < 10)) &
               (n.minobsinnode == 40))
    
    # grab the previously determined optimal number of trees
    opt_trees <- optimal_trees[[index]]$n.trees
    
    p <- subset %>%
      ggplot(aes(x = n.trees, y = ROC, color = as.factor(interaction.depth))) +
      geom_point() +
      geom_line() +
      geom_vline(xintercept = opt_trees, alpha = 0.75) +
      
      theme_bw() +
      labs(x = '# of Trees', 
           y = 'ROC (Repeated Cross-Validation',
           color = 'Max Tree Depth')
    
    # save plots to figure folder
    ggsave(file.path(fig_dir, paste0('tuning_results_lr005_', index, '.png')),
           plot = p,
           scale = 2, 
           width = 5,
           height = 3,
           units = 'in')
    p
  })

# look at the 'best' models for each of the 20 replicates based on maximizing ROC
# we are not going to use these as they are likely over fit to the data
best_tunes <- gbm_tuning_results %>%
  imap(function(result, index) {
    result$results %>%
      filter((n.minobsinnode == 40) & (interaction.depth == 5) & (shrinkage == 0.005)) %>%
      arrange(desc(ROC)) %>%
      .[1,] %>%
      mutate(replicate = index)
  }) %>%
  bind_rows()

## Conclusions: we can see that none of the models with a learning rate of 0.001
##    were quite able to reach a peak predictive performance. However, 0.005
##    seemed hit the peak between 1000 and 2500 (ish) trees, regardless of depth,
##    or min obs per node. Interestingly, when the learning rate was too low
##    there seemed to be a peak and then decrease in the ROC of models. Caret
##    always suggested the models with the greatest complexity (depth = 10) as
##    the 'best' but these models didn't turn out much better than many of those
##    with half of the depth. The point of diminishing returns seems to be around 5.
##    As the min.obs.per.node increased model performance decreased and there
##    was a greater difference between models with varying depth. However, max
##    depth helped to control the impact of obs per node. At a depth of 5, most
##    of the performance seemed similar between min.obs so we ended up going
##    with 40 observations to be conservative. At 50 obs, we saw that the
##    difference in perforance between trees of various size was commonly higher.
##    We also tested the tree optimization algorithm with different minobs and
##    it seemed to perform quite similarly between 30 and 40.
## 
##    So now our final model with use lr = 0.005, depth = 5, minobs = 40

###############################################################################
# determine optimal number of trees in model
###############################################################################

# we will use gbm.step to determine the optimal number of trees based on the 
# other previously chosen hyperparameters
# NOTE: Adding weights to the observations changes the deviance but doesn't seem
#       to impact the final suggested number of trees. 
if (file.exists(file.path(dir, 'modeling', 'optimal_trees.rds'))) {
  # read in tuning results file
  optimal_trees <- readRDS(file.path(dir, 'modeling', 'optimal_trees.rds'))
  
} else {
  optimal_trees <- dfs %>%
    map(function(df) {
      # convert 'presence' and 'absence' to 1,0
      df_prep <- df %>%
        mutate(presence = if_else(presence == 'presence', 1, 0))
      
      # calc optimal # of trees
      gbm.step(data = df_prep, 
               gbm.x = c(1:7,10:12),
               gbm.y = 13,
               family = 'bernoulli',
               tree.complexity = 5,
               learning.rate = 0.005,
               bag.fraction = 0.75,
               n.minobsinnode = 40,
               site.weights = df_prep$wt,
               n.folds = 5,
               prev.stratify = TRUE,
               n.trees = 100,
               max.trees = 10000, 
               plot.main = TRUE)
    })
  
  # save this object to save time in loading later on
  saveRDS(optimal_trees, 
          file.path(dir, 'modeling', 'optimal_trees.rds'),
          compress = TRUE)
  }

# mean number of optimal trees
median_opt_trees <- optimal_trees %>% 
  map(~ .x$n.trees) %>% 
  unlist() %>%
  median()

###############################################################################
# variable selection (model simplification)
###############################################################################

# model simplification (i.e., dropping unimportant predictors)
# alpha controls the number of drops when running in 'auto' mode 
# (see ProjecNotes.docx for details)
if (file.exists(file.path(dir, 'modeling', 'suggested_simplification.rds'))) {
  # open rds object so that we dont have to rerun the simplification code
  suggested_simplification <- readRDS(file.path(dir, 'modeling', 'suggested_simplification.rds'))
  
} else {
  
  # determine which variables to drop for each of 20 replicates
  suggested_simplification <- optimal_trees %>%
    map(function(model){
      gbm.simplify(model, 
                   n.folds = 5,
                   n.drops = 'auto',
                   alpha = 1,
                   prev.stratify = TRUE,
                   plot = TRUE)
    })
  
  # save the resulting object for future examination
  saveRDS(suggested_simplification, 
          file.path(dir, 'modeling', 'suggested_simplification.rds'),
          compress = TRUE)
}

# which variables were chosen most frequently across models to be dropped?
final_drops <- suggested_simplification %>%
  map(~ as.data.frame(.x$final.drop)) %>%
  bind_rows() %>%
  drop_na() %>%
  group_by(preds, order) %>%
  tally()

## NOTE: These results suggest that we should drop bio3 (90% of models suggest 
##       this). It is also suggested that we use 1500 trees in the final model
##       (see previous section of code for optimal trees).  