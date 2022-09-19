###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: examine correlation between climate
#   variables.
#     1. vedu range vs west
#     2. vedu range current vs future
#     3. west current vs future
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('raster', 'sf', 'rgdal',
         'tidyverse', 'reshape2', 'here',
         'ggmap', 'cowplot', 'ggpmisc', 'gridExtra', 'grid')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

rm(pkgs)
###############################################################################
# data
###############################################################################

# directories
dir <- here('Data')

#### vector data ####
# EPA lvl 3 ecoregion shapefile
ecoreg <- st_read(file.path(dir, 'current', 'us_eco_l3_west.shp')) 

# convex hull around vedu presence data with 50k buffer shapefile
vedu_range <- st_read(file.path(dir, 'current', 'vedu_mcp_50k.shp'))

#### current climate all west ####
# read in bioclim variables
current_west_bioclim <- brick(file.path(dir, 'current',
                                        'PRISM',
                                        'Nietupski',
                                        'biovars_1982_2012.tif')) 
names(current_west_bioclim) <- read.csv(file.path(dir, 
                                                  'current',
                                                  'PRISM',
                                                  'Nietupski',
                                                  'biovar_names.csv'))$x

## New climate variables
new_vars <- list.files(
  file.path(dir,
            'current',
            'PRISM',
            'Nietupski'),
  pattern = '(sosq_ppt.tif$)|(sosq_tavg.tif$)|(qacq_ppt.tif$)|(qacq_tavg.tif$)|(cool.*.tif$)',
  full.names = T) %>%
  stack()
names(new_vars) <- c('csppt', 'qacqppt', 'qacqtavg', 'sosqppt','sosqtavg')

# add new variables to the bioclim vars
current_west_bioclim <- current_west_bioclim %>% addLayer(new_vars) %>% brick()

#### current climate vedu range ####
# mask the western us climate data with the convex hull around
# vedu points and then convert to a dataframe
current_vedu_bioclim <- current_west_bioclim %>%
  mask(vedu_range) %>%
  as.data.frame(na.rm = T)

# convert raster brick to dataframe and remove NAs
current_west_bioclim_df <- current_west_bioclim %>% as.data.frame(na.rm = TRUE)

rm(current_west_bioclim, new_vars)
#### future climate fns ####
# future data file names from all 30 gcms
gcms <- list.dirs(file.path(dir, 'future', 'gcm'), 
                  recursive = F, 
                  full.names = F)[1:30] %>%
  map(function(gcm) {
    list.files(
      file.path(dir, 'future', 'gcm', gcm),
      pattern = "(biovars_2069_2099.tif$)|(sosq_.*_2069_2099.tif$)|(qacq_.*_2069_2099.tif$)|(cool.*_2069_2099.tif$)",
      full.names = TRUE, recursive = FALSE)
  }) %>%
  setNames(gcms)

###############################################################################
# functions
###############################################################################

# return upper triangle of correlation matrix
get_upper <- function(cor) {
  cor[lower.tri(cor)] <- NA
  return(cor)
}

# highly correlated variables
get_high_cor <- function(cor_upper) {
  # mask diagonal
  cor_upper[cor_upper == 1] <- NA
  # get row and column names associated with the indices
  high_cor <- data.frame(
    var1 = row.names(cor_upper)[
      which((cor_upper > 0.85 | cor_upper < -0.85), arr.ind = TRUE)[,'row']],
    var2 = names(cor_upper)[
      which((cor_upper > 0.85 | cor_upper < -0.85), arr.ind = TRUE)[, "col"]])
  return(high_cor)
}

# plot correlation matrix as heat map
cor_heatmap <- function(cor_upper, high_cor, fig_title) {
  # reshape dataframe for plotting
  melted <- melt(cor_upper, na.rm = TRUE)
  # mask out the diagonal
  melted[melted == 1] <- NA

  # size of the grid for figure
  g_sz <- dim(cor_upper)[1]
    
  # plot heatmap with annotation showing highly correlated variables
  p1 <- melted %>%
    ggplot(aes(Var2, Var1, fill=value)) +
    geom_tile(color = 'white') +
    scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white', 
                         na.value = NA, midpoint = 0, limit = c(-1,1),
                         space = 'Lab', name = 'Pearson\nCorrelation',
                         guide = guide_colorbar(title.position = 'top',
                                                title.hjust = 0.5)) +
    scale_y_discrete(position = 'right') +
    geom_text(aes(label = round(value, 2)), size=2.5) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = c(0.2, 0.85),
          legend.direction = 'horizontal') +
    coord_fixed() 

  # plot table of highly correlated values
  p2 <- ggplot() +
    theme_void() +
    annotate(geom = 'table',
             x = 0.5,
             y = 0.5,
             label = list(high_cor),
             table.theme = ttheme_gtbw()) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
    scale_y_continuous(limits = c(0,1), expand = c(0.01,0)) +
    annotate(geom = 'text',
             x = 0.5,
             y = 0.98,
             label = 'Highly correlated\n (r < -0.85 or r > 0.85)')
  
  comp_p <- arrangeGrob(grobs = list(p2, p1),
                        ncol = 5,
                        layout_matrix = rbind(c(1,2,2,2,2),
                                              c(1,2,2,2,2)),
                        top = textGrob(fig_title,
                                       gp=gpar(fontsize=16,fontface=2)))
  comp_p
}

# plot correlation heatmap with list of most correlated varialbes
show_correlation <- function(df, fig_title) {
  # correlation of bioclim vars
  cor.mat <- cor(df) 
  names(cor.mat) <- names(df)
  
  # find highly correlated variables
  # get upper triangle
  cor_upper <- get_upper(cor.mat)
  
  # get highly correlated variables
  high_cor <- get_high_cor(cor_upper)
  
  # check for case where we have an empty dataframe and return a dataframe with
  # 'none' for the x and y records
  if(dim(high_cor)[1] == 0){
    high_cor[1,] = c('none','none')
  }
  
  # plot correlation heatmap
  cor_heatmap(cor_upper, high_cor, fig_title)
  
}

# calculate correlation by ecoregion
eco_correlation <- function(ecoregions, ecoregion_lvl, bioclim) {
  ecoregions %>%
    split(.[[ecoregion_lvl]]) %>% # dynamic column selection
    map(function(x){
      bioclim %>%
        mask(x) %>% # clip raster brick
        as.data.frame(na.rm = TRUE) %>% # convert to df, drop na
        cor() %>% # calc correlation between biovars
        get_upper() %>% # get upper triangle
        na_if(1) %>% # drop diagonal
        as.data.frame() %>% # convet to df
        rownames_to_column() %>% # add column of row names
        pivot_longer(cols = -rowname, values_drop_na=TRUE) %>% # pivot long
        unite(name, rowname, name, sep = '_') %>% # merge col and row names
        pivot_wider(names_from = name, values_from = value) # pivot wide
    }) %>%
    bind_rows(.id = ecoregion_lvl)
}

# show the difference between correlation in two datasets
dif_correlation <- function(df1, df2, fig_title) {
  diff_cor <- get_upper(cor(df1)) - get_upper(cor(df2))
  diag(diff_cor) <- NA
  
  # melt correlation matrix
  melted <- melt(diff_cor, na.rm = TRUE) %>%
    mutate(title = fig_title)
  
  # we could dynamically set the scale to the maximum value associated with
  # the correlation matrix difference (we would use this in the limit argument
  # below)
  limit <- max(abs(diff_cor), na.rm = T)
  
  # plot heatmap with annotation showing highly correlated variables
  melted %>%
    ggplot(aes(Var2, Var1, fill=value)) +
    geom_tile(color = 'white') +
    scale_fill_gradient2(low = '#542788', high = '#b35806', mid = 'white', 
                         na.value = NA, midpoint = 0, limit = c(-1,1),
                         space = 'Lab', name = 'Correlation\nDifference',
                         guide = guide_colorbar(title.position = 'top',
                                                title.hjust = 0.5)) +
    scale_y_discrete(position = 'right') +
    geom_text(aes(label = round(value, 2)), size=2.5) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = c(0.15, 0.85),
          legend.direction = 'horizontal',
          strip.text = element_text(size = 12),
          strip.background = element_rect(color = '#d8d8d8', fill = '#d8d8d8')) +
    coord_fixed() +
    labs(title = '') + #fig_title
    facet_grid(.~title)
}

###############################################################################
# bioclim correlation (aspatial)
###############################################################################

# The purpose of this examination is to decide on 2 things. 
#
#   1.) generally, which variables should we retain and which should we drop 
#       before model fitting/tuning. which variables are highly correlated?
#   
#   2.) do any of the correlations change when comparing our sampled area 
#       (vedu range) with the projection area (US west)? If the 
#       relationship in the sampled area is drastically different we might not 
#       want to include these variables because the projection area has a much
#       different relationship. 
#
# We look at correlation at the sampled (vedu range) and projection 
# (US west) scale and compare these in order to make the decision. If there are
# sign changes in the correlation or if there is strong correlation in one but
# no correlation in the other we might want to consider dropping 1 of the 2
# variables from the model. 
# 

#### current in vedu range ####
# show correlation plot with table of highly correlated variables
current_vedu_plot <- show_correlation(current_vedu_bioclim, 'VEDU range (current)')

# save to disk
ggsave(filename = 'correlation_VEDU_current.png',
       path = here('Figures','current','correlation'),
       plot = current_vedu_plot,
       device = 'png',
       scale = 1.7,
       width = 5,
       height = 4,
       units = 'in',
       dpi = 300,
       bg = 'white')

#### current across west ####
# show correlation plot with table of highly correlated variables
current_west_plot <- show_correlation(current_west_bioclim_df, 'Western US (current)')

# save to disk
ggsave(filename = 'correlation_West_current.png',
       path = here('Figures','current','correlation'),
       plot = current_west_plot,
       device = 'png',
       scale = 1.7,
       width = 5,
       height = 4,
       units = 'in',
       dpi = 300,
       bg = 'white')

#### current vedu vs. west  ####
# show a plot of the difference between the sample and the western region
current_west_vs_vedu_dif_plot <- dif_correlation(
  current_vedu_bioclim, current_west_bioclim_df, 
  'VEDU vs West correlation difference\n (current)')

# save to disk
ggsave(filename = 'correlation_VEDU_vs_West_current_difference.png',
       path = here('Figures','current','correlation'),
       plot = current_west_vs_vedu_dif_plot,
       device = 'png',
       scale = 1.6,
       width = 5,
       height = 5,
       units = 'in',
       dpi = 300,
       bg = 'white')
#### vedu current vs. future  ####
# vedu range current & future
fut_west_bioclim_fns %>%
  imap(function(fns, gcm_name){
    # read bioclim variable data
    bvs <- brick(fns[1]) %>%
      setNames(read.csv(file.path(dir, 
                                  'PRISM', 
                                  'Nietupski', 
                                  'biovar_names.csv'))$x)
    # read new variable data
    new_vars <- stack(fns[2:6]) %>%
      setNames(c('csppt', 'qacqppt', 'qacqtavg', 'sosqppt','sosqtavg'))
    
    # combine biovars and new vars into single raster and convert to dataframe
    fut_climate <- bvs %>% 
      addLayer(new_vars) %>%
      brick() %>% 
      mask(vedu_range) %>%
      as.data.frame(na.rm = TRUE)
    
    # make correlation and correlation difference plots
    p1 <- show_correlation(
      fut_climate, 
      paste0('VEDU future ', '(', gcm_name, ')')
    )
    ggsave(filename = paste0('correlation_',gcm_name, '_', 'VEDU_future.png'),
           path = here('Figures','future','correlation'),
           plot = p1,
           device = 'png',
           scale = 1.7,
           width = 5,
           height = 4,
           units = 'in',
           dpi = 300,
           bg = 'white') 
    
    p2 <- dif_correlation(
      current_vedu_bioclim, fut_climate, 
      paste0('VEDU current vs future difference\n (current - future; ', gcm_name, ')')
    )
    
    ggsave(filename = paste0('correlation_',gcm_name, '_', 'VEDU_current_vs_future_difference.png'),
           path = here('Figures','future','correlation'),
           plot = p2,
           device = 'png',
           scale = 1.6,
           width = 5,
           height = 5,
           units = 'in',
           dpi = 300,
           bg = 'white')
    gc()
  }) 

#### west current vs. future  ####
# west current vs. west future
fut_west_bioclim_fns %>%
  imap(function(fns, gcm_name){
    # read bioclim variable data
    bvs <- brick(fns[1]) %>%
      setNames(read.csv(file.path(dir, 'current','PRISM',
                                  'Nietupski','biovar_names.csv'))$x)
    # read new variable data
    new_vars <- stack(fns[2:6]) %>%
      setNames(c('csppt', 'qacqppt', 'qacqtavg', 'sosqppt','sosqtavg'))
    
    # combine biovars and new vars into single raster and convert to dataframe
    fut_climate <- bvs %>% 
      addLayer(new_vars) %>%
      brick() %>% 
      as.data.frame(na.rm = TRUE)
    
    # make correlation and correlation difference plots
    p1 <- show_correlation(
      fut_climate, 
      paste0('West future ', '(', gcm_name, ')')
      )
    ggsave(filename = paste0('correlation_',gcm_name, '_', 'West_future.png'),
           path = here('Figures','future','correlation'),
           plot = p1,
           device = 'png',
           scale = 1.7,
           width = 5,
           height = 4,
           units = 'in',
           dpi = 300,
           bg = 'white') 
    
    p2 <- dif_correlation(
      current_west_bioclim_df, fut_climate, 
      gcm_name
      )
    
    ggsave(filename = paste0('correlation_',gcm_name, '_', 'West_current_vs_future_difference.png'),
           path = here('Figures','future','correlation'),
           plot = p2,
           device = 'png',
           scale = 2.2,
           width = 3.25,
           height = 3.25,
           units = 'in',
           dpi = 600,
           bg = 'white')
    gc()
    }) 
