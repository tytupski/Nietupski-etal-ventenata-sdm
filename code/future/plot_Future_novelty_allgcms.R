###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: future climate novelty
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('terra', 'sf', 'tidyverse', 'cowplot', 'here')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

###############################################################################
# data
###############################################################################

# relevant directories
dir <- here('Data', 'future', 'gcm')
fig_dir <- here('Figures', 'future', 'maps')

# gcms of interest
gcms <- list.dirs(dir, full.names = F, recursive = F)[1:30] %>%
  setNames(c('ACCESS1.0', 'BCC-CSM1.1', 'BCC-CSM1.1-m',
             'CanESM2', 'CCSM4', 'CESM1-BGC', 
             'CESM1-CAM5', 'CMCC-CM', 'CNRM-CM5', 
             'CSIRO-Mk3-6-0', 'FGOALS-g2', 'FIO-ESM',
             'GFDL-CM3', 'GFDL-ESM2G', 'GFDL-ESM2M',
             'GISS-E2-R', 'HadGEM2-AO', 'HadGEM2-CC',
             'HadGEM2-ES', 'INMCM4', 'IPSL-CM5A-LR',
             'IPSL-CM5A-MR', 'IPSL-CM5B-LR', 
             'MIROC-ESM', 'MIROC-ESM-CHEM', 'MIROC5',
             'MPI-ESM-LR', 'MPI-ESM-MR', 'MRI-CGCM3',
             'NorESM1-M'))


# state boundaries
states <- st_read(here('Data', 'current', 'states_west_clip.shp'))

# ecoregion boundaries
ecoreg <- st_read(here('Data', 'current', 'us_eco_l3_west.shp'))

# all mess plots for all gcms (with NA's filled with 0)
mess <- gcms %>%
  map(function(gcm) {
    mess <- rast(file.path(dir, gcm,
                           'novel_climate',
                           'MESS_MoD_vedu_vs_future_masked.tif'))
    mess[is.na(mess)] <- 0
    mess %>%
      mask(vect(states))
  }) %>%
  rast()

# count of gcms ranking each predictor as the most important
proportion_novel <- c(1:6) %>%
  map(~ app(mess, fun = function(pix) {mean(pix == .x)})) %>%
  rast() %>%
  setNames(c('bio2', 'bio4', 'bio10', 'bio11', 'bio15', 'csppt')) %>%
  mask(vect(states)) %>%
  as.data.frame(xy = T, na.rm = T) %>%
  pivot_longer(cols = c('bio2', 'bio4', 'bio10', 'bio11', 'bio15', 'csppt'),
               names_to = 'predictor', values_to = 'value') %>%
  filter(value > 0) %>%
  mutate(value = ceiling(value*30)) %>% # convert proportion to # of gcms
  mutate(class = case_when((value >= 1 & value < 7) ~ 1, 
                           (value >= 7 & value < 13) ~ 2,
                           (value >= 13 & value < 19) ~ 3,
                           (value >= 19 & value < 25) ~ 4,
                           (value >= 25 & value <= 30) ~ 5)) # group gcms into increments of 6

###############################################################################
# plots
###############################################################################

#### individual novelty plots for all 30 gcms  ####
## NOTE: These figures are pulled into GIMP to make the composite plot.
gcms %>%
  imap(function(gcm, gcm_name) {
    mess <- rast(file.path(dir, gcm, 'novel_climate', 'MESS_MoD_vedu_vs_future_masked.tif')) %>%
      setNames(c('mess')) %>%
      as.data.frame(xy = T, na.rm = T) %>%
      mutate(title = gcm_name)
    
    mess %>%
      ggplot() +
      geom_sf(data = states, fill = 'white', color = 'white', size = 0.2) +
      geom_tile(aes(x = x, y = y, 
                    fill = factor(mess, levels = c(1,2,3,4,5,6),
                                  labels = c('bio2 ', 
                                             'bio4',
                                             'bio10',
                                             'bio11', 
                                             'bio15',
                                             'csppt')))) +
      scale_fill_manual(values = c('#e69f00', '#d55e00', '#cc79a7',
                                   '#0072b2', '#009e73', '#f0e442')) +
      scale_y_continuous(expand = c(0.005,0.005)) +
      scale_x_continuous(expand = c(0.005,0.005)) +
      geom_sf(data = states, fill = NA, color = '#d3d3d3', size = 0.2) +
      theme_grey() +
      labs(x = '', y = '', fill = '') +
      theme(panel.background = element_rect(color = 'white', 
                                            fill = 'white'),
            panel.grid.major = element_line(color = 'grey50', 
                                            size = 0.25, 
                                            linetype = 'dashed'), 
            axis.ticks = element_blank(),
            legend.position = 'none', 
            strip.text = element_text(size = 14)) +
      facet_wrap(.~title)
    
    ggsave(filename = paste0('Climatic_novelty_', gcm, '.png'),
           path = fig_dir,
           device = 'png',
           scale = 2.4,
           width = 1.1,
           height = 1.25,
           units = 'in',
           dpi = 1200,
           bg = 'white')
  })

# pull out legend to add to composite plot (also used for importance composite)
gcm <- 'access1-0'
mess <- raster(file.path(dir, gcm, 'novel_climate', 'MESS_MoD_vedu_vs_future_masked.tif')) %>%
  setNames(c('mess')) %>%
  as.data.frame(xy = T, na.rm = T) %>%
  mutate(title = gcm)

p <- mess %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, 
                fill = factor(mess, levels = c(1,2,3,4,5,6),
                              labels = c('Mean Diurnal Range', 
                                         'Temperature Seasonality',
                                         'Mean Temperature of Warmest Quarter',
                                         'Mean Temperature of Coldest Quarter', 
                                         'Precipitation Seasonality',
                                         'Cool Season Precipitation')))) +
  scale_fill_manual(values = c('#e69f00', '#d55e00', '#cc79a7',
                               '#0072b2', '#009e73', '#f0e442')) +
  labs(x = '', y = '', fill = '') +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.spacing.x = unit(0.3, units = 'cm'))

# get legend
l <- get_legend(p + theme(legend.box.margin = margin(2, 2, 2, 2)))

# export 
ggsave(filename = 'Climatic_novelty_legend.png',
       path = fig_dir,
       plot = ggdraw(l),
       device = 'png',
       scale = 1.5,
       width = 4.4,
       height = 0.5,
       units = 'in',
       dpi = 1200,
       bg = 'white')

#### count of gcms showing novelty for each predictor ####
proportion_novel %>%
  ggplot() +
  geom_sf(data = states, fill = 'white', color = NA) +
  geom_tile(aes(x = x, 
                y = y, 
                fill = factor(class,
                              levels = c(1,2,3,4,5),
                              labels = c('1-6',
                                         '7-12',
                                         '13-18',
                                         '19-24',
                                         '25-30')),
                color = factor(class,
                               levels = c(1,2,3,4,5),
                               labels = c('1-6',
                                          '7-12',
                                          '13-18',
                                          '19-24',
                                          '25-30')))) + 
  scale_fill_viridis_d(option = 'plasma',
                       guide = guide_legend(title.position = 'top',
                                            title.hjust = 0.5),
                       direction = 1,
                       begin = 0,
                       end = 0.8) +
  scale_color_viridis_d(option = 'plasma',
                       guide = guide_legend(title.position = 'top',
                                            title.hjust = 0.5),
                       direction = 1,
                       begin = 0,
                       end = 0.8) +
  scale_y_continuous(expand = c(0.005,0.005)) +
  scale_x_continuous(expand = c(0.005,0.005)) +
  geom_sf(data = ecoreg, fill = NA, color = 'grey25', size = 0.15) +
  theme_grey() +
  labs(x = '', y = '', fill = 'Number of GCMs') +
  theme(panel.background = element_rect(color = 'white',
                                        fill = 'white'),
        panel.grid.major = element_line(color = 'grey50',
                                        size = 0.25,
                                        linetype = 'dashed'),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        legend.spacing.x = unit(0.1, 'lines'),
        plot.margin = margin(1, 0, 0, 0),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(-12,-10, 2,-10)) +
  guides(fill = guide_legend(label.position = 'bottom',
                             title.position = 'top',
                             title.hjust = 0.5,
                             keywidth = unit(30, 'points'),
                             keyheight = unit(20, 'points')),
         color = 'none') +
  facet_wrap(.~factor(predictor, 
                      levels = c('csppt', 'bio4', 'bio11',
                                 'bio10', 'bio15', 'bio2'),
                      labels = c("Cool Season Precipitation",
                                 "Temperature Seasonality",
                                 "Mean Temperature of\n Coldest Quarter",
                                 "Mean Temperature of\n Warmest Quarter",
                                 "Precipitation Seasonality",
                                 "Mean Diurnal Range")))

ggsave(filename = 'GCM_Count_Novel.tiff',
       path = fig_dir,
       scale = 1.5,
       width = 4.5,
       height = 4.5,
       units = 'in',
       dpi = 1200,
       compression = 'lzw',
       bg = 'white')
