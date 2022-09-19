###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: Plotting gcms by highest ranked 
#   sdm predictors
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('raster', 'sf', 'rgdal', 
         'tidyverse', 'cowplot', 'ggrepel', 'gridExtra',
         'here')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

###############################################################################
# data
###############################################################################

dir <- here('Data', 'future', 'gcm')
fig_dir <- here('Figures', 'future', 'ClimatePlots')

# labels for the gcms
gcm_names <- c('ACCESS1.0', 'BCC-CSM1.1', 'BCC-CSM1.1-m',
               'CanESM2', 'CCSM4', 'CESM1-BGC', 
               'CESM1-CAM5', 'CMCC-CM', 'CNRM-CM5', 
               'CSIRO-Mk3-6-0', 'FGOALS-g2', 'FIO-ESM',
               'GFDL-CM3', 'GFDL-ESM2G', 'GFDL-ESM2M',
               'GISS-E2-R', 'HadGEM2-AO', 'HadGEM2-CC',
               'HadGEM2-ES', 'INMCM4', 'IPSL-CM5A-LR',
               'IPSL-CM5A-MR', 'IPSL-CM5B-LR', 
               'MIROC-ESM', 'MIROC-ESM-CHEM', 'MIROC5',
               'MPI-ESM-LR', 'MPI-ESM-MR', 'MRI-CGCM3',
               'NorESM1-M')

# bioclim variable names 
bio_names <- data.frame(symbol = tolower(c("BIO2", "BIO4", "BIO10", "BIO11",
                                           "BIO15", "csppt")),
                        description = c("Mean Diurnal Range (°C)",
                                        "Temperature Seasonality (°C * 100)",
                                        "Mean Temperature of Warmest Quarter (°C)",
                                        "Mean Temperature of Coldest Quarter (°C)",
                                        "Precipitation Seasonality (mm)",
                                        "Cool Season Precipitation (mm)"))
#### western us climate ####
if(file.exists(here('Data', 'future', 'gcm', 'Summaries',
                    'gcm_medians_delta.csv'))) {
  gcm_medians_delta <- read.csv(here('Data', 'future', 'gcm', 'Summaries',
                                     'gcm_medians_delta.csv'))
}else {
  # map vs mat 
  gcm_delta_mapmat <- list.dirs(dir, recursive = F, full.names = T)[1:30] %>%
    map(function(gcm_fn){
      current <- brick(file.path(gcm_fn, 'biovars_1982_2012.tif')) %>%
        subset(c(1,12)) %>%
        setNames(c('Annual Mean Temperature', 'Annual Precipitation'))
      
      fut <- brick(file.path(gcm_fn, 'biovars_2069_2099.tif')) %>%
        subset(c(1,12)) %>%
        setNames(c('Annual Mean Temperature', 'Annual Precipitation'))
      
      delta <- fut - current 
      
      delta %>%
        cellStats(stat = 'mean', na.rm = T)%>%
        t() %>%
        as.data.frame() 
    }) %>%
    bind_rows() %>%
    mutate(gcm = gcm_names)
  
  # grab all the gcm delta climate files, calculate the median for all bands, and
  # return as a data frame, then add delta mat (for point size in plotting)
  gcm_medians_delta <- list.dirs(dir, recursive = F, full.names = T)[1:30] %>%
    map(~ brick(file.path(.x, 'final_variables_delta.tif')) %>%
          setNames(c('bio2', 'bio4', 'bio10', 'bio11', 'bio15', 'csppt')) %>%
          cellStats(stat = 'median', na.rm = T) %>%
          t() %>%
          as.data.frame()) %>%
    bind_rows() %>%
    mutate(gcm = gcm_names) %>%
    mutate(delta_mat_class = 
             case_when(gcm_delta_mapmat$Annual.Mean.Temperature < 3 ~ "< 3",
                       gcm_delta_mapmat$Annual.Mean.Temperature >= 3 & gcm_delta_mapmat$Annual.Mean.Temperature < 4 ~ '3 - 4',
                       gcm_delta_mapmat$Annual.Mean.Temperature >= 4 & gcm_delta_mapmat$Annual.Mean.Temperature < 5 ~ '4 - 5',
                       gcm_delta_mapmat$Annual.Mean.Temperature >= 5 & gcm_delta_mapmat$Annual.Mean.Temperature < 6 ~ '5 - 6',
                       gcm_delta_mapmat$Annual.Mean.Temperature > 6 ~ "> 6"),
           delta_mat = gcm_delta_mapmat$Annual.Mean.Temperature,
           delta_map = gcm_delta_mapmat$Annual.Precipitation
    )
  
  write.csv(gcm_medians_delta, 
            file = here('Data', 'future', 'gcm', 'Summaries',
                        'gcm_medians_delta.csv')) 
}

#### summary information for plotting ####
## western us 
# add an indicator so that we can color the chosen GCMs 
gcm_medians_delta <- gcm_medians_delta %>%
  mutate(point_color = if_else(
    gcm %in% c("MPI-ESM-MR",'MRI-CGCM3','FIO-ESM','CNRM-CM5'), 2,  1)
  )

# get the mean of each variable across the gcms (to define quadrants)
gcm_delta_summary <- gcm_medians_delta %>%
  select(-gcm, -point_color, -delta_mat, -delta_map, -delta_mat_class) %>%
  summarise(across(bio2:csppt, mean))

## regional
# add an indicator so that we can color the chosen GCMs 
gcm_medians_delta_regional <- gcm_medians_delta_regional %>%
  mutate(point_color = if_else(
    gcm %in% c("MPI-ESM-MR",'MRI-CGCM3','FIO-ESM','CNRM-CM5'), 2,  1)
  )
# do the same for the regional data
gcm_delta_summary_regional <- gcm_medians_delta_regional %>%
  group_by(region) %>%
  select(-gcm, -point_color, -delta_mat, -delta_map) %>%
  summarise(across(bio2:csppt, mean))

###############################################################################
# bio4 vs. csppt plots
###############################################################################

## plotting helper function for legend issue ----
# we need a hacky function to center align the legend key with the legend title
align_legend <- function(p, hjust = 0.5)
{
  # extract legend
  g <- cowplot::plot_to_gtable(p)
  grobs <- g$grobs
  legend_index <- which(sapply(grobs, function(x) x$name) == "guide-box")
  legend <- grobs[[legend_index]]
  
  # extract guides table
  guides_index <- which(sapply(legend$grobs, function(x) x$name) == "layout")
  
  # there can be multiple guides within one legend box  
  for (gi in guides_index) {
    guides <- legend$grobs[[gi]]
    
    # add extra column for spacing
    # guides$width[5] is the extra spacing from the end of the legend text
    # to the end of the legend title. If we instead distribute it by `hjust:(1-hjust)` on
    # both sides, we get an aligned legend
    spacing <- guides$width[5]
    guides <- gtable::gtable_add_cols(guides, hjust*spacing, 1)
    guides$widths[6] <- (1-hjust)*spacing
    title_index <- guides$layout$name == "title"
    guides$layout$l[title_index] <- 2
    
    # reconstruct guides and write back
    legend$grobs[[gi]] <- guides
  }
  
  # reconstruct legend and write back
  g$grobs[[legend_index]] <- legend
  g
}

## color showing change in MAT ----
p <- ggplot(gcm_medians_delta) +
  aes(x = csppt, y = bio4, fill = factor(point_color)) +
  geom_hline(yintercept = gcm_delta_summary$bio4[1], 
             linetype = 'dashed',
             size=0.75) +
  geom_vline(xintercept = gcm_delta_summary$csppt[1],
             linetype = 'dashed',
             size=0.75) +
  geom_point(aes(color = delta_mat, 
                 shape = factor(point_color), 
                 size = factor(point_color))) +
  scale_color_gradient(low = '#ffd903', high = '#d10505') +#low = 'blue', high = 'red'
  scale_fill_manual(values = alpha(c('#ffffff', '#d6d6d6'), 0.75)) +
  scale_size_manual(values = c(4, 6)) +
  scale_x_continuous(breaks = c(-10,0, 10, 20, 30, 40, 50)) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125)) +
  geom_label_repel(aes(label = gcm),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   min.segment.length = 0.25,
                   segment.color = 'grey50',
                   max.time = 5,
                   max.iter = 100000) +
  labs(x = bio_names$description[bio_names$symbol == 'csppt'],
       y = bio_names$description[bio_names$symbol == 'bio4'],
       color = 'Change in\nMean Annual\nTemperature\n(°C)') +
  theme_bw() +
  theme(panel.grid = element_line(color = 'grey'),
        panel.grid.minor = element_blank(), 
        legend.title.align = 0.5) +
  guides(shape = 'none',
         fill = 'none',
         size = 'none')
ggdraw(align_legend(p))

ggsave(filename = 'csppt_vs_bio4_delta_quadrants_colored_MAT.pdf',
       path = fig_dir,
       plot = last_plot(),
       scale = 1.7,
       width = 5.8,
       height = 3.5,
       units = 'in',
       dpi = 1200,
       bg = 'white')