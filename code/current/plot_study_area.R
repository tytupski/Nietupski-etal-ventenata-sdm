###############################################
# Author: Ty Nietupski
#
# Code in support of the manuscript titled "An
#   exploration of current and future 
#   niche-based invasion projections and
#   uncertainty for an invasive grass in the
#   western US"
#
# Objective: plot of study area showing
#     1. presence
#     2. minimum convex polygon (invaded range)
#     3. NLCD land cover
#     4. ecoregions
###############################################

###############################################################################
# libraries
###############################################################################

pkgs = c('sf', 'terra', 'tidyverse', 'gridExtra', 'cowplot', 'here', 
         'ggnewscale', 'FedData')

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(pkgs)

###############################################################################
# data
###############################################################################

# relevant directories
dir <- here('Data', 'current')
fig_dir <- here('Figures', 'current', 'maps')

# state boundaries
states <- st_read(file.path(dir, 'states_west_clip.shp'))%>%
  st_transform(crs = st_crs(4326))

# ecoregion boundaries
ecoreg <- st_read(file.path(dir, 'us_eco_l3_west.shp')) %>%
  mutate(NA_L2NAME = if_else(NA_L2NAME == 'UPPER GILA MOUNTAINS (?)',
                             'UPPER GILA MOUNTAINS', 
                             NA_L2NAME))
ecoreg <- ecoreg %>%
  mutate(area_m2 = st_area(ecoreg))

# get ecoregion centroids for labeling and keep only the largest polygon
eco_centroids <- st_centroid(ecoreg, of_largest_polygon = T) %>%
  group_by(US_L3NAME) %>%
  filter(area_m2 == max(area_m2)) %>%
  ungroup() %>%
  mutate(x = st_coordinates(geometry)[,1],
         y = st_coordinates(geometry)[,2]) %>%
  arrange(US_L3NAME) %>% #desc(y), desc(x)
  st_drop_geometry() %>%
  mutate(Label = seq(1,35),
         Ecoregion = US_L3NAME)

# change the latitude for labels 5, 7, and 10 so that they do not overlap with 34
eco_centroids[eco_centroids$Label == 5,'y'] <- eco_centroids[eco_centroids$Label == 5,'y']-1
eco_centroids[eco_centroids$Label == 10,'y'] <- eco_centroids[eco_centroids$Label == 10,'y']+1.5
eco_centroids[eco_centroids$Label == 7,'y'] <- eco_centroids[eco_centroids$Label == 7,'y']-1

# create save ecoregion label dataframe to add to manuscript
eco_df <- eco_centroids %>%
  st_drop_geometry() %>%
  select(Label, Ecoregion)

write.csv(eco_df, file.path(dir, 'Ecoregion_label_table.csv'))

# presence points
pres <- st_read(file.path(dir, 'vedu_presence.shp'))

# clipped mcp
mcp <- st_read(file.path(dir, 'vedu_mcp_50k.shp')) %>%
  st_intersection(states) %>%
  st_combine() 

# nlcd
nlcd <- rast(file.path(dir, 'NLCD_usWest.tif')) %>% 
  as.data.frame(xy = T, na.rm = T)

# legend for ncld layer
nlcd_legend <- pal_nlcd() %>%
  filter(code %in% unique(nlcd$NLCD_usWest)) %>%
  mutate(class = if_else(class == 'planted', 'Cultivated', str_to_title(class)),
         class_color = case_when(class == 'Water' ~ '#476BA0',
                                 class == 'Developed' ~ '#AA0000',
                                 class == 'Barren' ~ '#B2ADA3',
                                 class == 'Forest' ~ '#1C6330',
                                 class == 'Shrubland' ~ '#CCBA7C',
                                 class == 'Herbaceous' ~ '#E2E2C1',
                                 class == 'Cultivated' ~ '#AA7028',
                                 class == 'Wetlands' ~ '#BAD8EA'))

###############################################################################
# plots
###############################################################################

#### study area plot ####
nlcd_map <- ggplot() +
  geom_tile(data = nlcd, aes(x = x, y = y, 
                             fill = factor(NLCD_usWest,
                                           levels = as.numeric(nlcd_legend$code),
                                           labels = nlcd_legend$class),
                             color = factor(NLCD_usWest,
                                           levels = as.numeric(nlcd_legend$code),
                                           labels = nlcd_legend$class))) +
  scale_fill_manual(values = unique(nlcd_legend$class_color)) +
  scale_color_manual(values = unique(nlcd_legend$class_color)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  scale_x_continuous(expand = c(0.005,0.01)) +
  geom_sf(data = ecoreg, fill = NA, color = 'grey15', size = 0.25) +
  geom_sf(data = mcp, fill = 'white', color = NA, alpha = 0.45) +
  geom_sf(data = pres, fill = NA, color = 'black', size = 0.1) +
  geom_label(data = eco_centroids,
             aes(x = x, y = y, label = Label),
             size = 2.5,
             fill = alpha('grey90', 0.9),
             label.size = NA,
             label.padding = unit(0.1, "lines")) +
  labs(x = '', y = '', fill = '') +
  theme(panel.background = element_rect(color = 'white', 
                                        fill = 'white'),
        panel.grid.major = element_line(color = 'grey50', 
                                        size = 0.15, 
                                        linetype = 'dashed'), 
        axis.ticks = element_blank(), 
        legend.text = element_text(size = 8),
        legend.key.size = unit(11, 'points'),
        plot.margin = margin(1,-2,-3,-2)) +
    guides(color = 'none')

ggsave(filename = 'StudyArea_nlcd.tiff',
       path = fig_dir,
       plot = nlcd_map,
       scale = 1.2,
       width = 4,
       height = 3.5,
       units = 'in',
       dpi = 1200,
       compression = 'lzw',
       bg = 'white')
