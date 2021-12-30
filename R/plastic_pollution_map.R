# Load packages
library(sdmpredictors)
library(leaflet)
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggnewscale)
library(sf)

mytheme <- function() {
  theme_bw() + 
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 0.8), # Border around plotting area.
          panel.grid.major = element_blank(), # Major grid lines blank
          panel.grid.minor = element_blank(), # Minor grid lines blank
          axis.line = element_blank(), # axis line size
          axis.ticks = element_line(colour = "black", size = 0.8),
          axis.text = element_text(size = 10, colour = "black"), # axis text size
          axis.title = element_text(size = 10), #axis title size
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent", color = NA)) # get rid of legend panel bg)
} # set up plot theme

# Set directory
setwd('E:/Projects/Plastic pollution project - USYD/Manuscript - Energetic cost of growth/Proceedings B/Github')

## SEA SURFACE TEMPERATURE MAP ##--------------------------------------------------------------------
# Data from Bio-ORACLE v2.1
# Download current mean surface temp at resolution 0.25 arcmin (2000-2014)
surf_temp_raw <- sdmpredictors::load_layers("BO21_tempmean_ss")

# Future scenario 2040-2050 and 2090-2100 RCP8.5 surface temperature
temp_2050_raw <- raster::raster("2050AOGCM.RCP85.Surface.Temperature.Mean.tif")
temp_2100_raw <- raster::raster("2100AOGCM.RCP85.Surface.Temperature.Mean.tif")

# Crop raster to remove Antarctica
ext       <- raster::extent(c(xmin = -180, xmax = 180, ymin = -60, ymax = 62))
surf_temp <- crop(surf_temp_raw, ext)
temp_2050 <- crop(temp_2050_raw, ext)
temp_2100 <- crop(temp_2100_raw, ext)

# Change raster resolution
surf_temp <- aggregate(surf_temp, fact = 1/res(surf_temp)) # Change resolution
temp_2050 <- aggregate(temp_2050, fact = 1/res(temp_2050)) # Change resolution
temp_2100 <- aggregate(temp_2100, fact = 1/res(temp_2100)) # Change resolution

# Convert raster to df
surf_temp_df <- as.data.frame(raster::rasterToPoints(surf_temp))
temp_2050_df <- as.data.frame(raster::rasterToPoints(temp_2050))
temp_2100_df <- as.data.frame(raster::rasterToPoints(temp_2100))

# Rename raster values and categorise
surf_temp_df <- surf_temp_df %>% 
  dplyr::rename(mean_ss_temp = BO21_tempmean_ss) %>% 
  dplyr::mutate(category = dplyr::case_when(
    mean_ss_temp >= 30                     ~ '>30°C',                       
    mean_ss_temp >= 25 & mean_ss_temp < 30 ~ '25-30°C', 
    mean_ss_temp >= 20 & mean_ss_temp < 25 ~ '20-25°C',   
    mean_ss_temp > 10  & mean_ss_temp < 20 ~ '>10-20°C',     
    mean_ss_temp <= 10                     ~ '<10°C')) %>% 
  dplyr::mutate(category = factor(category,
                                  levels = c('>30°C', '25-30°C', '20-25°C', '>10-20°C', '<10°C'),
                                  ordered = TRUE))
temp_2050_df <- temp_2050_df %>% 
  dplyr::rename(mean_ss_temp = X2050AOGCM.RCP85.Surface.Temperature.Mean) %>% 
  dplyr::mutate(category = dplyr::case_when(
    mean_ss_temp >= 30                     ~ '>30°C',                       
    mean_ss_temp >= 25 & mean_ss_temp < 30 ~ '25-30°C', 
    mean_ss_temp >= 20 & mean_ss_temp < 25 ~ '20-25°C',   
    mean_ss_temp > 10  & mean_ss_temp < 20 ~ '>10-20°C',     
    mean_ss_temp <= 10                     ~ '<10°C')) %>% 
  dplyr::mutate(category = factor(category,
                                  levels = c('>30°C', '25-30°C', '20-25°C', '>10-20°C', '<10°C'),
                                  ordered = TRUE))
temp_2100_df <- temp_2100_df %>% 
  dplyr::rename(mean_ss_temp = X2100AOGCM.RCP85.Surface.Temperature.Mean) %>% 
  dplyr::mutate(category = dplyr::case_when(
    mean_ss_temp >= 30                     ~ '>30°C',                       
    mean_ss_temp >= 25 & mean_ss_temp < 30 ~ '25-30°C', 
    mean_ss_temp >= 20 & mean_ss_temp < 25 ~ '20-25°C',   
    mean_ss_temp > 10  & mean_ss_temp < 20 ~ '>10-20°C',     
    mean_ss_temp <= 10                     ~ '<10°C')) %>% 
  dplyr::mutate(category = factor(category,
                                  levels = c('>30°C', '25-30°C', '20-25°C', '>10-20°C', '<10°C'),
                                  ordered = TRUE))

## PLASTIC POLLUTION MAP ##-------------------------------------------------------
# Plastic input data from Lebreton et al 2015 (https://www.nature.com/articles/ncomms15611)
plastic <- rgdal::readOGR("PlasticRiverInput/PlasticRiverInputs.shp")

# convert spatial data to dataframe and extract plastic input into ocean from rivers (tonne/year)
plastic_df <- data.frame(plastic)
plastic_df <- plastic_df %>% 
  dplyr::select(coords.x1, coords.x2, i_high) %>% 
  dplyr::rename(x = coords.x1,
                y = coords.x2,
                input_t_y = i_high) 

# Dataframe to raster res = 1
plastic_extent <- raster::extent(surf_temp) # match extent with sea surface temp
plastic_raster <- raster::raster(plastic_extent, ncol = 500, nrow = 500, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", res = 1)
plastic_raster <- raster::rasterize(plastic_df[,1:2], plastic_raster, plastic_df[,3], fun = max)
plastic_df_2   <- as.data.frame(raster::rasterToPoints(plastic_raster))

# Categories
plastic_df_2 <- plastic_df_2 %>% 
  dplyr::mutate(category = dplyr::case_when(
    is.infinite(layer)          ~ '>1,000',
    layer >= 1000               ~ '>1,000',                       
    layer >= 100 & layer < 1000 ~ '100-1,000', 
    layer >= 1   & layer < 100  ~ '1-100',   
    layer > 0    & layer < 1    ~ '>0-1',     
    layer <= 0                  ~ '0')) %>% 
  dplyr::mutate(category = factor(category,
                           levels = c('>1,000', '100-1,000', '1-100', '>0-1', '0'),
                           ordered = TRUE))

## MERGE TEMP and POLLUTION ##-------------------------------------------
# Merge surface temp with plastic pollution
grouped_df <- plastic_df_2 %>% 
  dplyr::right_join(surf_temp_df, by = c("x" ,"y")) %>%
  dplyr::right_join(temp_2050_df, by = c("x" ,"y")) %>%
  dplyr::right_join(temp_2100_df, by = c("x" ,"y")) %>%
  dplyr::rename(plastic_level       = layer,
                ss_temp_current     = mean_ss_temp.x,
                ss_temp_2050        = mean_ss_temp.y,
                ss_temp_2100        = mean_ss_temp,
                plastic_cat         = category.x,
                ss_temp_current_cat = category.y,
                ss_temp_2050_cat    = category.x.x,
                ss_temp_2100_cat    = category.y.y)

# Categorise risk factor
grouped_df <- grouped_df %>% 
  dplyr::mutate(risk_current = dplyr::case_when(
    ss_temp_current >= 30                        & plastic_level >= 100                     ~ 'High risk (>30°C + >100 t/yr)', 
    ss_temp_current >= 30                        & plastic_level >= 1 & plastic_level < 100 ~ 'Medium risk (25-30°C + >1 t/yr)',
    ss_temp_current >= 30                        & plastic_level > 0 & plastic_level < 1    ~ 'Low risk (<1 t/yr)', 
    ss_temp_current >= 30                        & plastic_level == 0                       ~ 'No risk (0 t/yr)',
    ss_temp_current >= 25 & ss_temp_current < 30 & plastic_level >= 100                     ~ 'Medium risk (25-30°C + >1 t/yr)', 
    ss_temp_current >= 25 & ss_temp_current < 30 & plastic_level >= 1 & plastic_level < 100 ~ 'Low risk (<1 t/yr)',
    ss_temp_current >= 25 & ss_temp_current < 30 & plastic_level > 0 & plastic_level < 1    ~ 'Low risk (<1 t/yr)', 
    ss_temp_current >= 25 & ss_temp_current < 30 & plastic_level == 0                       ~ 'No risk (0 t/yr)',
    ss_temp_current < 25                         & plastic_level >= 100                     ~ 'Medium risk (25-30°C + >1 t/yr)', 
    ss_temp_current < 25                         & plastic_level >= 1 & plastic_level < 100 ~ 'Low risk (<1 t/yr',
    ss_temp_current < 25                         & plastic_level > 0 & plastic_level < 1    ~ 'Low risk (<1 t/yr)', 
    ss_temp_current < 25                         & plastic_level == 0                       ~ 'No risk (0 t/yr)')) %>% 
  dplyr::mutate(risk_2050 = dplyr::case_when(
    ss_temp_2050 >= 30                     & plastic_level >= 100                     ~ 'High risk (>30°C + >100 t/yr)', 
    ss_temp_2050 >= 30                     & plastic_level >= 1 & plastic_level < 100 ~ 'Medium risk (25-30°C + >1 t/yr)',
    ss_temp_2050 >= 30                     & plastic_level > 0 & plastic_level < 1    ~ 'Low risk (<1 t/yr)', 
    ss_temp_2050 >= 30                     & plastic_level == 0                       ~ 'No risk (0 t/yr)',
    ss_temp_2050 >= 25 & ss_temp_2050 < 30 & plastic_level >= 100                     ~ 'Medium risk (25-30°C + >1 t/yr)', 
    ss_temp_2050 >= 25 & ss_temp_2050 < 30 & plastic_level >= 1 & plastic_level < 100 ~ 'Low risk (<1 t/yr)',
    ss_temp_2050 >= 25 & ss_temp_2050 < 30 & plastic_level > 0 & plastic_level < 1    ~ 'Low risk (<1 t/yr)', 
    ss_temp_2050 >= 25 & ss_temp_2050 < 30 & plastic_level == 0                       ~ 'No risk (0 t/yr)',
    ss_temp_2050 < 25                      & plastic_level >= 100                     ~ 'Medium risk (25-30°C + >1 t/yr)', 
    ss_temp_2050 < 25                      & plastic_level >= 1 & plastic_level < 100 ~ 'Low risk (<1 t/yr',
    ss_temp_2050 < 25                      & plastic_level > 0 & plastic_level < 1    ~ 'Low risk (<1 t/yr)', 
    ss_temp_2050 < 25                      & plastic_level == 0                       ~ 'No risk (0 t/yr)')) %>% 
  dplyr::mutate(risk_2100 = dplyr::case_when(
    ss_temp_2100 >= 30                     & plastic_level >= 100                     ~ 'High risk (>30°C + >100 t/yr)', 
    ss_temp_2100 >= 30                     & plastic_level >= 1 & plastic_level < 100 ~ 'Medium risk (25-30°C + >1 t/yr)',
    ss_temp_2100 >= 30                     & plastic_level > 0 & plastic_level < 1    ~ 'Low risk (<1 t/yr)', 
    ss_temp_2100 >= 30                     & plastic_level == 0                       ~ 'No risk (0 t/yr)',
    ss_temp_2100 >= 25 & ss_temp_2100 < 30 & plastic_level >= 100                     ~ 'Medium risk (25-30°C + >1 t/yr)', 
    ss_temp_2100 >= 25 & ss_temp_2100 < 30 & plastic_level >= 1 & plastic_level < 100 ~ 'Low risk (<1 t/yr)',
    ss_temp_2100 >= 25 & ss_temp_2100 < 30 & plastic_level > 0 & plastic_level < 1    ~ 'Low risk (<1 t/yr)', 
    ss_temp_2100 >= 25 & ss_temp_2100 < 30 & plastic_level == 0                       ~ 'No risk (0 t/yr)',
    ss_temp_2100 < 25                      & plastic_level >= 100                     ~ 'Medium risk (25-30°C + >1 t/yr)', 
    ss_temp_2100 < 25                      & plastic_level >= 1 & plastic_level < 100 ~ 'Low risk (<1 t/yr)',
    ss_temp_2100 < 25                      & plastic_level > 0 & plastic_level < 1    ~ 'Low risk (<1 t/yr)', 
    ss_temp_2100 < 25                      & plastic_level == 0                       ~ 'No risk (0 t/yr)')) %>% 
  dplyr::mutate(risk_current = factor(risk_current,
                                  levels = c('No risk (0 t/yr)', 'Low risk (<1 t/yr)',
                                             'Medium risk (25-30°C + >1 t/yr)', 'High risk (>30°C + >100 t/yr)'),
                                  ordered = TRUE)) %>% 
  dplyr::mutate(risk_2050 = factor(risk_2050,
                                   levels = c('No risk (0 t/yr)', 'Low risk (<1 t/yr)',
                                              'Medium risk (25-30°C + >1 t/yr)', 'High risk (>30°C + >100 t/yr)'),
                                   ordered = TRUE)) %>% 
  dplyr::mutate(risk_2100 = factor(risk_2100,
                                   levels = c('No risk (0 t/yr)', 'Low risk (<1 t/yr)',
                                              'Medium risk (25-30°C + >1 t/yr)', 'High risk (>30°C + >100 t/yr)'),
                                   ordered = TRUE))

## FISHERIES ## ------------------------------------------------------------------
# Catch data from Watson 2017 (https://www.nature.com/articles/sdata201739#Sec20)
catch_data   <- read.csv("CatchInd2015_2015.csv")
code         <- read.csv("Codes.csv")
catch_data$x <-  code$LonCentre[match(catch_data$Cell, code$Cell)]
catch_data$y <-  code$LatCentre[match(catch_data$Cell, code$Cell)]
catch_data$total_t_km <- (catch_data$Reported + catch_data$IUU)
names(catch_data)

# Dataframe to raster and categorise, res = 1
catch_extent <- raster::extent(surf_temp) # match extent with sea surface temp
catch_raster <- raster::raster(catch_extent, ncol = 500, nrow = 500, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", res = 1)
catch_raster <- raster::rasterize(catch_data[,6:7], catch_raster, catch_data[,8], fun = mean)
catch_df     <- as.data.frame(raster::rasterToPoints(catch_raster))
catch_df     <- catch_df %>% 
  dplyr::mutate(category = dplyr::case_when(
    layer >= 100 ~ '>100',                       
    layer >= 10 & layer < 100 ~ '10-100', 
    layer >= 1  & layer < 10  ~ '1-10',   
    layer > 0.1 & layer < 1   ~ '0.1-1',     
    layer <= 0.1              ~ '<0.1')) %>% 
  dplyr::mutate(category = factor(category,
                                  levels = c('>100', '10-100', '1-10', '0.1-1', '<0.1'),
                                  ordered = TRUE))

# Plot risk over fishing intensity
catch_grey <- c('#666666', '#969696', '#bfbfbf', '#dbdbdb', '#f0f0f0')
risk_col_1 <- c('#2D90A5', '#fecc5c', '#fd8d3c')
risk_col_2 <- c('#2D90A5', '#fecc5c', '#fd8d3c', '#e31a1c')

risk_curr_map <- ggplot() +
  geom_raster(data = catch_df, aes(x = x, y = y, fill = category)) +
  scale_fill_manual(values = catch_grey) +
  ggnewscale::new_scale_fill() +
  geom_raster(data = grouped_df, aes(x = x, y = y, fill = risk_current, alpha = risk_current)) +
  scale_fill_manual(values = risk_col_1) +
  scale_alpha_manual(values = c(0.6, 0.7, 0.9)) +
  scale_y_continuous(limits = c(-60, 62), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-180, 180), expand = c(0, 0)) +
  ylab(NULL) + xlab(NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  mytheme() +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  coord_fixed() 

risk_2050_map <- ggplot() +
  geom_raster(data = catch_df, aes(x = x, y = y, fill = category)) +
  scale_fill_manual(values = catch_grey) +
  ggnewscale::new_scale_fill() +
  geom_raster(data = grouped_df, aes(x = x, y = y, fill = risk_2050, alpha = risk_2050)) +
  scale_fill_manual(values = risk_col_2) +
  scale_alpha_manual(values = c(0.6, 0.7, 0.9, 1)) +
  scale_y_continuous(limits = c(-60, 62), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-180, 180), expand = c(0, 0)) +
  ylab(NULL) + xlab(NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  mytheme() +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  coord_fixed() 

risk_2100_map <- ggplot() +
  geom_raster(data = catch_df, aes(x = x, y = y, fill = category)) +
  scale_fill_manual(values = catch_grey) +
  ggnewscale::new_scale_fill() +
  geom_raster(data = grouped_df, aes(x = x, y = y, fill = risk_2100, alpha = risk_2100)) +
  scale_fill_manual(values = risk_col_2) +
  scale_alpha_manual(values = c(0.6, 0.7, 0.9, 1)) +
  scale_y_continuous(limits = c(-60, 62), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-180, 180), expand = c(0, 0)) +
  ylab(NULL) + xlab(NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  mytheme() +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  coord_fixed() 

# Calculate grid cells occupied for current climate
risk_current_sum <- grouped_df %>% 
  dplyr::group_by(risk_current) %>% 
  dplyr::summarise(n = length(risk_current)) %>% 
  tibble::add_row(risk_current = "High risk (>30°C + >100 t/yr)", n = 0) %>%
  dplyr::arrange(risk_current = factor(risk_current,
                                       levels = c('No risk (0 t/yr)', 'Low risk (<1 t/yr)',
                                                  'Medium risk (25-30°C + >1 t/yr)', 'High risk (>30°C + >100 t/yr)'),
                                       ordered = TRUE))

# Calculate grid cells occupied for future climate
risk_2050_sum <- grouped_df %>% 
  dplyr::group_by(risk_2050) %>% 
  dplyr::summarise(n = length(risk_2050))

risk_2100_sum <- grouped_df %>% 
  dplyr::group_by(risk_2100) %>% 
  dplyr::summarise(n = length(risk_2100))

# calculate change with future scenario
risk_current_sum$diff_2050 <- risk_2050_sum$n - risk_current_sum$n
risk_current_sum$diff_2100 <- risk_2100_sum$n - risk_current_sum$n
risk_current_sum           <- reshape2::melt(risk_current_sum, id.vars = c("risk_current", "n")) %>% tidyr::drop_na()

change_plot <- risk_current_sum %>% 
  ggplot(aes(x = risk_current, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("#79ABE2", "#00366C")) +
  ylab("Change in risk from current scenario") + xlab(NULL) +
  mytheme()

#COMBINE PLOTS
cowplot::plot_grid(risk_curr_map + ggtitle("Current scenario (2000-2014)"), 
                   risk_2050_map + ggtitle("Future scenario (2040-2050)"), 
                   risk_2100_map + ggtitle("Future scenario (2090-2100)"),
                   change_plot,
                   ncol = 1)

# Merge group data with fisheries data
all_df <- grouped_df %>% 
  dplyr::right_join(catch_df, by = c("x" ,"y")) %>%
  dplyr::rename(total_kg_km = layer,
                catch_cat   = category)
names(all_df)
levels(all_df$risk_2050)

# GENERAL STAN SPECS
rstan::rstan_options(auto_write = TRUE) # translate to STAN platform for running Bayesian model
options(mc.cores = parallel::detectCores()) # detects how many cores available to use

all_df$lnPlastic <-  log(all_df$plastic_level + 1)
all_df$lnCatch   <-  log(all_df$total_kg_km + 1)

catch_prior  <- prior(normal(0, 5), class = "Intercept") + prior(normal(0, 5), class = "b")
catch_model <- brms::brm(bf(lnCatch ~ lnPlastic + ss_temp_current, sigma ~ lnCatch), 
                         data    = all_df,
                         family  = gaussian,
                         prior   = catch_prior,
                         chains  = 4, 
                         cores   = 4, 
                         iter    = 3e4, 
                         warmup  = 1.5e4,
                         control = list(adapt_delta = 0.99, max_treedepth = 15))

brms::pp_check(catch_model)
summary(catch_model)
brms::fixef(catch_model)
plot(conditional_effects(catch_model), points = TRUE)

catch_me <- marginal_effects(catch_model, "lnPlastic")
catchme_df <- catch_me$'lnPlastic'

all_df %>% 
  ggplot(aes(x = lnPlastic, y = lnCatch)) +
  geom_point(aes(colour = risk_2050, alpha = risk_2050), size = 2) +
  geom_ribbon(data = catchme_df, aes(ymin = lower__, ymax = upper__), fill = "#e31a1c", alpha = 0.5) +
  geom_line(data = catchme_df, aes(x = lnPlastic, y = estimate__), size = 1) +
  scale_colour_manual(values = risk_col_2) +
  scale_alpha_manual(values = c(0.2, 0.4, 0.8, 1)) +
  xlab(expression("log Plastic pollution (t yr"^"-1"*")")) +
  ylab(expression("log Fishery intensity (t km"^"-2"*" yr"^"-1"*")")) +
  mytheme()

# EEZ map
eez_shp <- sf::st_read("SAUEEZ_July2015.shp")
eez_sp  <- as(eez_shp, "Spatial") # convert to sp class

names(grouped_df)
grouped_df <- grouped_df %>%
  dplyr::mutate(rank_current = as.numeric(factor(risk_current)) - 1,
                rank_2050    = as.numeric(factor(risk_2050)) - 1,
                rank_2100    = as.numeric(factor(risk_2100)) - 1)


map_extent       <- raster::extent(surf_temp) # match extent with sea surface temp
rank_raster      <- raster::raster(map_extent, ncol = 500, nrow = 500, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", res = 1)
rank_current_ras <- raster::rasterize(grouped_df[,1:2], rank_raster, grouped_df[,14], fun = mean)
rank_2050_ras    <- raster::rasterize(grouped_df[,1:2], rank_raster, grouped_df[,15], fun = mean)
rank_2100_ras    <- raster::rasterize(grouped_df[,1:2], rank_raster, grouped_df[,16], fun = mean)

# Extract raster values to list object and calculate mean
rank_current_mean <- raster::extract(rank_current_ras, eez_shp, fun = mean, na.rm = TRUE)
rank_2050_mean    <- raster::extract(rank_2050_ras, eez_shp, fun = mean, na.rm = TRUE)
rank_2100_mean    <- raster::extract(rank_2100_ras, eez_shp, fun = mean, na.rm = TRUE)

# Join mean values to polygon data
eez_df <- data.frame(eez_shp, rank_current = rank_current_mean,
                     rank_2050 = rank_2050_mean,
                     rank_2100 = rank_2100_mean)
nc_df <- eez_df %>% 
  as.data.frame() %>% 
  select(Name, rank_current, rank_2050,rank_2100)

eez_merged <- merge(eez_shp, nc_df, by = "Name")

eez_curr_plot <- ggplot() +
  geom_sf(data = eez_merged, aes(fill = rank_current), colour = NA, show.legend = FALSE) +
  colorspace::scale_fill_continuous_sequential(palette = "YlOrRd", na.value = NA, begin = 0, end = 0.7) +
  scale_y_continuous(limits = c(-60, 62), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-180, 180), expand = c(0, 0)) +
  ylab(NULL) + xlab(NULL) +
  mytheme() +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

eez_2050_plot <- ggplot() +
  geom_sf(data = eez_merged, aes(fill = rank_2050), colour = NA, show.legend = FALSE) +
  colorspace::scale_fill_continuous_sequential(palette = "YlOrRd", na.value = NA, begin = 0, end = 1) +
  scale_y_continuous(limits = c(-60, 62), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-180, 180), expand = c(0, 0)) +
  ylab(NULL) + xlab(NULL) +
  mytheme() +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

eez_2100_plot <- ggplot() +
  geom_sf(data = eez_merged, aes(fill = rank_2100), colour = NA, show.legend = FALSE) +
  colorspace::scale_fill_continuous_sequential(palette = "YlOrRd", na.value = NA, begin = 0, end = 1) +
  scale_y_continuous(limits = c(-60, 62), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-180, 180), expand = c(0, 0)) +
  ylab(NULL) + xlab(NULL) +
  mytheme() +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

cowplot::plot_grid(eez_curr_plot + ggtitle("Current scenario (2000-2014)"), 
                   eez_2050_plot + ggtitle("Future scenario (2040-2050)"), 
                   eez_2100_plot + ggtitle("Future scenario (2090-2100)"),
                   ncol = 2)

eez_df %>%
  arrange(desc(rank_current)) %>%
  slice(1:40) %>%
  ggplot() +
  geom_segment(aes(x = reorder(Name, rank_current), xend = reorder(Name, rank_2050), y = rank_current, yend = rank_2050), linetype = "dashed", alpha = 0.5) +
  geom_point(aes(x = reorder(Name, rank_current), y = rank_current, colour = rank_current), size = 2) +
  geom_point(aes(x = reorder(Name, rank_current), y = rank_2050, colour = rank_2050), size = 2, shape = 17) +
  ylab("Risk score (arbitrary)") + xlab(NULL) +
  colorspace::scale_colour_continuous_sequential(palette = "YlOrRd", begin = 0.5, end = 1) +
  coord_flip() +
  ggtitle("Change in risk for Exclusive Economic Zone by 2040-2050") +
  mytheme()

