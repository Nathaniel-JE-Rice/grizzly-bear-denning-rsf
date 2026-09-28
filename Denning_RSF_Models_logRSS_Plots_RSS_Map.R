# Denning Models and Predictions -- RSS take 2 #

# Load packages ####

library(lme4)
library(tidyverse)
library(sf)
library(terra)
library(raster)
library(cowplot)
library(officer)
library(scales)
library(dplyr)
library(ggspatial)
library(grid)

# Load data ####

denning_data <- read.csv("data/denning_rsf_data.csv")

head(denning_data)

# Check for missing values in the relevant columns
missing_data <- denning_data %>%
  filter(is.na(elev_sc) | is.na(slope_sc) | is.na(sin_aspect) | is.na(cos_aspect) | is.na(distance_to_road_log))

# View the rows with missing values
nrow(missing_data)

# Remove missing values
denning_data <- denning_data %>%
  filter(!is.na(elev_sc) & !is.na(slope_sc) & !is.na(sin_aspect) & !is.na(cos_aspect) & !is.na(distance_to_road_log))

# Null Model

null_model = glm(used ~ 1, family = "binomial", data=denning_data)
summary(null_model)



# Model 3  

mod3 = glm(used ~ elev_sc + I(elev_sc^2) + 
             slope_sc + I(slope_sc^2) +
             sin_aspect + cos_aspect + distance_to_road_log,
           family="binomial",data=denning_data)

summary(mod3)

################################################################################
#### Model predictions - Lowest Elevation Den ####
################################################################################

# Lowest elevation den reference sc values #

lw_elev_ref <- denning_data %>% 
  filter(used == 1) %>% 
  filter(elev == min(.$elev, na.rm = TRUE))

## Elevation ####

pred_elev <- data.frame(elev_sc = seq(min(denning_data$elev_sc, na.rm = TRUE),
                                      max(denning_data$elev_sc, na.rm = TRUE),
                                      length.out = 100),
                        slope_sc = lw_elev_ref$slope_sc,
                        sin_aspect = lw_elev_ref$sin_aspect, # sin = 0 and cos = 1 means North
                        cos_aspect = lw_elev_ref$cos_aspect,
                        distance_to_road_log = lw_elev_ref$distance_to_road_log)

pred_ref <- data.frame(elev_sc = lw_elev_ref$elev_sc,
                       slope_sc = lw_elev_ref$slope_sc,
                       sin_aspect = lw_elev_ref$sin_aspect, 
                       cos_aspect = lw_elev_ref$cos_aspect,
                       distance_to_road_log = lw_elev_ref$distance_to_road_log)

pred_elev_num <- predict(mod3, newdata = pred_elev, type = "response")
pred_elev_den <- predict(mod3, newdata = pred_ref, type = "response")
pred_elev$log_rss <- log(pred_elev_num/pred_elev_den)

ggplot(pred_elev, aes(x = elev_sc, y = log_rss)) +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dashed")


## Aspect ####

pred_asp <- data.frame(elev_sc = lw_elev_ref$elev_sc,
                       slope_sc = lw_elev_ref$slope_sc,
                       aspect = seq(0, 2*pi, length.out = 100)) %>% 
  mutate(sin_aspect = sin(aspect),
         cos_aspect = cos(aspect),
         distance_to_road_log = lw_elev_ref$distance_to_road_log)

pred_asp_num <- predict(mod3, newdata = pred_asp, type = "response")
pred_asp_den <- predict(mod3, newdata = pred_ref, type = "response")
pred_asp$log_rss <- log(pred_asp_num/pred_asp_den)

ggplot(pred_asp, aes(x = aspect, y = log_rss)) +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dashed")


## Slope ####

pred_slope <- data.frame(elev_sc = lw_elev_ref$elev_sc,
                         slope_sc = seq(min(denning_data$slope_sc, na.rm = TRUE),
                                        max(denning_data$slope_sc, na.rm = TRUE),
                                        length.out = 100),
                         sin_aspect = lw_elev_ref$sin_aspect, 
                         cos_aspect = lw_elev_ref$cos_aspect,
                         distance_to_road_log = lw_elev_ref$distance_to_road_log)

pred_slope_num <- predict(mod3, newdata = pred_slope, type = "response")
pred_slope_den <- predict(mod3, newdata = pred_ref, type = "response")
pred_slope$log_rss <- log(pred_slope_num/pred_slope_den)

ggplot(pred_slope, aes(x = slope_sc, y = log_rss)) +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dashed")


## Distance to Road ####

pred_distance_to_road_log <- data.frame(elev_sc = lw_elev_ref$elev_sc,
                                        slope_sc = lw_elev_ref$slope_sc,
                                        sin_aspect = lw_elev_ref$sin_aspect, # sin = 0 and cos = 1 means North
                                        cos_aspect = lw_elev_ref$cos_aspect,
                                        distance_to_road_log = seq(min(denning_data$distance_to_road_log, na.rm = TRUE),
                                                                   max(denning_data$distance_to_road_log, na.rm = TRUE),
                                                                   length.out = 100))

pred_distance_to_road_num <- predict(mod3, newdata = pred_distance_to_road_log, type = "response")
pred_distance_to_road_den <- predict(mod3, newdata = pred_ref, type = "response")
pred_distance_to_road_log$log_rss <- log(pred_distance_to_road_num/pred_distance_to_road_den)

ggplot(pred_distance_to_road_log, aes(x = distance_to_road_log, y = log_rss)) +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dashed")


# Unscale and uncenter ####

# Improved Road Layer  
means <- readRDS("data/covariate_means.rds")
sds <- readRDS("data/covariate_sds.rds")

# Elevation

pred_elev$elev <- pred_elev$elev_sc * sds["elev"] + means["elev"]

ggplot(pred_elev, aes(x = elev, y = log_rss)) +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dashed")


# Get max logRSS value at top of curve

quad_model <- lm(log_rss ~ poly(elev, 2, raw = TRUE), data = pred_elev)
a <- coef(quad_model)[3]  
b <- coef(quad_model)[2]
elev_peak <- -b / (2 * a)
print(elev_peak)


# Slope

pred_slope$slope <- pred_slope$slope_sc * sds["slope"] + means["slope"]

ggplot(pred_slope, aes(x = slope, y = log_rss)) +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dashed")


# Get max logRSS value at top of curve

quad_model_slope <- lm(log_rss ~ poly(slope, 2, raw = TRUE), data = pred_slope)
a_slope <- coef(quad_model_slope)[3]  
b_slope <- coef(quad_model_slope)[2]  
slope_peak <- -b_slope / (2 * a_slope)
print(slope_peak)

# Distance to Roads

pred_distance_to_road_log$distance_to_road_log_bt <- (pred_distance_to_road_log$distance_to_road_log) * sds["distance_to_road_log"] + means["distance_to_road_log"]

pred_distance_to_road_log$distance_to_road_bt <- exp(pred_distance_to_road_log$distance_to_road_log_bt - 1)

ggplot(pred_distance_to_road_log, aes(x = distance_to_road_bt, y = log_rss)) +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dashed")


# LogRSS value at 90% of the curve

quad_model_dist <- lm(log_rss ~ poly(distance_to_road_bt, 2, raw = TRUE), data = pred_distance_to_road_log)
new_data <- data.frame(distance_to_road_bt = seq(min(pred_distance_to_road_log$distance_to_road_bt), 
                                                 max(pred_distance_to_road_log$distance_to_road_bt), 
                                                 length.out = 1000))

new_data$log_rss_pred <- predict(quad_model_dist, newdata = new_data)
min_log_rss <- min(new_data$log_rss_pred)
max_log_rss <- max(new_data$log_rss_pred)

threshold_90 <- min_log_rss + 0.9 * (max_log_rss - min_log_rss)
dist_90 <- new_data$distance_to_road_bt[which.min(abs(new_data$log_rss_pred - threshold_90))]
print(dist_90)

#### Confidence Intervals

pred_log_rss <- function(mod, x1, x2) {
  
  pred_X <- model.matrix(mod, data = x1)
  ref_X <- model.matrix(mod, data = x2)
  B <- coef(mod)
  
  # Difference
  diff_X <- pred_X - ref_X
  
  # Linear predictor for difference
  diff_dist <- unname((diff_X %*% B)[, 1])
  
  # Get variance of difference
  var_pred <- diag(diff_X %*% vcov(mod) %*% t(diff_X))
  
  # Standard error for difference
  SE <- unname(sqrt(var_pred))
  
  out <- x1 %>%
    mutate(
      log_rss = diff_dist,
      se = SE,
      lwr = log_rss - 1.96 * se,
      upr = log_rss + 1.96 * se
    )
  
  return(out)
  
}

pred_ref_new <- data.frame()
for (i in 1:100) {
  pred_ref_new <- rbind(pred_ref_new, pred_ref)
}

# Elevation
pred_elev_ci <- pred_elev
pred_elev$used <- 1
pred_ref_new$used <- 1
pred_elev_ci <- pred_log_rss(mod = mod3,x1 = pred_elev,x2 = pred_ref_new)

# Back Transform
pred_elev_ci$elev <- pred_elev_ci$elev_sc * sds["elev"] + means["elev"]

# Plot
plot1 <- ggplot(pred_elev_ci, aes(x = elev, y = log_rss)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "gray70") +
  geom_line() +
  labs(x = "Elevation (m)", y = "log-RSS") +
  geom_hline(yintercept = 0, lty = "dashed") +
  theme_minimal() 

# Slope
pred_slope_ci <- pred_slope
pred_slope$used <- 1
pred_ref_new$used <- 1
pred_slope_ci <- pred_log_rss(mod = mod3,x1 = pred_slope,x2 = pred_ref_new)

# Back Transform
pred_slope_ci$slope <- pred_slope_ci$slope_sc * sds["slope"] + means["slope"]

# Plot
plot2 <- ggplot(pred_slope_ci, aes(x = slope, y = log_rss)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "gray70") +
  geom_line() +
  labs(x = "Slope (degrees)", y = "log-RSS") +
  geom_hline(yintercept = 0, lty = "dashed") +
  theme_minimal() 

# Aspect
pred_asp_ci <- pred_asp
pred_asp$used <- 1
pred_ref_new$used <- 1
pred_asp_ci <- pred_log_rss(mod = mod3,x1 = pred_asp,x2 = pred_ref_new)

# Plot
plot3 <- ggplot(pred_asp_ci, aes(x = aspect, y = log_rss)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "gray70") +
  geom_line() +
  labs(x = "Aspect (radians)", y = "log-RSS") +
  geom_hline(yintercept = 0, lty = "dashed") +
  theme_minimal() 

# Distance to Roads
pred_distance_to_road_log_ci <- pred_distance_to_road_log
pred_distance_to_road_log$used <- 1
pred_ref_new$used <- 1
pred_distance_to_road_log_ci <- pred_log_rss(mod = mod3,x1 = pred_distance_to_road_log,x2 = pred_ref_new)

# Back Transform
pred_distance_to_road_log_ci$distance_to_road_bt <- (pred_distance_to_road_log_ci$distance_to_road_log) * sds["distance_to_road_log"] + means["distance_to_road_log"]

pred_distance_to_road_log_ci$distance_to_road_bt <- exp(pred_distance_to_road_log_ci$distance_to_road_bt - 1)

# Plot
plot4 <- ggplot(pred_distance_to_road_log_ci, aes(x = distance_to_road_bt, y = log_rss)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "gray70") +
  geom_line() +
  labs(x = "Distance to Road (m)", y = "log-RSS") +
  geom_hline(yintercept = 0, lty = "dashed") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(floor(min(pred_distance_to_road_log_ci$log_rss)), 
                                  ceiling(max(pred_distance_to_road_log_ci$log_rss)), 
                                  by = 2))

# Plot all 4 together ####

combined_plot <- plot_grid(plot1, plot2, plot3, plot4, 
                           ncol = 2, 
                           labels = c("a", "b", "c", "d"))         


################################################################################
##### Map #
################################################################################

# Load RZ and Study Area

rz <- read_sf("data/recovery_zones.gpkg")

study_area <- read_sf("data/study_area.gpkg")

study_area <- st_transform(study_area, crs = 5070)
rz <- st_transform(rz, crs(study_area))



################################################################################
#### RSS Map
################################################################################

denning_raster <- rast("data/denning_raster_stack.tiff")
denning_raster <- as.data.frame(denning_raster)

denning_raster$elev_sc <- (denning_raster$elev - means["elev"]) / sds["elev"]

denning_raster$slope_sc <- (denning_raster$slope - means["slope"]) / sds["slope"]

denning_raster$distance_to_road_log <- (
  log(denning_raster$distance_to_road + 1) -
    means["distance_to_road_log"]
) / sds["distance_to_road_log"]

denning_raster$cos_aspect <- cos(denning_raster$aspect)

denning_raster$sin_aspect <- sin(denning_raster$aspect)

################################################################################
#### Predict RSS
################################################################################

pred_map_x1 <- predict(
  mod3,
  newdata = denning_raster,
  type = "response"
)

pred_map_x2 <- predict(
  mod3,
  newdata = pred_ref,
  type = "response"
)

# Relative selection strength

denning_raster$RSS <- pred_map_x1 / pred_map_x2

################################################################################
#### Create RSS raster
################################################################################

denning_raster_template_1 <- rast(
  "data/denning_raster_stack.tiff"
)[[1]]

names(denning_raster_template_1) <- "RSS"

values(denning_raster_template_1) <- denning_raster$RSS

################################################################################
#### Mask RSS values less than or equal to 1
################################################################################

denning_raster_template_1 <- ifel(
  denning_raster_template_1 > 1,
  denning_raster_template_1,
  NA
)

names(denning_raster_template_1) <- "RSS"

################################################################################
#### Get RSS values and define color limits
################################################################################

RSS_values <- values(
  denning_raster_template_1$RSS
)

RSS_values <- RSS_values[
  !is.na(RSS_values)
]

positive_min <- min(
  RSS_values,
  na.rm = TRUE
)

positive_max <- max(
  RSS_values,
  na.rm = TRUE
)

################################################################################
#### Clip map to study area
################################################################################

study_area <- st_transform(
  study_area,
  crs = 5070
)

rz <- st_transform(
  rz,
  crs(study_area)
)

denning_raster_template_1 <- crop(
  denning_raster_template_1,
  ext(study_area)
)

denning_raster_template_1 <- mask(
  denning_raster_template_1,
  vect(study_area)
)

################################################################################
#### Reproject to UTM
################################################################################

utm_crs <- "EPSG:32611"

denning_raster_template_1_utm <- project(
  denning_raster_template_1,
  utm_crs,
  method = "near"
)

names(denning_raster_template_1_utm) <- "RSS"

writeRaster(
  denning_raster_template_1_utm,
  "denning_map_1_utm.tiff",
  overwrite = TRUE
)

denning_map_1_utm <- as.data.frame(
  denning_raster_template_1_utm,
  xy = TRUE
)

study_area <- st_transform(
  study_area,
  crs = utm_crs
)

rz <- st_transform(
  rz,
  crs = utm_crs
)


################################################################################
# RSS Map 
################################################################################

################################################################################
# Calculate the 99th percentile of RSS
################################################################################

rss_99 <- quantile(
  denning_map_1_utm$RSS,
  probs = 0.99,
  na.rm = TRUE
)

################################################################################
# Create the map
################################################################################

den_map_plot <- ggplot(
  denning_map_1_utm
) +
  
  geom_raster(
    aes(
      x = x,
      y = y,
      fill = RSS
    )
  ) +
  
  geom_sf(
    data = study_area,
    fill = NA,
    color = "black",
    linewidth = 1
  ) +
  
  geom_sf(
    data = rz,
    fill = NA,
    color = "red",
    linewidth = 0.5
  ) +
  
  scale_fill_viridis_c(
    option = "cividis",
    
    limits = c(
      positive_min,
      rss_99
    ),
    
    oob = scales::squish,
    
    na.value = "white",
    
    name = "RSS"
  ) +
  
  # Label geographic coordinates
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_classic(
    base_size = 12
  ) +
  
  theme(
    legend.position = "right",
    legend.text = element_text(
      size = 10
    ),
    axis.text = element_text(
      size = 10
    ),
    axis.title = element_text(
      size = 12
    )
  ) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(0.8, "cm"),
    width = unit(0.8, "cm")
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    unit_category = "metric",
    text_cex = 0.8,
    line_width = 0.8
  ) +
  
  coord_sf(
    xlim = st_bbox(
      study_area
    )[c(
      "xmin",
      "xmax"
    )],
    
    ylim = st_bbox(
      study_area
    )[c(
      "ymin",
      "ymax"
    )],
    
    expand = FALSE
  )

################################################################################
# Display the map
################################################################################

print(
  den_map_plot
)
