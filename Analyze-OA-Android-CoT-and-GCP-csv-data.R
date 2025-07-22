# Copyright 2025 Theta Informatics LLC
# Provided under MIT license software license
# Modification and Redistribution permitted while above notice is preserved

library(dplyr)
library(ggplot2)

# Haversine function in R
haversine <- function(lon1, lat1, lon2, lat2, alt) {
  lon1 <- lon1 * pi / 180
  lat1 <- lat1 * pi / 180
  lon2 <- lon2 * pi / 180
  lat2 <- lat2 * pi / 180

  dlon <- lon2 - lon1
  dlat <- lat2 - lat1

  a <- (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))

  avg_lat <- (lat1 + lat2) / 2
  r <- radius_at_lat_lon(avg_lat, 0)
  r <- r + alt
  return(c * r)
}

radius_at_lat_lon <- function(lat, lon) {
  A <- 6378137.0
  B <- 6356752.3
  lat <- lat * pi / 180
  r <- ((A^2 * cos(lat))^2 + (B^2 * sin(lat))^2) / ((A * cos(lat))^2 + (B * sin(lat))^2)
  return(sqrt(r))
}

## Load Cursor on Target openAthenaCalculationInfo data from CSV file
cot_data <- read.csv("path/to/your/OA-CoT-Capture-file.csv")
## Load GCP data from a ZIP file containing inner CSV file
## User note: remove unzip() operation if operating directly on csv file
gcp_data <- read.csv(unzip("path/to/your/ground_control_points_from_sw_maps.zip", files = NULL))

# Find nearest GCP for each CoT capture and calculate distances
cot_data <- cot_data %>%
  mutate(
    nearest_gcp = sapply(seq_len(n()), function(i) {
      min_idx <- which.min(haversine(lon[i], lat[i], gcp_data$Longitude, gcp_data$Latitude, 0))
      return(min_idx)
    }),
    drone_to_gcp_horizontal_distance = mapply(function(i, j) {
      haversine(droneLongitude[i], droneLatitude[i], gcp_data$Longitude[j], gcp_data$Latitude[j], 0)
    }, i = seq_len(n()), j = nearest_gcp),
    drone_to_gcp_vertical_distance = abs(droneElevationHAE - gcp_data$Elevation[nearest_gcp]),
    distance_ratio =  drone_to_gcp_vertical_distance / drone_to_gcp_horizontal_distance
  )

# -----------------------------------------------------------------------------
# Calculate slant distance (hypotenuse) from drone to GCP
# -----------------------------------------------------------------------------
cot_data$drone_to_gcp_slant_range <- with(cot_data,
                                           +  sqrt(drone_to_gcp_horizontal_distance^2 +
                                                     +       drone_to_gcp_vertical_distance^2))

# Calculate circular and vertical errors
cot_data$horizontal_error <- mapply(haversine, cot_data$lon, cot_data$lat, gcp_data$Longitude[cot_data$nearest_gcp], gcp_data$Latitude[cot_data$nearest_gcp], MoreArgs = list(alt = 0))
cot_data$vertical_error <- abs(cot_data$hae - gcp_data$Elevation[cot_data$nearest_gcp])

# Calculate distance of user's selected pixel from Principal Point (center) of the image:
cot_data$pixelDistFromPrincipalPoint <- sqrt(
  (cot_data$imageWidth * (0.5 - cot_data$imageSelectedProportionX))^2 +
  (cot_data$imageLength * (0.5 - cot_data$imageSelectedProportionY))^2
)

# If only one make or model of drone is present in data, omit this
# categorical variable from regression model(s)
# Dynamically build model formula based on the data
model_components <- c("cameraSlantAngleDeg", "focalLength", "digitalZoomRatio",
                      "imageSelectedProportionX", "imageSelectedProportionY",
                      "pixelDistFromPrincipalPoint", "drone_to_gcp_horizontal_distance",
                      "drone_to_gcp_vertical_distance", "distance_ratio")
# Add 'make' and 'model' conditionally
if(length(unique(cot_data$make)) > 1) {
  cot_data$make <- factor(cot_data$make)
  model_components <- c(model_components, "make")
}
if(length(unique(cot_data$model)) > 1) {
  cot_data$model <- factor(cot_data$model)
  model_components <- c(model_components, "model")
}
horizontal_model_formula <- as.formula(paste("horizontal_error ~", paste(model_components, collapse = " + ")))
vertical_model_formula <- as.formula(paste("vertical_error ~", paste(model_components, collapse = " + ")))

# Multiple regression models incorporating the new variables
horizontal_model <- lm(horizontal_model_formula, data = cot_data)
summary(horizontal_model)

vertical_model <- lm(vertical_model_formula, data = cot_data)
summary(vertical_model)

# Calculate the slope of horizontal error by horizontal distance
# This represents how much circular error is introduced per meter of distance

# Perform a simple linear regression
slope_model <- lm(horizontal_error ~ drone_to_gcp_horizontal_distance, data = cot_data)

# Extract the slope coefficient
# simple slope model without outlier removal. Not to be used for droneModels.json!
slope <- coef(slope_model)["drone_to_gcp_horizontal_distance"]
# Print diagnostic info for simple slope model without outlier removal
# summary(slope_model)

# Extract the confidence interval for the slope
conf_interval <- confint(slope_model, "drone_to_gcp_horizontal_distance", level = 0.95)

# Print the slope with interpretation
cat(sprintf("Slope of Horizontal Error by Horizontal Distance: %.4f meters per meter\n", slope))
cat(sprintf("95%% Confidence Interval for the Slope: [%.4f, %.4f] meters per meter\n", conf_interval[1], conf_interval[2]))

# Optionally, plot the regression line and display the slope on the plot
ggplot(cot_data, aes(x = drone_to_gcp_horizontal_distance, y = horizontal_error)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  ggtitle("Horizontal Error vs. Horizontal Distance with Regression Line") +
  xlab("Drone to GCP Horizontal Distance (meters)") +
  ylab("Horizontal Error (meters)") +
  annotate("text",
           x = Inf, y = Inf,
           label = sprintf("Slope: %.4f m/m", slope),
           hjust = 1.1, vjust = 2,
           size = 5,
           color = "blue") +
  theme_minimal()

# Plotting results for diagnostics and interpretation
ggplot(cot_data, aes(x = drone_to_gcp_vertical_distance, y = horizontal_error)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Horizontal Error by Drone to GCP Vertical Distance")

ggplot(cot_data, aes(x = distance_ratio, y = horizontal_error)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Horizontal Error by Distance Ratio")

# Plot Horizontal Error by various camera settings and model characteristics
ggplot(cot_data, aes(x = cameraSlantAngleDeg, y = horizontal_error)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Horizontal Error by Camera Slant Angle")

ggplot(cot_data, aes(x = raySlantAngleDeg, y = horizontal_error)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Horizontal Error by Ray Slant Angle")

ggplot(cot_data, aes(x = cameraRollAngleDeg, y = horizontal_error)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Horizontal Error by Camera Roll Angle")

ggplot(cot_data, aes(x = pixelDistFromPrincipalPoint, y = horizontal_error)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Horizontal Error by Pixel Distance from Principal Point")

ggplot(cot_data, aes(x = digitalZoomRatio, y = horizontal_error)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Horizontal Error by DigitalZoomRatio")

ggplot(cot_data, aes(x = as.factor(model), y = horizontal_error)) +
  geom_boxplot() +
  ggtitle("Horizontal Error by Drone Model") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve label readability for models

# Calculate Circular Error Probable (CEP) and Mean Absolute Error for each drone model in data

# Calculate CEP (50th percentile of horizontal errors) in meters for each drone model
cep_by_model <- cot_data %>%
  group_by(model) %>%
  summarise(CEP = quantile(horizontal_error, 0.5))

# Print CEP by model
print(cep_by_model)

# Calculate Mean Absolute Error in meters for horizontal error for each drone model
mae_by_model <- cot_data %>%
  group_by(model) %>%
  summarise(MAE = mean(abs(horizontal_error)))

# Print MAE by model
print(mae_by_model)

# Plotting CEP by Model
ggplot(cep_by_model, aes(x = model, y = CEP)) +
  geom_col(fill = "steelblue") +
  ggtitle("Circular Error Probable (CEP) by Drone Model") +
  xlab("Model") +
  ylab("CEP (meters)") +
  theme_minimal()

# Calculate CE90% (90th percentile of horizontal errors) in meters for each drone model
ce90_by_model <- cot_data %>%
  group_by(model) %>%
  summarise(CE90 = quantile(horizontal_error, 0.9))

#Print CE90
print(ce90_by_model)

# Plotting MAE by Model
ggplot(mae_by_model, aes(x = model, y = MAE)) +
  geom_col(fill = "darkred") +
  ggtitle("Mean Absolute Error (MAE) by Drone Model") +
  xlab("Model") +
  ylab("MAE (meters)") +
  theme_minimal()

# Convert focalLength to a categorical variable if it's numeric
cot_data$focalLength_cat <- as.factor(cot_data$focalLength)

# Create a box plot of horizontal error grouped by focal length
ggplot(cot_data, aes(x = focalLength_cat, y = horizontal_error)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  ggtitle("Horizontal Error by Focal Length") +
  xlab("Focal Length (categorical)") +
  ylab("Horizontal Error (meters)") +
  theme_minimal()

# Save the enhanced dataset with calculated fields
write.csv(cot_data, "enhanced_analyzed_cot_data.csv")

# Diagnostic plots for the regression model
par(mfrow=c(2,2))
plot(horizontal_model)
plot(vertical_model)

# -----------------------------------------------------------------------------
# Model fitting with Cook’s Distance removal
# -----------------------------------------------------------------------------

# 1) Fit initial two‐factor model and compute Cook’s distances
initial_model <- lm(horizontal_error ~ drone_to_gcp_slant_range,
                    data = cot_data)
cooks        <- cooks.distance(initial_model)
n_total      <- nrow(cot_data)
p            <- length(coef(initial_model))
cook_thresh  <- 4 / (n_total - p)

# 2) Identify & remove high‐influence points
high_idx <- which(cooks > cook_thresh)
n_high   <- length(high_idx)
n_keep   <- n_total - n_high

cat(sprintf("Total data points: %d\n",         n_total))
cat(sprintf("Cook’s D threshold: %.4f\n",     cook_thresh))
cat(sprintf("Removing %d high‐influence points\n", n_high))
cat(sprintf("Data points remaining: %d\n\n",    n_keep))

cot_data_filtered <- cot_data[cooks <= cook_thresh, ]

final_model <- lm(horizontal_error ~ drone_to_gcp_slant_range,
                  data = cot_data_filtered)

# 3) Extract and report the “tle_model_*” y intercept and slant range coeff for inclusion in droneModels.json to tune OpenAthena's target location errror (TLE) estimate
# See: https://github.com/Theta-Limited/DroneModels
cf <- coef(final_model)
tle_model_y_intercept       <- cf[1]
tle_model_slant_range_coeff <- cf["drone_to_gcp_slant_range"]

cat("tle_model_y_intercept:       ",
    sprintf("%.4f\n", tle_model_y_intercept), sep = "")
cat("tle_model_slant_range_coeff: ",
    sprintf("%.6f\n", tle_model_slant_range_coeff), sep = "")

# -----------------------------------------------------------------------------
# Prediction function
# -----------------------------------------------------------------------------
predict_horizontal_error <- function(slant_range) {
  intercept  <- tle_model_y_intercept
  sr_coef    <- tle_model_slant_range_coeff
  intercept + sr_coef * slant_range
}

# Example prediction
example_slant <- 600  # meters
pred_err <- predict_horizontal_error(example_slant)
cat(sprintf("Predicted Horizontal Error for %d m slant range: %.4f m\n\n",
            example_slant, pred_err))

# Summary for final_model, whether two factor or one factor
summary(final_model)

# -----------------------------------------------------------------------------
# 4-panel diagnostic plots for the model
# -----------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(final_model)
par(mfrow = c(1, 1))

# # 1) Compute theta (radians & degrees) and the “old formula” estimate
# LINEAR_ERROR <- 5.9  # from ASEJ 2017
# cot_data <- cot_data %>%
#   mutate(
#     theta_rad    = atan2(drone_to_gcp_vertical_distance,
#                          drone_to_gcp_horizontal_distance),
#     theta_deg    = theta_rad * 180 / pi,
#     old_estimate = abs(1 / tan(theta_rad) * LINEAR_ERROR)
#   )
# 
# # 2) Define and fit each model
# model_list <- list(
#   old_formula      = lm(horizontal_error ~ old_estimate,                data = cot_data),
#   slant_range      = lm(horizontal_error ~ drone_to_gcp_slant_range,    data = cot_data),
#   ray_slant_angle  = lm(horizontal_error ~ raySlantAngleDeg,            data = cot_data),
#   distance_ratio   = lm(horizontal_error ~ distance_ratio,              data = cot_data),
#   slant_plus_angle = lm(horizontal_error ~ drone_to_gcp_slant_range +
#                           raySlantAngleDeg,          data = cot_data),
#   slant_plus_ratio = lm(horizontal_error ~ drone_to_gcp_slant_range +
#                           distance_ratio,            data = cot_data)
# )
# 
# # 3) Summarize each model for easy comparison
# for (nm in names(model_list)) {
#   cat("==== Model:", nm, "====\n")
#   print(summary(model_list[[nm]]))
#   cat("\n")
# }

