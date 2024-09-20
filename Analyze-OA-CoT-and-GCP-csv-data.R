# Copyright 2024 Theta Informatics LLC
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

# Load CoT data from CSV
cot_data <- read.csv("path/to/your/OA-CoT-Capture-file.csv")

# Load GCP data from a ZIP file
gcp_data <- read.csv(unzip("path/to/your/ground_control_points.zip", files = NULL))

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
model_components <- c("cameraSlantAngleDeg", "raySlantAngleDeg", "focalLength", "digitalZoomRatio",
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
hoizontal_model_formula <- as.formula(paste("horizontal_error ~", paste(model_components, collapse = " + ")))
vertical_model_formula <- as.formula(paste("vertical_error ~", paste(model_components, collapse = " + ")))


# Multiple regression models incorporating the new variables
horizontal_model <- lm(hoizontal_model_formula, data = cot_data)
summary(horizontal_model)

vertical_model <- lm(vertical_model_formula, data = cot_data)
summary(vertical_model)

# Plotting results for diagnostics and interpretation
ggplot(cot_data, aes(x = drone_to_gcp_horizontal_distance, y = horizontal_error)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Horizontal Error by Drone to GCP Horizontal Distance")

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

# Plotting MAE by Model
ggplot(mae_by_model, aes(x = model, y = MAE)) +
  geom_col(fill = "darkred") +
  ggtitle("Mean Absolute Error (MAE) by Drone Model") +
  xlab("Model") +
  ylab("MAE (meters)") +
  theme_minimal()

# Save the enhanced dataset with calculated fields
write.csv(cot_data, "enhanced_analyzed_cot_data.csv")

# Diagnostic plots for the regression model
par(mfrow=c(2,2))
plot(horizontal_model)
plot(vertical_model)
