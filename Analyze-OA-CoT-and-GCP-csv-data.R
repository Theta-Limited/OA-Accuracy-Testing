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
    distance_ratio = drone_to_gcp_horizontal_distance / drone_to_gcp_vertical_distance
  )

# Calculate circular and vertical errors
cot_data$horizontal_error <- mapply(haversine, cot_data$lon, cot_data$lat, gcp_data$Longitude[cot_data$nearest_gcp], gcp_data$Latitude[cot_data$nearest_gcp], MoreArgs = list(alt = 0))
cot_data$vertical_error <- abs(cot_data$hae - gcp_data$Elevation[cot_data$nearest_gcp])

# Multiple regression models incorporating the new variables
horizontal_model <- lm(horizontal_error ~ cameraSlantAngleDeg + make + model + focalLength + digitalZoomRatio + imageSelectedProportionX + imageSelectedProportionY + drone_to_gcp_horizontal_distance + drone_to_gcp_vertical_distance + distance_ratio, data = cot_data)
summary(horizontal_model)

vertical_model <- lm(vertical_error ~ cameraSlantAngleDeg + make + model + focalLength + digitalZoomRatio + imageSelectedProportionX + imageSelectedProportionY + drone_to_gcp_horizontal_distance + drone_to_gcp_vertical_distance + distance_ratio, data = cot_data)
summary(vertical_model)

# Plotting results for diagnostics and interpretation
ggplot(cot_data, aes(x = drone_to_gcp_horizontal_distance, y = horizontal_error)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Horizontal Error by Drone to GCP Horizontal Distance")

ggplot(cot_data, aes(x = distance_ratio, y = horizontal_error)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Horizontal Error by Distance Ratio")

# Save the enhanced dataset with calculated fields
write.csv(cot_data, "enhanced_analyzed_cot_data.csv")

# Diagnostic plots for the regression model
par(mfrow=c(2,2))
plot(horizontal_model)
plot(vertical_model)
