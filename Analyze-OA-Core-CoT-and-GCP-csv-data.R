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

## # May be wrong
## # Function to calculate bearing (azimuth) from point A to point B
## bearing <- function(lon1, lat1, lon2, lat2) {
##   delta_lon <- (lon2 - lon1) * pi / 180
##   lat1 <- lat1 * pi / 180
##   lat2 <- lat2 * pi / 180
##   x <- sin(delta_lon) * cos(lat2)
##   y <- cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(delta_lon)
##   initial_bearing <- atan2(x, y) * 180 / pi
##   bearing_deg <- (initial_bearing + 360) %% 360
##   return(bearing_deg)
## }

## # May be wrong
## # Function to calculate elevation angle (pitch) from horizontal and vertical distances
## elevation_angle <- function(horizontal_distance, vertical_distance) {
##   angle_rad <- atan(vertical_distance / horizontal_distance)
##   angle_deg <- angle_rad * 180 / pi
##   return(angle_deg)
## }

# Load CoT data from CSV
cot_data <- read.csv("/PATH/TO/OA-CoT-Capture.csv", stringsAsFactors = FALSE)

# Load GCP data from a ZIP file
gcp_data <- read.csv(unzip("/PATH/TO/ground_control_points.csv.zip", files = NULL), stringsAsFactors = FALSE)

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
  (cot_data$imageHeight * (0.5 - cot_data$imageSelectedProportionY))^2
)

## ### Code to Calculate Azimuth and Pitch Differences ###
## ### May be wrong
## # Calculate actual azimuth (bearing) and pitch (elevation angle) from drone to GCP
## cot_data <- cot_data %>%
##   mutate(
##     actual_azimuth = bearing(lon, lat, gcp_data$Longitude[nearest_gcp], gcp_data$Latitude[nearest_gcp]),
##     actual_pitch = elevation_angle(drone_to_gcp_horizontal_distance, drone_to_gcp_vertical_distance),
##     # Extract reported gimbal yaw and pitch
##     reported_gimbal_yaw = gimbalYawDegree,
##     reported_gimbal_pitch = gimbalPitchDegree,
##     # Calculate differences
##     difference_yaw = (actual_azimuth - reported_gimbal_yaw + 180) %% 360 - 180,  # Normalize to [-180, 180]
##     difference_pitch = actual_pitch - reported_gimbal_pitch
##   )

## # Remove outliers using the IQR method for yaw differences
## yaw_Q1 <- quantile(cot_data$difference_yaw, 0.25, na.rm = TRUE)
## yaw_Q3 <- quantile(cot_data$difference_yaw, 0.75, na.rm = TRUE)
## yaw_IQR <- yaw_Q3 - yaw_Q1
## yaw_lower_bound <- yaw_Q1 - 1.5 * yaw_IQR
## yaw_upper_bound <- yaw_Q3 + 1.5 * yaw_IQR

## # Remove outliers using the IQR method for pitch differences
## pitch_Q1 <- quantile(cot_data$difference_pitch, 0.25, na.rm = TRUE)
## pitch_Q3 <- quantile(cot_data$difference_pitch, 0.75, na.rm = TRUE)
## pitch_IQR <- pitch_Q3 - pitch_Q1
## pitch_lower_bound <- pitch_Q1 - 1.5 * pitch_IQR
## pitch_upper_bound <- pitch_Q3 + 1.5 * pitch_IQR

## # Filter out outliers
## cot_data_filtered <- cot_data %>%
##   filter(
##     difference_yaw >= yaw_lower_bound,
##     difference_yaw <= yaw_upper_bound,
##     difference_pitch >= pitch_lower_bound,
##     difference_pitch <= pitch_upper_bound
##   )

## # Calculate average differences excluding outliers
## average_difference_yaw <- mean(cot_data_filtered$difference_yaw, na.rm = TRUE)
## average_difference_pitch <- mean(cot_data_filtered$difference_pitch, na.rm = TRUE)

## # Print the average differences
## print(paste("Average Yaw Difference (degrees):", round(average_difference_yaw, 2)))
## print(paste("Average Pitch Difference (degrees):", round(average_difference_pitch, 2)))

## # (Optional) Plot the differences to visualize
## ggplot(cot_data_filtered, aes(x = difference_yaw)) +
##   geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
##   ggtitle("Histogram of Yaw Differences") +
##   xlab("Yaw Difference (degrees)") +
##   ylab("Frequency")

## ggplot(cot_data_filtered, aes(x = difference_pitch)) +
##   geom_histogram(binwidth = 1, fill = "salmon", color = "black") +
##   ggtitle("Histogram of Pitch Differences") +
##   xlab("Pitch Difference (degrees)") +
##   ylab("Frequency")

# Dynamically build model formula based on the data
## model_components <- c("cameraSlantAngleDeg", "raySlantAngleDeg", "focalLength", "digitalZoomRatio",
##                       "imageSelectedProportionX", "imageSelectedProportionY",
##                       "pixelDistFromPrincipalPoint", "drone_to_gcp_horizontal_distance",
##                       "drone_to_gcp_vertical_distance", "distance_ratio")
model_components <- c("cameraSlantAngleDeg", "raySlantAngleDeg",
                      "drone_to_gcp_horizontal_distance",
                      "drone_to_gcp_vertical_distance")
# If only one make or model of drone is present in data, omit this
# categorical variable from regression model(s)
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

# Calculate the slope of horizontal error by horizontal distance
# This represents how much circular error is introduced per meter of distance

# Perform a simple linear regression
slope_model <- lm(horizontal_error ~ drone_to_gcp_horizontal_distance, data = cot_data)

# Extract the slope coefficient
slope <- coef(slope_model)["drone_to_gcp_horizontal_distance"]

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

# Convert EXIF.DateTime to POSIXct Date-Time Object
# Assuming EXIF.DateTime is in ISO 8601 format, e.g., "2024-11-15T13:41:50Z"
cot_data$EXIF_DateTime_POSIXct <- as.POSIXct(cot_data$EXIF.DateTime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Handle any parsing issues by checking for NA values
num_na_dates <- sum(is.na(cot_data$EXIF_DateTime_POSIXct))
if (num_na_dates > 0) {
  warning(paste("There are", num_na_dates, "rows with invalid EXIF.DateTime formats. These will be excluded from the time-based plot."))
}

# Create the plot: Horizontal Error vs. EXIF.DateTime
ggplot(cot_data, aes(x = EXIF_DateTime_POSIXct, y = horizontal_error / drone_to_gcp_horizontal_distance)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  #geom_smooth(method = "loess", se = TRUE, color = "red") +
  ggtitle("Horizontal Error Over Time") +
  xlab("EXIF DateTime") +
  ylab("Horizontal Error (meters) / GCP Horizontal Distance") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Improve readability of date labels
    plot.title = element_text(hjust = 0.5)              # Center the plot title
  )

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

# Calculate CE90% (90th percentile of horizontal errors) in meters for each drone model
ce90_by_model <- cot_data %>%
  group_by(model) %>%
  summarise(CE90 = quantile(horizontal_error, 0.9))

#Print CE90
print(ce90_by_model)

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

# Plotting MAE by Model
ggplot(mae_by_model, aes(x = model, y = MAE)) +
  geom_col(fill = "darkred") +
  ggtitle("Mean Absolute Error (MAE) by Drone Model") +
  xlab("Model") +
  ylab("MAE (meters)") +
  theme_minimal()

# 1. Calculate Median Horizontal Error Overall
median_horizontal_error <- median(cot_data$horizontal_error, na.rm = TRUE)
print(paste("Median Horizontal Error (Overall):", round(median_horizontal_error, 2), "meters"))

# 2. Calculate Median Horizontal Error by Drone Model
median_error_by_model <- cot_data %>%
  group_by(model) %>%
  summarise(Median_Horizontal_Error = median(horizontal_error, na.rm = TRUE))

# Print Median Horizontal Error by Model
print(median_error_by_model)

# 3. (Optional) Plot Median Horizontal Error by Drone Model
ggplot(median_error_by_model, aes(x = model, y = Median_Horizontal_Error)) +
  geom_col(fill = "forestgreen") +
  ggtitle("Median Horizontal Error by Drone Model") +
  xlab("Drone Model") +
  ylab("Median Horizontal Error (meters)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve label readability for models


# Save the enhanced dataset with calculated fields
write.csv(cot_data, "enhanced_analyzed_cot_data.csv")

# Diagnostic plots for the regression model
par(mfrow=c(2,2))
plot(horizontal_model)
plot(vertical_model)
