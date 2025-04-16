# Revised Script for Comparing SRTM (Control) vs. 3DEP (Experiment) Groups
# Copyright 2025 Theta Informatics LLC
# Provided under MIT License. Modification and Redistribution permitted while above notice is preserved.

library(dplyr)
library(ggplot2)

### FUNCTIONS #################################################################

# Haversine function to calculate the great-circle distance between two points
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
  # Use a custom radius function that accounts for altitude; see below.
  r <- radius_at_lat_lon(avg_lat, 0)
  r <- r + alt
  return(c * r)
}

# Function to compute Earth radius at a given latitude using the ellipsoid parameters
radius_at_lat_lon <- function(lat, lon) {
  A <- 6378137.0  # semi-major axis in meters
  B <- 6356752.3  # semi-minor axis in meters
  lat <- lat * pi / 180
  r <- ((A^2 * cos(lat))^2 + (B^2 * sin(lat))^2) / ((A * cos(lat))^2 + (B * sin(lat))^2)
  return(sqrt(r))
}

### DATA INGESTION ############################################################

# Set file paths for control (SRTM) and experimental (3DEP) CSV files.
# Update these paths as appropriate for your environment.
control_file <- "~/path/to/control_SRTM.csv"
experiment_file <- "~/path/to/experiment_3DEP.csv"

# Read the CSV files with CoT data
cot_data_control <- read.csv(control_file, stringsAsFactors = FALSE)
cot_data_experiment <- read.csv(experiment_file, stringsAsFactors = FALSE)

# Create a group indicator variable: SRTM for control and 3DEP for experimental
cot_data_control$group <- "SRTM"
cot_data_experiment$group <- "3DEP"

# Combine the two datasets into one for analysis
cot_data <- rbind(cot_data_control, cot_data_experiment)

# Load GCP data from CSV (update path as necessary)
gcp_file <- "~/path/to/gcp_data.csv"
gcp_data <- read.csv(gcp_file, stringsAsFactors = FALSE)

### DISTANCE CALCULATIONS #####################################################

# For each CoT record find the nearest Ground Control Point (GCP) and calculate distances.
cot_data <- cot_data %>%
  mutate(
    nearest_gcp = sapply(seq_len(n()), function(i) {
      # For each row, find the index of the GCP with the minimum Haversine distance
      which.min(haversine(lon[i], lat[i], gcp_data$Longitude, gcp_data$Latitude, 0))
    }),
    drone_to_gcp_horizontal_distance = mapply(function(i, j) {
      haversine(droneLongitude[i], droneLatitude[i],
                gcp_data$Longitude[j], gcp_data$Latitude[j], 0)
    }, i = seq_len(n()), j = nearest_gcp),
    drone_to_gcp_vertical_distance = abs(droneElevationHAE - gcp_data$Elevation[nearest_gcp]),
    distance_ratio = drone_to_gcp_vertical_distance / drone_to_gcp_horizontal_distance
  )

# Calculate horizontal error (using the haversine distance between the CoT point and the nearest GCP)
cot_data$horizontal_error <- mapply(haversine,
                                    cot_data$lon, cot_data$lat,
                                    gcp_data$Longitude[cot_data$nearest_gcp],
                                    gcp_data$Latitude[cot_data$nearest_gcp],
                                    MoreArgs = list(alt = 0))

# Calculate vertical error (absolute difference in elevation)
cot_data$vertical_error <- abs(cot_data$hae - gcp_data$Elevation[cot_data$nearest_gcp])

# Calculate the distance of the user-selected pixel from the principal point (image center)
cot_data$pixelDistFromPrincipalPoint <- sqrt(
  (cot_data$imageWidth * (0.5 - cot_data$imageSelectedProportionX))^2 +
    (cot_data$imageHeight * (0.5 - cot_data$imageSelectedProportionY))^2
)

### PLOTTING GROUP COMPARISONS ###############################################

# 1. Box Plot of Horizontal Error for SRTM vs. 3DEP
ggplot(cot_data, aes(x = group, y = horizontal_error, fill = group)) +
  geom_boxplot() +
  labs(title = "Horizontal Error Comparison: SRTM vs. 3DEP",
       x = "Group (Digital Elevation Model)",
       y = "Horizontal Error (meters)") +
  theme_minimal()

### SUMMARY ERROR STATISTICS ###############################################

# Calculate Circular Error Probable (CEP, the 50th percentile), 
# CE90 (90th percentile), and Mean Absolute Error (MAE) for each group.
group_stats <- cot_data %>% 
  group_by(group) %>%
  summarise(
    CEP = quantile(horizontal_error, 0.5, na.rm = TRUE),
    CE90 = quantile(horizontal_error, 0.9, na.rm = TRUE),
    MAE = mean(abs(horizontal_error), na.rm = TRUE)
  )

print(group_stats)

# Plot CEP by group
ggplot(group_stats, aes(x = group, y = CEP, fill = group)) +
  geom_col(alpha = 0.7) +
  labs(title = "Circular Error Probable (CEP) by Group",
       x = "Group", y = "CEP (meters)") +
  theme_minimal()

# Plot CE90 by group
ggplot(group_stats, aes(x = group, y = CE90, fill = group)) +
  geom_col(alpha = 0.7) +
  labs(title = "CE90 (90th Percentile Error) by Group",
       x = "Group", y = "CE90 (meters)") +
  theme_minimal()

# Plot MAE by group
ggplot(group_stats, aes(x = group, y = MAE, fill = group)) +
  geom_col(alpha = 0.7) +
  labs(title = "Mean Absolute Error (MAE) by Group",
       x = "Group", y = "MAE (meters)") +
  theme_minimal()

### SCATTERPLOT WITH TREND LINES AND SLOPE ANNOTATIONS #####################

# Compute slopes (error per horizontal distance) for each group separately.
slope_control <- coef(lm(horizontal_error ~ drone_to_gcp_horizontal_distance, 
                         data = filter(cot_data, group == "SRTM")))[2]
slope_experiment <- coef(lm(horizontal_error ~ drone_to_gcp_horizontal_distance, 
                            data = filter(cot_data, group == "3DEP")))[2]

# Get approximate maximum values to set annotation positions
max_x <- max(cot_data$drone_to_gcp_horizontal_distance, na.rm = TRUE)
max_y <- max(cot_data$horizontal_error, na.rm = TRUE)

# Create scatter plot with regression trend lines for both groups
ggplot(cot_data, aes(x = drone_to_gcp_horizontal_distance, y = horizontal_error, color = group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Horizontal Error vs. Drone-to-GCP Horizontal Distance",
       x = "Drone to GCP Horizontal Distance (meters)",
       y = "Horizontal Error (meters)") +
  annotate("text", x = 0.75*max_x, y = 0.9*max_y,
           label = sprintf("SRTM slope: %.4f m/m", slope_control),
           color = "darkblue", hjust = 1) +
  annotate("text", x = 0.75*max_x, y = 0.85*max_y,
           label = sprintf("3DEP slope: %.4f m/m", slope_experiment),
           color = "darkred", hjust = 1) +
  theme_minimal()

### ADDITIONAL ANALYSIS #######################################################

# (Example) Comparing influence of pixel distance from the principal point on horizontal error
ggplot(cot_data, aes(x = pixelDistFromPrincipalPoint, y = horizontal_error, color = group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Horizontal Error vs. Pixel Distance from Principal Point",
       x = "Pixel Distance from Principal Point",
       y = "Horizontal Error (meters)") +
  theme_minimal()

### SAVE ENHANCED DATA ########################################################

# Optionally, save the enhanced and merged dataset for future analysis.
write.csv(cot_data, "enhanced_analyzed_cot_data.csv", row.names = FALSE)
