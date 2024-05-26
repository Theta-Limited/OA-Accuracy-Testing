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

# Find nearest GCP for each CoT capture
find_nearest_gcp <- function(lat, lon, alt) {
  distances <- sapply(1:nrow(gcp_data), function(i) {
    haversine(lon, lat, gcp_data$Longitude[i], gcp_data$Latitude[i], alt)
  })
  return(which.min(distances))
}

cot_data$nearest_gcp <- sapply(1:nrow(cot_data), function(i) {
  find_nearest_gcp(cot_data$lat[i], cot_data$lon[i], cot_data$hae[i])
})

# Calculate circular and vertical errors
cot_data$horizontal_error <- mapply(function(i, j) {
  haversine(cot_data$lon[i], cot_data$lat[i], gcp_data$Longitude[j], gcp_data$Latitude[j], 0)
}, i = 1:nrow(cot_data), j = cot_data$nearest_gcp)

cot_data$vertical_error <- abs(cot_data$hae - gcp_data$Elevation[cot_data$nearest_gcp])

# Regression model and plot
model <- lm(horizontal_error ~ slantAngleDegrees, data = cot_data)
summary(model)

ggplot(cot_data, aes(x = slantAngleDegrees, y = horizontal_error)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Dependency of Horizontal Error on Slant Angle Degrees")

# Save analyzed data
write.csv(cot_data, "analyzed_cot_data.csv")

# Convert categorical variables to factors
cot_data$make <- as.factor(cot_data$make)
cot_data$model <- as.factor(cot_data$model)

# Multiple linear regression for horizontal error
horizontal_model <- lm(horizontal_error ~ make + model + focalLength + digitalZoomRatio + imageSelectedProportionX + imageSelectedProportionY, data = cot_data)
summary(horizontal_model)

# Multiple linear regression for vertical error
vertical_model <- lm(vertical_error ~ make + model + focalLength + digitalZoomRatio + imageSelectedProportionX + imageSelectedProportionY, data = cot_data)
summary(vertical_model)

# Plotting the results for horizontal error
ggplot(cot_data, aes(x = focalLength, y = horizontal_error)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~make) +
  ggtitle("Horizontal Error by Focal Length and Make")

# Plotting the results for vertical error
ggplot(cot_data, aes(x = focalLength, y = vertical_error)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~make) +
  ggtitle("Vertical Error by Focal Length and Make")

# To assess model diagnostics, plot residuals
par(mfrow=c(2,2))
plot(horizontal_model)
plot(vertical_model)
