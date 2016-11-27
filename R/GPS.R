library(plyr)
library(dplyr)
library(ggmap)

# Set our working directory path to a string for easy retreival
# Anyone implementing this code should point to where their GPS data is
# NOTE: This was data pulled from the Adafruit Ultimate GPS breakout board
#  with output set to RMC and GGA
wd <- "C:/users/yamak_000/OneDrive/School/PHYS395L/Final Lab/Code/GPSData"

# Set our working directory
setwd(wd)

# Read in the raw GPS data (Both GPRMC and GPGGA)
# TODO: grab files automatically from folder
rawGps <- read.csv("GPSLG002.csv", header = FALSE)

# Remove any entries that doesn't start with $GPRMC 
#  and remove unneeded columns
gprmc <- subset(rawGps[rawGps[1] == "$GPRMC",], select = c(V2, V4, V5, V6, V7, V8, V10))

# Do the same with the $GPGGA data, but only keep
#  the Longitude and Latitude values with their directions
altitude <- subset(rawGps[rawGps[1] == "$GPGGA",], select = c(V2, V3, V4, V5, V6, V10))

# Set the column names before turning it into a data frame
names(gprmc) <- c("UtcTimestamp", "Latitude", "DirLat", "Longitude", "DirLon", "Speed", "Date")
names(altitude) <- c("UtcTimestamp", "Latitude", "DirLat", "Longitude", "DirLon", "Altitude")

combined <- merge(altitude, gprmc)

# Create our function to parse the Longitude/Latitude correctly
convertToDegrees <- function(x, direction) {
  # Convert our coordinates to numeric first
  num <- as.numeric(as.character(x))
  
  # Get the degree portion, and throw away the rest
  degreesInt <- floor(num/100)
  
  # Get the minutes portion, and convert it to degrees
  minutes <- ((num/100) - degreesInt) * 1.66667
  
  # Add them together
  degrees <- degreesInt + minutes
  
  # Set sign of value dependent on hemisphere we're in
  degrees <- ifelse(direction == "W" | direction == "S", degrees*-1, degrees)
  
  return(degrees)
}

# Create our function to convert Knots to m/s
convertToMetersPerSecond <- function(x) {
  # Convert our value to numeric
  num <- as.numeric(as.character(x))
  
  # Change our units from knots to m/s
  metersPerSecond <- num*0.5144
  
  return(metersPerSecond)
}

# Use our functions
combined$Latitude <- convertToDegrees(combined$Latitude, combined$DirLat)
combined$Longitude <- convertToDegrees(combined$Longitude, combined$DirLon)
combined$Speed <- convertToMetersPerSecond(combined$Speed)

combined$DateTime <- as.POSIXct(paste(combined$Date, combined$UtcTimestamp), format = "%d%m%y %H%M%S", tz = "GMT")

# Convert our data to a dataframe
df <- as.data.frame(subset(combined, select = -c(DirLat, DirLon, UtcTimestamp, Date)))

# Create our map image by finding the center of all the values
mapImageData <- get_googlemap(center = c(lon = mean(df$Longitude), 
                                         lat = mean(df$Latitude)),
                                         zoom = 14,
                                         #size = c(500, 500),
                                         maptype= c("roadmap"))

# Output our map with a gradient fill for start to finish
ggmap(mapImageData) + 
  geom_path(aes(x = Longitude, 
                y = Latitude,
                color = as.numeric(DateTime),
                alpha = 1/3),
             data = df,
             size = 2
            ) + 
  guides(
    fill = FALSE, alpha = FALSE, size = FALSE, color = FALSE
  ) +
  scale_colour_gradient(low = "red", 
                        high = "green")