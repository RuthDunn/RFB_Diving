# Give.trip.IDs function
# Modified from Alice Trevail's script:
# https://github.com/AliceTrevail/GPS-file-processing/blob/master/Functions/Track_calculations.R

if(!"tidyverse" %in% installed.packages())
  install.packages("tidyverse")
library(tidyverse)

if(!"sp" %in% installed.packages())
  install.packages("sp")
library(sp)

if(!"trip" %in% installed.packages())
  remotes::install_github("Trackage/trip")
library(trip)

if(!"maptools" %in% installed.packages())
  install.packages("maptools")
library(maptools)

if(!"plyr" %in% installed.packages())
  install.packages("plyr")
library(plyr)

# Assign trip Ids ####

# Automatically assign trip ID for an individual bird file

Give.trip.IDs <- function(x){
  # 1. Create new column where if colony or trip same as above = TRUE, if different = FALSE
  # This means that rows that are the start of a trip can be identified
  x$Same <- NA
  
  for (i in 2:NROW(x)){
    x$Same[i] <- ifelse((x[i,"Trip"] == x[i-1, "Trip"]), "TRUE", "FALSE")
  }
  
  # 2. Create subset that is only trip points
  trip.matrix <- subset(x, Trip == "TRUE")
  
  # 3. Create subset that is only the first points of each trip
  new.trips <- subset(trip.matrix, Same == FALSE)
  
  # 4. Asign trip IDs
  # First trip ID = BirdID .01
  # Rest are sequential
  new.trips$TripID <- NA
  new.trips$TripID[1] <- 1
  if (NROW(new.trips) > 1) {
    for (i in 2:NROW(new.trips)){
      new.trips$TripID[i] <- (as.numeric(new.trips$TripID[i-1]) + 1)
    } 
  }
  
  # 5. Merge data frames to include trip IDs
  # Fill NAs with previous value of trip ID
  trip.matrix2 <- merge(trip.matrix, new.trips, all.x = TRUE)
  trip.matrix2 <- trip.matrix2[order(trip.matrix2$Date),]
  trip.matrix2$TripID <- na_locf(trip.matrix2$TripID)
  
  # 6. Merge data frames to include colony points again
  trip.matrix3 <- merge(x, trip.matrix2,  all.x = TRUE)
  trip.matrix3 <- trip.matrix3[order(trip.matrix3$Date),]
  trip.matrix3 <- trip.matrix3[,-5]
  
  trip.matrix3
}
