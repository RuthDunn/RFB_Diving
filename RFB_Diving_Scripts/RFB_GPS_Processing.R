rm(list = ls(all = TRUE))

library(tidyverse) # for piping etc
library(geosphere) # for distHaversine function
library(lubridate) # for dmy function
library(adehabitatHR) # for interpolation
library(data.table) # for as.data.table function

# Function to extract statistical mode:
getmode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Loop through all bird dive files ####

files <- as.data.frame(list.files(path = "RFB_Diving_Data/BIOT_AxyTrek_GPS_txt/",
                                  pattern = "*.txt")) %>%
  separate(1, into = "files", sep = ".txt")

for (j in 1:nrow(files)) {
  
  # j = 2 # use to test code
  
  # Load files ####
  
  df.gps <- read_tsv(paste0("RFB_Diving_Data/BIOT_AxyTrek_GPS_txt/", files[j,], ".txt"), col_names = F) %>%
    dplyr::select(X1:X4) %>%
    drop_na() %>%
    dplyr::rename(Date = 1, Time = 2, Lat = 3, Lon = 4)
  df.gps$Bird <- files[j,]
  
  # df.gps <- df.gps[1:10000,] # use to test code
  
  # ~~~~~~~~
  
  # Edit time format ####
  
  df.gps$Date <- dmy(df.gps$Date)
  df.gps$Date.Time <- as.POSIXct(paste(df.gps$Date, as.character(df.gps$Time)), format = "%Y-%m-%d %H:%M:%S", tz="UTC")
  
  # ~~~~~~~~
  
  # Interpolation ####
  
  tr <- as.ltraj(data.frame(cbind(df.gps$Lon, df.gps$Lat)),
                 df.gps$Date.Time, df.gps$Bird)
  df.gps <- redisltraj(tr, u = 1, type = "time") # u = new step length in seconds
  df.gps <- ld(df.gps) %>%
    dplyr::select(x:date) %>%
    dplyr::rename(Lon = 1, Lat = 2, DateTime = 3)
  
  # ~~~~~~~~
  
  # >1 km from colony = trip ####
  
  # Determine nest coordinates (as the most common GPS)
  
  nest.coords = c(getmode(df.gps$Lon), getmode(df.gps$Lat))
  
  # Calc dists from nest
  
  df.gps$Dist.km = distHaversine(nest.coords, cbind(df.gps$Lon, df.gps$Lat))/1000

  df.gps$Trip <- ifelse(df.gps$Dist.km > 1,
                        TRUE,
                        FALSE)
  
  # ~~~~~~~~
  
  # Assign trip IDs and remove short (< 30 min) trips ####
  
  # Split up data frame where value of Trip factor changes:
  
  split <- split(df.gps, cumsum(1:nrow(df.gps) %in%
                                     which(df.gps$Trip != dplyr::lag(df.gps$Trip))))
  
  # Extract whether time period is a trip or at the nest:
  
  Trip <- as.character(unlist(lapply(split, "[", 1, "Trip")))
  
  # Calculate duration of time period:
  
  Duration <- unname(unlist(lapply(split, nrow)) * as.numeric(mean(diff(df.gps$DateTime))))
  
  # Combine trip/nest and duration:
  
  All.info <- as.data.frame(cbind(Trip, Duration))
  All.info$Duration <- as.numeric(as.character(All.info$Duration))
  All.info$Row.n <- seq.int(nrow(All.info))
  
  # If trip > 30 mins, make non-trip
  
  All.info$Trip <- ifelse(All.info$Trip == "TRUE" &
                            All.info$Duration < 1800,
                               "FALSE", All.info$Trip)
  
  # Assign trip IDs to TRUE trips
  
  Colony.info <- All.info[which(All.info$Trip=='FALSE'),]
  Colony.info$TripID <- NA
  Trip.info <- All.info[which(All.info$Trip=='TRUE'),]
  Trip.info$TripID <- seq.int(nrow(Trip.info))
  
  All.info <- rbind(Colony.info, Trip.info)
  All.info <- All.info[order(All.info$Row.n),]
  
  # Add trip info data back into normal data
  
  # Add trip info start and end times
  Times <- lapply(split, "[", , "DateTime")
  All.info$StartTime <- do.call(c, (lapply(Times, head, 1)))
  All.info$EndTime <- do.call(c, (lapply(Times, tail, 1)))
  All.info <- as.data.table(All.info)
  # Create "end time" for trip data
  df.gps.dt <- as.data.table(df.gps)
  df.gps.dt$EndTime <- df.gps.dt$DateTime + as.numeric(mean(diff(df.gps$DateTime)))
  names(df.gps.dt)[3]<-"StartTime"
  # Set key on dt2
  setkey(All.info, StartTime, EndTime)
  # Do the join
  df.gps.dt <- foverlaps(df.gps.dt,
                         All.info,
                         type = "any", mult="first") %>%
    dplyr::select(StartTime, Lon, Lat, Trip, Dist.km, TripID) %>%
    dplyr::rename(DateTime = 1)
  
  # ~~~~~~~~
  
  # Write file ####
  
  write_csv(df.gps.dt, paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[j,], "_gps.csv"))

  }

rm(All.info, Colony.info, Trip.info, df.gps, split, Times, tr, Duration, nest.coords, Trip, j)