rm(list = ls(all = TRUE))

library(tidyverse) # for piping etc
library(geosphere) # for distHaversine function
library(lubridate) # for dmy function
library(adehabitatHR) # for interpolation

# Function to designate Trip IDs:
source('RFB_Diving_Scripts/Functions/TripID.R', echo=TRUE) # Function for assigning trip IDs; commented out for now

# Function to extract statistical mode:
getmode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load raw-ish 2022 gps data ####

files2022 <- list.files(path = "RFB_Diving_Data/BIOT_CH_2022_AxyTrek/", pattern = "*.csv")
files2022 <- separate(as.data.frame(files2022), 1, into = "files2022", sep = ".csv")

for (j in 1:nrow(files2022)) {
  
  # j = 1 # use to test code
  
  # Load files
  df.gps <- read_tsv(paste0("RFB_Diving_Data/BIOT_CH_2022_AxyTrek/", files2022[j,], ".txt"), col_names = F) %>%
    dplyr::select(X1:X4) %>%
    drop_na() %>%
    dplyr::rename(Date = 1, Time = 2, Lat = 3, Lon = 4)
  
  # df.gps <- df.gps[1:10000,] # use to test code
  
  # Edit time format
  df.gps$Date <- dmy(df.gps$Date)
  df.gps$Date.Time <- as.POSIXct(paste(df.gps$Date, as.character(df.gps$Time)), format = "%Y-%m-%d %H:%M:%S", tz="UTC")
  df.gps$Bird <- files2022[j,]
  
  # Interpolation
  tr <- as.ltraj(data.frame(cbind(df.gps$Lon, df.gps$Lat)),
                 df.gps$Date.Time, df.gps$Bird)
  df.gps <- redisltraj(tr, u = 1, type = "time") # u = new step length in seconds
  df.gps <- ld(df.gps) %>%
    dplyr::select(x:date) %>%
    dplyr::rename(Lon = 1, Lat = 2, DateTime = 3)
  
  # >1 km from colony = trip
  # Determine nest coordinates (as the most common GPS)
  nest.coords = c(getmode(df.gps$Lon), getmode(df.gps$Lat))
  
  # Calc dists from nest
  df.gps$Dist.km = distHaversine(nest.coords, cbind(df.gps$Lon, df.gps$Lat))/1000

  df.gps$Trip <- ifelse(df.gps$Dist.km > 1,
                        TRUE,
                        FALSE)
  
  # Trip IDs
  df.gps <- Give.trip.IDs(df.gps)
  
  write_csv(df.gps, paste0("RFB_Diving_Data/CH_AxyTrek_Processed/", files2022[j,], "_gps.csv"))

  }

rm(df.gps, tr, j, nest.coords, Give.trip.IDs, getmode)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load raw-ish 2019 gps data ####

files2019 <- list.files(path = "RFB_Diving_Data/BIOT_DGBP_2019_AxyTrek/", pattern = "*.csv")
files2019 <- separate(as.data.frame(files2019), 1, into = "files2019", sep = ".csv")

for (j in 1:nrow(files2019)) {
  
  # j = 1 # use to test code
  
  # Load files
  df.gps <- read_tsv(paste0("RFB_Diving_Data/BIOT_DGBP_2019_AxyTrek/", files2019[j,], ".txt"), col_names = F) %>%
    dplyr::select(X1:X4) %>%
    drop_na() %>%
    dplyr::rename(Date = 1, Time = 2, Lat = 3, Lon = 4)
  
  # df.gps <- df.gps[1:1000,] # use to test code
  
  # Edit time format
  df.gps$Date <- dmy(df.gps$Date)
  df.gps$Date.Time <- as.POSIXct(paste(df.gps$Date, as.character(df.gps$Time)), format = "%Y-%m-%d %H:%M:%S", tz="UTC")
  df.gps$Bird <- files2019[j,]
  
  # Interpolation
  tr <- as.ltraj(data.frame(cbind(df.gps$Lon, df.gps$Lat)),
                 df.gps$Date.Time, df.gps$Bird)
  df.gps <- redisltraj(tr, u = 1, type = "time") # u = new step length in seconds
  df.gps <- ld(df.gps) %>%
    dplyr::select(x:date) %>%
    dplyr::rename(Lon = 1, Lat = 2, DateTime = 3)
  
  # >1 km from colony = trip
  # Determine nest coordinates (as the most common GPS)
  nest.coords = c(getmode(df.gps$Lon), getmode(df.gps$Lat))
  
  # Calc dists from nest
  df.gps$Dist.km = distHaversine(nest.coords, cbind(df.gps$Lon, df.gps$Lat))/1000
  
  df.gps$Trip <- ifelse(df.gps$Dist.km > 1,
                        TRUE,
                        FALSE)
  
  # Trip IDs
  df.gps <- Give.trip.IDs(df.gps)
  
  write_csv(df.gps, paste0("RFB_Diving_Data/CH_AxyTrek_Processed/", files2019[j,], "_gps.csv"))
  
}

rm(df.gps, tr, j, nest.coords, Give.trip.IDs, getmode)

