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

files <- list.files(path = "RFB_Diving_Data/BIOT_CH_2022_AxyTrek/", pattern = "*.csv")
files <- separate(as.data.frame(files), 1, into = "files", sep = ".csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load raw-ish gps data ####

for (j in 1:nrow(files)) {
  
  # j = 1 # use to test code
  
  # Load files
  df.gps <- read_tsv(paste0("RFB_Diving_Data/BIOT_CH_2022_AxyTrek/", files[j,], ".txt"), col_names = F) %>%
    dplyr::select(X1:X4) %>%
    drop_na() %>%
    dplyr::rename(Date = 1, Time = 2, Lat = 3, Lon = 4)
  
  # df.gps <- df.gps[1:10000,] # use to test code
  
  # Edit time format
  df.gps$Date <- dmy(df.gps$Date)
  df.gps$Date.Time <- as.POSIXct(paste(df.gps$Date, as.character(df.gps$Time)), format = "%Y-%m-%d %H:%M:%S", tz="UTC")
  df.gps$Bird <- files[j,]
  
  # Interpolation
  tr <- as.ltraj(data.frame(cbind(df.gps$Lon, df.gps$Lat)),
                 df.gps$Date.Time, df.gps$Bird)
  df.gps <- redisltraj(tr, u = 1, type = "time") # u = new step length in seconds
  df.gps <- ld(df.gps) %>%
    dplyr::select(x:date) %>%
    dplyr::rename(Lon = 1, Lat = 2, Date = 3)
  
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
  
  write_csv(df.gps, paste0("RFB_Diving_Data/CH_AxyTrek_Processed/", files[j,], "_gps.csv"))

  }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load raw-ish dive data ####

# Loop through .csv files
# Select depth cols only
# Drop na data
# Combine GPS data

for (i in 1:nrow(files)) {
  
  df <- left_join(read_tsv(paste0("RFB_Diving_Data/BIOT_CH_2022_AxyTrek/", files[i,], ".csv")) %>%
                    dplyr::select(TagID, Date, Time, Depth) %>%
                    drop_na() %>%
                    mutate(DateTime = as.POSIXct(paste(Date, as.character(Time)), format = "%d/%m/%Y %H:%M:%S", tz="UTC")) %>%
                    dplyr::select(TagID, DateTime, Depth),
                  read_csv(paste0("RFB_Diving_Data/CH_AxyTrek_Processed/", files[i,], "_gps.csv")) %>%
                    dplyr::select(Date, Lon, Lat, Trip, TripID) %>%
                    rename("DateTime" = 1),
                  by = "DateTime")
  
  write_csv(df, paste0("RFB_Diving_Data/CH_AxyTrek_Processed/", files[i,], "_depth.csv"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Correct for device drift ####

# Looks at every time point in conjunction with the next 30 to determine whether the baseline has shifted

k = 30  # window width for smoothing

for (i in 1:nrow(files)) {
  
  df <- read_csv(paste0("RFB_Diving_Data/CH_AxyTrek_Processed/", files[i,], "_depth.csv"))
  
  # Calculate rolling median depth every 30 readings
  offset <- zoo::rollapply(df$Depth, width = k, by = k, FUN = median)
  offset <- rep(offset, each = k)

  # Match lengths of offset df and OG depth df
  dif = length(df$Depth) - length(offset)
  offset = c(offset, rep(tail(offset, 1), dif))

  # Perform zero-offset correction on depth data
  new_series = df$Depth - offset
  new_series[new_series<0] = 0  # correct meaningless negative depths
  new_series[(length(new_series)-dif):length(new_series)]
  df$Depth_mod = new_series
  
  write_csv(df, paste0("RFB_Diving_Data/CH_AxyTrek_Processed/", files[i,], "_depth_corrected.csv"))
  
  for (j in 1:length(unique(as.Date(df$DateTime)))) {
    
    datj <- unique(as.Date(df$DateTime))[j]
    
    ggplot(subset(df, as.Date(DateTime) == datj)) +
      geom_point(aes(x = DateTime, y = Depth_mod, col = Trip), alpha = 0.4) +
      theme_light()
    
    ggsave(paste0("RFB_Diving_Plots/", files[i,], "_", j, "b.png"), width = 8, height = 8)
  }
}
