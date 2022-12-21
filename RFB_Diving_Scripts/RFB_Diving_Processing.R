rm(list = ls(all = TRUE))

library(tidyverse) # for piping etc
# library(lubridate) # for dmy function
# library(adehabitatHR) # for interpolation

files <- as.data.frame(list.files(path = "RFB_Diving_Data/BIOT_AxyTrek_Dives_csv/", pattern = "*.csv")) %>%
  separate(1, into = "files", sep = ".csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load raw-ish dive data

# Loop through .csv files
# Select depth cols only
# Drop na data
# Combine GPS data

# Correct for device drift

# Look at every time point in conjunction with the next 30
# to determine whether the baseline has shifted

k = 30  # window width for smoothing

for (i in 1:nrow(files)) {
  
  # Load data ####
  
  df <- left_join(read_tsv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Dives_csv/", files[i,], ".csv")) %>%
                    dplyr::select(TagID, Date, Time, Depth) %>%
                    drop_na() %>%
                    mutate(DateTime = as.POSIXct(paste(Date, as.character(Time)), format = "%d/%m/%Y %H:%M:%S", tz="UTC")) %>%
                    dplyr::select(TagID, DateTime, Depth),
                  read_csv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_gps.csv")) %>%
                    dplyr::select(DateTime, Lon, Lat, Dist.km, Trip, TripID),
                  by = "DateTime") %>%
    drop_na(Lat)
  
  # Correct drift ####
  
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
  
  # Write file ####
  
  write_csv(df, paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_depth_corrected.csv"))
  
  df <- na.omit(df)
  
  # Save plots of each trip ####
  
  for (j in 1:length(unique(df$TripID))) {
    
    tripj <- unique(df$TripID)[j]
    
    ggplot(subset(df, TripID == tripj)) +
      geom_point(aes(x = DateTime, y = Depth_mod), alpha = 0.4) +
      theme_light()
    
    ggsave(paste0("RFB_Diving_Plots/", files[i,], "_", j, ".png"), width = 8, height = 8)
  }
}
