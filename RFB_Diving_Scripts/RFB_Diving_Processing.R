rm(list = ls(all = TRUE))

library(tidyverse) # for piping etc
# library(lubridate) # for dmy function
# library(adehabitatHR) # for interpolation
library(data.table) # for converting df to dt
library(diveMove) # for extracting dive stats
library(sf) # for loading and plotting the Chagos shapefile

chagos = read_sf("RFB_Diving_Data/Chagos_Maps/chagos_maps/Chagos_v6_land_simple.shp")

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
  
  # i = 1
  
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
  
  # Remove outlier
  df = df[which(df$Depth_mod<9),]  # remove outlier
  
  # Keep trips with dives only ####
  
  # Split up data frame where value of Trip factor changes:
  df <- na.omit(df)
  split <- split(df, cumsum(1:nrow(df) %in%
                                  which(df$TripID != dplyr::lag(df$TripID))))
  
  # Calculate max depth (& extract Trip ID):
  TripID <- c(unlist(lapply(lapply(split, "[[", 8), max)))
  Max.Depth <- c(unlist(lapply(lapply(split, "[[", 3), max)))
  
  # Assign new Tip IDs
  All.info <- as.data.frame(cbind(TripID, Max.Depth))
  All.info$TripID <- ifelse(All.info$Max.Depth < 0.4, NA, All.info$TripID)
  
  # Add trip info data back into normal data
  # Add trip info start and end times
  Times <- do.call(c, lapply(split, "[", , "DateTime"))
  All.info$StartTime <- do.call(c, (lapply(Times, head, 1)))
  All.info$EndTime <- do.call(c, (lapply(Times, tail, 1)))
  All.info <- as.data.table(All.info)
  # Create "end time" for trip data
  df.dt <- as.data.table(df)
  df.dt$EndTime <- df.dt$DateTime + as.numeric(mean(diff(df$DateTime)))
  names(df.dt)[2]<-"StartTime"
  # Set key on dt2
  setkey(All.info, StartTime, EndTime)
  # Do the join
  df.dt <- foverlaps(df.dt,
                         All.info,
                         type = "any", mult="first") %>%
    dplyr::select(i.StartTime, Lon, Lat, Trip, Dist.km, TripID, Depth_mod) %>%
    dplyr::rename(DateTime = 1)
  
  df.dt <- na.omit(df.dt)
  
  # Save dive plots for each trip ####
  
  df.dt$Dive <- ifelse(df.dt$Depth_mod > 0.1, TRUE, FALSE)
  
  for (j in 1:length(unique(df$TripID))) {
    
    # j = 1
    
    tripj <- unique(df.dt$TripID)[j]
    
    ggplot(subset(df.dt, TripID == tripj)) +
      geom_point(aes(x = DateTime, y = Depth_mod, col = as.factor(Dive)), alpha = 0.4, show.legend = F) +
      scale_colour_manual(values = c("#FFB000", "#DC267F")) +
      theme_light() +
      scale_y_continuous(breaks = c(0.1,0.2,0.3,0.4,0.5))
    
    ggsave(paste0("RFB_Diving_Plots/Dive", files[i,], "_", j, ".png"), width = 8, height = 8)
  }
  
  # Use diveMove to extract dive stats ####
  
  filename <- paste0("RFB_Diving_Data/BIOT_AxyTrek_Dives_csv/", files[i,], ".csv")
  divesTDR <- createTDR(df.dt$DateTime,
                        depth = df.dt$Depth_mod,
                        dtime = 1,
                        concurrentData = data.frame(df.dt[,c(2:3,4:6)]),
                        file = filename, speed=F)
  
  caldivesTDR <- calibrateDepth(divesTDR, dive.thr = 0.1, zoc.method = "offset", offset = 0)
  
  df <- right_join(diveStats(caldivesTDR) %>%
                        select(begdesc, divetim, maxdep) %>%
                        rename(DateTime = 1, Duration = 2, MaxDepth = 3),
                      df.dt %>%
                        select(DateTime, Lon, Lat, Dist.km, TripID, Dive),
                      by = "DateTime")
  
  # Save location plots for each trip ####
  
  for (j in 1:length(unique(df$TripID))) {
    
    # j = 1
    
    tripj <- unique(df$TripID)[j]
    
    ggplot(subset(df, TripID == tripj)) +
      geom_point(aes(x = Lon, y = Lat, col = as.factor(Dive), size = as.factor(Dive)), alpha = 0.4, show.legend = F) +
      scale_colour_manual(values = c("#FFB000", "#DC267F")) +
      scale_size_manual(values = c(0.2,2)) +
      geom_sf(data = chagos, fill = "#009E73", col = "#009E73") +
      theme_light()
    
    ggsave(paste0("RFB_Diving_Plots/", files[i,], "_", j, ".png"), width = 8, height = 8)
  }
  
  # Write file ####
  
  write_csv(df, paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/Map", files[i,], "_depth_corrected.csv"))
  
  # Save plots of each trip ####

  
  
}
