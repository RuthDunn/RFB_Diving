rm(list = ls(all = TRUE))

library(tidyverse) # for piping etc
library(data.table) # for converting df to dt
library(diveMove) # for extracting dive stats
library(sf) # for loading and plotting the Chagos shapefile
library(suncalc) # for calculating times of day
library(lubridate) # for ymd function
library(geosphere) # calculate distances between locations

chagos = read_sf("RFB_Diving_Data/Chagos_Maps/chagos_maps/Chagos_v6_land_simple.shp")
diego.garcia = st_coordinates(tail(chagos$geometry, 1))[, c('X', 'Y')]
colnames(diego.garcia) = c('lon', 'lat')

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
  
  # i = 9
  
  # Load data ####
  
  filename <- paste0("RFB_Diving_Data/BIOT_AxyTrek_Dives_csv/", files[i,], ".csv")
  
  df.dive.locs <- left_join(read_tsv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Dives_csv/", files[i,], ".csv")) %>%
                    dplyr::select(TagID, Date, Time, Depth) %>%
                    drop_na() %>%
                    mutate(DateTime = as.POSIXct(paste(Date, as.character(Time)), format = "%d/%m/%Y %H:%M:%S", tz="Etc/GMT-6")) %>%
                    dplyr::select(TagID, DateTime, Depth),
                  read_csv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_gps_interp.csv")) %>%
                    dplyr::select(DateTime, Lon, Lat, Dist.km, Trip, TripID),
                  by = "DateTime") %>%
    drop_na(Lat)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
  # Correct drift & do ZOC ####
  
  # Calculate rolling median depth every 30 readings
  offset <- rep(zoo::rollapply(df.dive.locs$Depth, width = k, by = k, FUN = median), each = k)
  
  # Match lengths of offset df and OG depth df
  dif = length(df.dive.locs$Depth) - length(offset)
  offset = c(offset, rep(tail(offset, 1), dif))
  
  # Perform zero-offset correction on depth data
  new_series = df.dive.locs$Depth - offset
  new_series[new_series<0] = 0  # correct meaningless negative depths
  new_series[(length(new_series)-dif):length(new_series)]
  df.dive.locs$Depth_mod = new_series
  
  # Remove outlier
  df.dive.locs = df.dive.locs[which(df.dive.locs$Depth_mod<9),]  %>% # remove outlier
    drop_na() # remove NAs
  
  rm(offset, dif, new_series)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Keep trips with dives only ####
  
  # Split up data frame where value of Trip factor changes:
  df.trips <- split(df.dive.locs, cumsum(1:nrow(df.dive.locs) %in%
                                  which(df.dive.locs$TripID != dplyr::lag(df.dive.locs$TripID))))
  
  # Calculate max depth (& extract Trip ID):
  All.info <- tibble(TripID = c(unlist(lapply(lapply(df.trips, "[[", 8), max))),
                     Max.Depth = c(unlist(lapply(lapply(df.trips, "[[", 3), max))))
  
  # Remove unreal trips
  All.info$TripID <- ifelse(All.info$Max.Depth < 0.4, NA, All.info$TripID)
  
  # Add trip info data back into main df
  Times <- do.call(c, lapply(df.trips, "[", "DateTime"))
  All.info <- All.info %>%
    mutate(StartTime = do.call(c, (lapply(Times, head, 1)))) %>%
    mutate(EndTime = do.call(c, (lapply(Times, tail, 1))))
  All.info <- as.data.table(All.info)
  # Create "end time" for trip data
  dt.dive.locs <- as.data.table(df.dive.locs) %>%
    mutate(EndTime = DateTime + as.numeric(mean(diff(DateTime)))) %>%
    rename("StartTime" = 2)
    # Set key on dt2
  setkey(All.info, StartTime, EndTime)
  # Do the join
  dt.dive.locs <- foverlaps(dt.dive.locs,
                         All.info,
                         type = "any", mult="first") %>%
    dplyr::select(i.StartTime, Lon, Lat, Trip, Dist.km, TripID, Depth_mod) %>%
    dplyr::rename(DateTime = 1) %>%
    mutate(date = format(DateTime, format = "%Y-%m-%d")) %>%
    mutate(date = ymd(date)) %>%
    rename(lat = Lat, lon = Lon) %>%
    drop_na()
  
  rm(df.trips, All.info, Times, df.dive.locs)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Include a 'time of day' column ####
  
  dt.dive.locs <- cbind(dt.dive.locs, getSunlightTimes(data = dt.dive.locs, keep = c("dawn", "sunrise", "sunset", "dusk"), tz = "Etc/GMT-6")[,c(4:7)])
  
  dt.dive.locs$ToD = ifelse(dt.dive.locs$DateTime <= dt.dive.locs$dawn | dt.dive.locs$DateTime > dt.dive.locs$dusk, "Night",
                  ifelse(dt.dive.locs$DateTime <= dt.dive.locs$sunrise & dt.dive.locs$DateTime > dt.dive.locs$dawn, "Dawn",
                         ifelse(dt.dive.locs$DateTime > dt.dive.locs$sunrise & dt.dive.locs$DateTime <= dt.dive.locs$sunset, "Day",
                                ifelse(dt.dive.locs$DateTime > dt.dive.locs$sunset & dt.dive.locs$DateTime <= dt.dive.locs$dusk, "Dusk", NA))))
  
  dt.dive.locs <- dt.dive.locs %>%
    rename(Lat = lat, Lon = lon) %>%
    dplyr::select(DateTime, Lon, Lat, Trip, Dist.km, TripID, Depth_mod, ToD) %>%
    mutate(Dive = ifelse(Depth_mod > 0.1, TRUE, NA))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Save dive plots for each trip ####
  
  # for (j in 1:length(unique(dt.dive.locs$TripID))) {
  # 
  #   # j = 1
  # 
  #   tripj <- unique(dt.dive.locs$TripID)[j]
  # 
  #   ggplot(subset(dt.dive.locs, TripID == tripj & Dive == TRUE)) +
  #     geom_rect(aes(xmin = DateTime, xmax = lead(DateTime), ymin = -Inf, ymax = Inf,
  #                   fill = Hour_shade), alpha = .2) +
  #     geom_point(aes(x = DateTime, y = Depth_mod), alpha = 0.4, show.legend = F) +
  #     theme_light() +
  #     scale_fill_identity()
  # 
  #   ggsave(paste0("RFB_Diving_Plots/", files[i,], "_", j, "dives.png"), width = 8, height = 8)
  # }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Use diveMove to extract dive stats ####
  
  divesTDR <- createTDR(dt.dive.locs$DateTime,
                        depth = dt.dive.locs$Depth_mod,
                        dtime = 1,
                        concurrentData = data.frame(dt.dive.locs[,c(2:3,4:6)]),
                        file = filename, speed=F)
  
  caldivesTDR <- calibrateDepth(divesTDR, dive.thr = 0.1, zoc.method = "offset", offset = 0)
  
  df.dive.locs <- right_join(diveStats(caldivesTDR) %>%
                        dplyr::select(begdesc, divetim, maxdep) %>%
                        rename(DateTime = 1, Dive.Duration = 2, MaxDepth = 3),
                   dt.dive.locs %>%
                     dplyr::select(DateTime, Lon, Lat, Dist.km, TripID, Dive, ToD),
                      by = "DateTime") %>%
    drop_na() %>%
    mutate(PostDiveTime = lead(DateTime) - DateTime)
  
  rm(dt.dive.locs, divesTDR, caldivesTDR)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Classify dive bouts ####
  # Using criteria estimated in script "RFB_Processing_Diving_boutbreakpoints.R" (221 seconds)

  # Split up data frame where value of bout factor changes:
  
  df.dive.bouts <- df.dive.locs %>%
    mutate(Bout = ifelse(df.dive.locs$PostDiveTime < 221 | lag(df.dive.locs$PostDiveTime < 221),
                              "yes", NA)) %>%
    drop_na() %>%
    mutate(PreDiveTime = as.numeric(DateTime - lag(DateTime))) %>%
    mutate(PreDiveTime = replace_na(PreDiveTime, 1)) %>%
    mutate(Bout = cumsum(PreDiveTime >= 221)+1) %>%
    rowwise() %>%
    group_by(Bout) %>% filter(n() > 1) # remove bouts with only 1 dive
  
  # Save this file which includes dives that are in bouts only:
  
  write_csv(df.dive.bouts, paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_dive_stats.csv"))
  
  rm(df.dive.locs)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Save location plots for each trip ####
  
  # for (j in 1:length(unique(df$TripID))) {
  # 
  #   # j = 1
  # 
  #   tripj <- unique(df$TripID)[j]
  # 
  #   ggplot(subset(df, TripID == tripj)) +
  #     geom_point(aes(x = Lon, y = Lat, col = ToD, size = as.factor(Dive)), alpha = 0.4, show.legend = F) +
  #     scale_colour_manual(values = c("#FFB000", "#DC267F")) +
  #     scale_size_manual(values = c(0.2,2)) +
  #     geom_sf(data = chagos, fill = "#009E73", col = "#009E73") +
  #     theme_light()
  # 
  #   ggsave(paste0("RFB_Diving_Plots/", files[i,], "_", j, "map.png"), width = 8, height = 8)
  # }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Extract bout-specific stats ####
  
  # Split up data frame where value of bout factor changes:
  df.bouts <- split(df.dive.bouts, cumsum(1:nrow(df.dive.bouts) %in%
                                           which(df.dive.bouts$Bout != dplyr::lag(df.dive.bouts$Bout))))
  
  # Calculate max depth, bout duration, max dist from the colony, distance from the previous bout and bout ID):
  # Remove bout duration for the one at the start of each trip ####
  
  All.info <- tibble(BoutID = c(unlist(lapply(lapply(df.bouts, "[", 11), max))),
                     TripID = c(unlist(lapply(lapply(df.bouts, "[", 7), max))),
                     Max.Depth = c(unlist(lapply(lapply(df.bouts, "[", 3), max))),
                     N.Dives = c(unlist(lapply(lapply(df.bouts, "[", 1), nrow))),
                     BoutDuration = as.numeric(difftime(as.POSIXct(t(as.data.frame(lapply(sapply(df.bouts, "[", 1), tail, 1)))),
                                                        as.POSIXct(t(as.data.frame(lapply(sapply(df.bouts, "[", 1), head, 1)))),
                                                        units = "secs")),
                     Lat = unlist(lapply(sapply(df.bouts, "[", 5), mean)),
                     Lon = unlist(lapply(sapply(df.bouts, "[", 4), mean)),
                     ColonyDist.km = unlist(lapply(sapply(df.bouts, "[", 6), mean)),
                     ToD = unlist(lapply(sapply(df.bouts, "[", 9), head, 1))) %>%
    mutate(DateTime = do.call(c, (lapply(do.call(c, lapply(df.bouts, "[", "DateTime")), head, 1))))
  
  All.info$InterBoutDist.km = ifelse(All.info$TripID == lag(All.info$TripID),
                                                            distHaversine(cbind(All.info$Lon, All.info$Lat), cbind(lag(All.info$Lon), lag(All.info$Lat)))/1000,
                                     NA)
  
  write_csv(All.info, paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_bout_stats.csv"))
  
}

# beep(5)
