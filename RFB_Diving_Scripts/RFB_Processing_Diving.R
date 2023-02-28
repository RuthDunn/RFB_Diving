rm(list = ls(all = TRUE))

library(tidyverse) # for piping etc
library(data.table) # for converting df to dt
library(diveMove) # for extracting dive stats
library(oce) # to calculate sun angle
library(lubridate) # for ymd function
library(geosphere) # calculate distances between locations

files <- as.data.frame(list.files(path = "RFB_Diving_Data/BIOT_AxyTrek_Dives_csv/", pattern = "*.csv")) %>%
  separate(1, into = "files", sep = ".csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load raw-ish dive data

# (Loop through .csv files)
# (Select depth cols only)
# (Drop na data)
# (Combine GPS data)
# (Correct for device drift)
#     (Look at every time point in conjunction with the next 30
#      to determine whether the baseline has shifted)

k = 30  # window width for smoothing

for (i in 1:nrow(files)) {
  
  # i = 4
  
  # Load data ####
  
  filename <- paste0("RFB_Diving_Data/BIOT_AxyTrek_Dives_csv/", files[i,], ".csv")
  
  df.dive.locs <- left_join(read_tsv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Dives_csv/", files[i,], ".csv")) %>%
                    dplyr::select(TagID, Date, Time, Depth) %>%
                    drop_na() %>%
                    mutate(DateTime = as.POSIXct(paste(Date, as.character(Time)), format = "%d/%m/%Y %H:%M:%S")) %>%
                    dplyr::select(TagID, DateTime, Depth),
                  read_csv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_gps_interp.csv")) %>%
                    dplyr::select(DateTime, Lon, Lat, Dist.km, Trip, TripID),
                  by = "DateTime") %>%
    drop_na(Lat) %>%
    mutate(EndTime = DateTime + as.numeric(mean(diff(DateTime)))) %>%
    rename("StartTime" = 2)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
  # Correct drift & do ZOC ####
  
  # Calculate rolling median depth every 30 readings
  # Match lengths of offset df and OG depth df
  dif <- length(df.dive.locs$Depth) - length(rep(zoo::rollapply(df.dive.locs$Depth, width = k, by = k, FUN = median), each = k))
  offset <- c(rep(zoo::rollapply(df.dive.locs$Depth, width = k, by = k, FUN = median), each = k),
             rep(tail(rep(zoo::rollapply(df.dive.locs$Depth, width = k, by = k, FUN = median), each = k), 1),
                 dif))
  
  # Perform zero-offset correction on depth data
  new_series <- df.dive.locs$Depth - offset
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
  All.info <- data.table(tibble(TripID = c(unlist(lapply(lapply(df.trips, "[[", 8), max))),
                     Max.Depth = c(unlist(lapply(lapply(df.trips, "[[", 3), max)))) %>%
    mutate(TripID = ifelse(Max.Depth < 0.4, NA, TripID)) %>%
    mutate(StartTime = do.call(c, (lapply(do.call(c, lapply(df.trips, "[", "StartTime")), head, 1)))) %>%
    mutate(EndTime = do.call(c, (lapply(do.call(c, lapply(df.trips, "[", "StartTime")), tail, 1)))))
  
  # Add trip info data back into main df
  # Set key
  setkey(All.info, StartTime, EndTime)
  # Do the join
  # (Also include sun angle calculation)
  dt.dive.locs <- foverlaps(as.data.table(df.dive.locs),
                         All.info,
                         type = "any", mult="first") %>%
    dplyr::select(i.StartTime, Lon, Lat, Trip, Dist.km, TripID, Depth_mod) %>%
    dplyr::rename(DateTime = 1) %>%
    mutate(date = format(DateTime, format = "%Y-%m-%d")) %>%
    drop_na() %>%
    mutate(Sun.Alt = sunAngle(t = DateTime, longitude = Lon, latitude = Lat)[[3]]) %>%
    mutate(Dive = ifelse(Depth_mod > 0.1, TRUE, NA))
    
  rm(df.trips, All.info, df.dive.locs)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Save dive plots for each trip ####
  
  # for (j in 1:length(unique(dt.dive.locs$TripID))) {
  # 
  #   # j = 1
  # 
  #   tripj <- unique(dt.dive.locs$TripID)[j]
  # 
  #   ggplot(subset(dt.dive.locs, TripID == tripj & Dive == TRUE)) +
  #     # geom_rect(aes(xmin = DateTime, xmax = lead(DateTime), ymin = -Inf, ymax = Inf), alpha = .2) +
  #     geom_point(aes(x = DateTime, y = Depth_mod, col = Sun.Alt)) +
  #     theme_light() +
  #     scale_fill_identity()
  # 
  #   ggsave(paste0("RFB_Diving_Plots/", files[i,], "_", j, "dives.png"), width = 8, height = 8)
  # }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Extract dive stats ####
  
  t.diff <- median(as.numeric(dt.dive.locs$DateTime - lag(dt.dive.locs$DateTime)), na.rm = T)
  
  df.dives <- dt.dive.locs %>%
    mutate(Dive = ifelse(Depth_mod > 0.1,           # 10 cm cut-off
                         "yes", NA)) %>%
    drop_na() %>%
    mutate(PreDiveTime = as.numeric(DateTime - lag(DateTime))) %>%
    mutate(PreDiveTime = replace_na(PreDiveTime, t.diff)) %>%
    mutate(DiveID = cumsum(PreDiveTime > t.diff)+1) %>%
    rowwise()
  
  rm(dt.dive.locs)
  
  # Split up data frame where value of DiveID changes:
  
  df.dives.split <- split(df.dives, cumsum(1:nrow(df.dives) %in%
                                                  which(df.dives$DiveID != dplyr::lag(df.dives$DiveID))))
  
  # Calculate dive max depth & duration

  df.dives2 <- right_join(tibble(DiveID = c(unlist(lapply(lapply(df.dives.split, "[", 11), max))),
                                 DiveDuration_s = as.numeric(difftime(as.POSIXct(t(as.data.frame(lapply(sapply(df.dives.split, "[", 1), tail, 1)))),
                                                                      as.POSIXct(t(as.data.frame(lapply(sapply(df.dives.split, "[", 1), head, 1)))),
                                                                      units = "secs")) + t.diff,
                                 MaxDepth_cm = c(unlist(lapply(lapply(df.dives.split, "[", 7), max)))) %>%
                            mutate(DateTime = do.call(c, (lapply(do.call(c, lapply(df.dives.split, "[", "DateTime")), head, 1)))),
                          df.dives %>%
                            dplyr::select(DateTime, Lon, Lat, Dist.km, TripID, Dive, Sun.Alt),
                          by = "DateTime") %>%
    drop_na() %>%
    mutate(PostDiveTime = lead(DateTime) - DateTime)
  
  rm(df.dives.split, df.dives)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Classify dive bouts ####
  # Using criteria estimated in script "RFB_Processing_Diving_boutbreakpoints.R" (221 seconds)

  # Split up data frame where value of bout factor changes:
  
  df.dive.bouts <- df.dives2 %>%
    mutate(Bout = ifelse(df.dives2$PostDiveTime < 221 | lag(df.dives2$PostDiveTime < 221),
                              "yes", NA)) %>%
    drop_na() %>%
    mutate(PreDiveTime = as.numeric(DateTime - lag(DateTime))) %>%
    mutate(PreDiveTime = replace_na(PreDiveTime, 1)) %>%
    mutate(Bout = cumsum(PreDiveTime >= 221)+1) %>%
    rowwise() %>%
    group_by(Bout) %>% filter(n() > 1) # remove bouts with only 1 dive
  
  # Save this file which includes dives that are in bouts only:
  
  write_csv(df.dive.bouts, paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_dive_stats.csv"))
  
  rm(df.dives2)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Extract bout-specific stats ####
  
  # Split up data frame where value of bout factor changes:
  df.bouts <- split(df.dive.bouts, cumsum(1:nrow(df.dive.bouts) %in%
                                           which(df.dive.bouts$Bout != dplyr::lag(df.dive.bouts$Bout))))
  
  # Calculate max depth, bout duration, max dist from the colony, distance from the previous bout and bout ID):
  # Remove bout duration for the one at the start of each trip ####
  
  All.info <- tibble(BoutID = c(unlist(lapply(lapply(df.bouts, "[", 12), max))),
                     TripID = c(unlist(lapply(lapply(df.bouts, "[", 8), max))),
                     Max.Depth = c(unlist(lapply(lapply(df.bouts, "[", 3), max))),
                     N.Dives = c(unlist(lapply(lapply(df.bouts, "[", 1), nrow))),
                     BoutDuration = as.numeric(difftime(as.POSIXct(t(as.data.frame(lapply(sapply(df.bouts, "[", 4), tail, 1)))),
                                                        as.POSIXct(t(as.data.frame(lapply(sapply(df.bouts, "[", 4), head, 1)))),
                                                        units = "secs")),
                     Lat = unlist(lapply(sapply(df.bouts, "[", 6), mean)),
                     Lon = unlist(lapply(sapply(df.bouts, "[", 5), mean)),
                     ColonyDist.km = unlist(lapply(sapply(df.bouts, "[", 7), mean)),
                     ToD = unlist(lapply(sapply(df.bouts, "[", 10), head, 1))) %>%
    mutate(DateTime = do.call(c, (lapply(do.call(c, lapply(df.bouts, "[", "DateTime")), head, 1))))
  
  All.info$InterBoutDist.km = ifelse(All.info$TripID == lag(All.info$TripID),
                                                            distHaversine(cbind(All.info$Lon, All.info$Lat), cbind(lag(All.info$Lon), lag(All.info$Lat)))/1000,
                                     NA)
  
  write_csv(All.info, paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_bout_stats.csv"))
  
}

# beep(5)
