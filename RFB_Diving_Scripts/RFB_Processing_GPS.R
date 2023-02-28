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

# chagos = read_sf("RFB_Diving_Data/Chagos_Maps/chagos_maps/Chagos_v6_land_simple.shp")
# diego.garcia = st_coordinates(tail(chagos$geometry, 1))[, c('X', 'Y')]
# colnames(diego.garcia) = c('lon', 'lat')

files <- as.data.frame(list.files(path = "RFB_Diving_Data/BIOT_AxyTrek_GPS_txt/",
                                  pattern = "*.txt")) %>%
  separate(1, into = "files", sep = ".txt")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Loop through all bird dive files ####

for (j in 1:nrow(files)) {
  
  # j = 1 # use to test code
  
  # Load files ####
  
  # (Determine nest coordinates as the most common GPS)
  # (Calc dists from nest)
  # (>1 km from colony = trip)
  
  df.gps <- read_tsv(paste0("RFB_Diving_Data/BIOT_AxyTrek_GPS_txt/", files[j,], ".txt"), col_names = F) %>%
    dplyr::select(X1:X4) %>%
    drop_na() %>%
    dplyr::rename(Date = 1, Time = 2, Lat = 3, Lon = 4) %>%
    mutate(Bird = files[j,]) %>%
    mutate(Date = dmy(Date)) %>%
    mutate(DateTime = as.POSIXct(paste(Date, as.character(Time)), format = "%Y-%m-%d %H:%M:%S")) %>%
    mutate(Dist.km = distHaversine(round(c(getmode(Lon), getmode(Lat)), digits = 3),
                                   cbind(Lon, Lat))/1000) %>%
    mutate(Trip = ifelse(Dist.km > 1,
                         TRUE,
                         FALSE)) %>%
    mutate(EndTime = DateTime + as.numeric(mean(diff(DateTime)))) %>%
    dplyr::rename(StartTime = 6)
  
  # ~~~~~~~~
  
  # Assign trip IDs and re-assign short (< 30 min) trips ####
  
  # Split up data frame where value of Trip factor changes:
  
  split <- split(df.gps, cumsum(1:nrow(df.gps) %in%
                                     which(df.gps$Trip != dplyr::lag(df.gps$Trip))))
  
  # Combine trip metric info
  # (Extract whether time period is a trip or at the nest)
  # (Calculate duration of time period)
  # (Calculate max dist from the nest)
  # (If trip < 30 mins, make non-trip)
  # (If trip < 1 km, make non-trip)
  
  All.info <- tibble(Trip = as.character(unlist(lapply(split, "[", 1, "Trip"))),
              Duration = unname(unlist(lapply(split, nrow)) * as.numeric(mean(diff(df.gps$StartTime)))),
              Max.Dist = unlist(lapply(lapply(split, "[[", 5), max))) %>%
    add_rownames(var = "Row.n") %>%
    mutate(Trip = ifelse(Trip == "TRUE" &
                           Duration < 1800 | Max.Dist < 1,
                         "FALSE", Trip))
  
  # Assign trip IDs to "TRUE" trips:
  
  All.info2 <- data.table(bind_rows(All.info[which(All.info$Trip=='FALSE'),] %>%
                          mutate(TripID = NA), 
                        All.info[which(All.info$Trip=='TRUE'),] %>%
                          add_rownames(var = "TripID")) %>%
    arrange(Row.n) %>%
    mutate(StartTime = do.call(c, (lapply(do.call(c, lapply(split, "[", "StartTime")), head, 1)))) %>%
    mutate(EndTime = do.call(c, (lapply(do.call(c, lapply(split, "[", "StartTime")), tail, 1)))))
  
  rm(All.info, split)
  
  # Add trip info data back into normal data:
  
  setkey(All.info2, StartTime, EndTime)
  df.gps2 <- foverlaps(as.data.table(df.gps),
                         All.info2,
                         type = "any", mult="first") %>%
    dplyr::select(i.StartTime, Lon, Lat, Trip, Dist.km, TripID, Bird) %>%
    dplyr::rename(DateTime = 1)
  
  # ~~~~~~~~
  
  # Write file ####
  
  write_csv(df.gps2, paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[j,], "_gps.csv"))

  # ~~~~~~~~
  
  # Interpolation ####
  
  df.gps <- ld(redisltraj(as.ltraj(data.frame(cbind(df.gps2$Lon, df.gps2$Lat)),
                                df.gps2$DateTime, df.gps2$Bird),
                       u = 1, type = "time")) %>%
    dplyr::select(x:date) %>%
    dplyr::rename(Lon = 1, Lat = 2, DateTime = 3) %>%
    mutate(Bird = files[j,]) %>%
    mutate(Dist.km = distHaversine(round(c(getmode(Lon), getmode(Lat)), digits = 3),
                                   cbind(Lon, Lat))/1000) %>%
    mutate(Trip = ifelse(Dist.km > 1,
                         TRUE,
                         FALSE)) %>%
    mutate(EndTime = DateTime + as.numeric(mean(diff(DateTime)))) %>%
    dplyr::rename(StartTime = 3)
  
  rm(df.gps2, All.info2)
  
  # ~~~~~~~~
  
  # Assign trip IDs and re-assign short (< 30 min) trips ####
  
  # Split up data frame where value of Trip factor changes:
  
  split <- split(df.gps, cumsum(1:nrow(df.gps) %in%
                                  which(df.gps$Trip != dplyr::lag(df.gps$Trip))))
  
  # Combine trip metric info
  # (Extract whether time period is a trip or at the nest)
  # (Calculate duration of time period)
  # (Calculate max dist from the nest)
  # (If trip < 30 mins, make non-trip)
  # (If trip < 1 km, make non-trip)
  
  All.info <- tibble(Trip = as.character(unlist(lapply(split, "[", 1, "Trip"))),
                     Duration = unname(unlist(lapply(split, nrow)) * as.numeric(mean(diff(df.gps$StartTime)))),
                     Max.Dist = unlist(lapply(lapply(split, "[[", 5), max))) %>%
    add_rownames(var = "Row.n") %>%
    mutate(Trip = ifelse(Trip == "TRUE" &
                           Duration < 1800 | Max.Dist < 1,
                         "FALSE", Trip))
  
  # Assign trip IDs to "TRUE" trips:
  
  All.info2 <- data.table(bind_rows(All.info[which(All.info$Trip=='FALSE'),] %>%
                                      mutate(TripID = NA), 
                                    All.info[which(All.info$Trip=='TRUE'),] %>%
                                      add_rownames(var = "TripID")) %>%
                            arrange(Row.n) %>%
                            mutate(StartTime = do.call(c, (lapply(do.call(c, lapply(split, "[", "StartTime")), head, 1)))) %>%
                            mutate(EndTime = do.call(c, (lapply(do.call(c, lapply(split, "[", "StartTime")), tail, 1)))))
  
  rm(All.info, split)
  
  # Add trip info data back into normal data:
  
  setkey(All.info2, StartTime, EndTime)
  df.gps2 <- foverlaps(as.data.table(df.gps),
                       All.info2,
                       type = "any", mult="first") %>%
    dplyr::select(i.StartTime, Lon, Lat, Trip, Dist.km, TripID, Bird) %>%
    dplyr::rename(DateTime = 1)
  
  rm(df.gps, All.info2)
  
  # ~~~~~~~~
  
  # Write file ####
  
  write_csv(df.gps2, paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[j,], "_gps_interp.csv"))
  
  }

rm(All.info, Colony.info, Trip.info, df.gps, split, Times, tr, Duration, nest.coords, Trip, j)
