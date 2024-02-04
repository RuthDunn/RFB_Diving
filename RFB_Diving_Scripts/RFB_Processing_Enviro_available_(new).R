
library(tidyverse) # for general working
library(adehabitatHR) # for creating MCPs
library(sf) # for creating sf polygons and points
library(sfheaders) # for creating sf lines
library(rerddapXtracto) # for extracting ERDDAP enviro data at particular dates and locations
library(geosphere) # for distHaversine function

rm(list = ls(all = TRUE))

files <- as.data.frame(list.files(path = "RFB_Diving_Data/BIOT_AxyTrek_Dives_csv/", pattern = "*.csv")) %>%
  separate(1, into = "files", sep = ".csv")

# Download Chagos shapefile to calculate distances
chagos = read_sf("RFB_Diving_Data/Chagos_Maps/chagos_maps/Chagos_v6_land_simple.shp")
diego.garcia = st_coordinates(tail(chagos$geometry, 1))[, c('X', 'Y')]
colnames(diego.garcia) = c('lon', 'lat')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Select the environmental data that I want:
# (Guide: https://cran.r-project.org/web/packages/rerddapXtracto/vignettes/UsingrerddapXtracto.html)
# Data portal: https://upwell.pfeg.noaa.gov/erddap/index.html

# MODIS AQUA L3 SST THERMAL 4KM DAYTIME V2019.0
sstInfo <- rerddap::info('erdMH1sstd8dayR20190SQ') # 8DAY
# sstInfo <- rerddap::info('erdMH1sstdmdayR20190SQ') # Monthly
sstInfo

# The GEBCO_2020 Grid, a continuous terrain model for oceans and land at 15 arc-second intervals
bathyInfo <- rerddap::info('GEBCO_2020')
bathyInfo
# Extracting data from this isn't working for some reason. Try to do this another way instead.

# Chlorophyll-a, Aqua MODIS, NPP, L3SMI, Global, 4km, Science Quality, 2003-present
chlorInfo <- rerddap::info('erdMH1chla8day') # (8 Day Composite)
# chlorInfo <- rerddap::info('erdMH1chlamday') # (Monthly Composite) # data doesn't cover Feb 2022 yet
chlorInfo

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bird.enviro <- NULL
all.enviro <- NULL

k = 50 # (The number of random points to extract)

# Go through all the birds to get a pop-level MCP ####

df.all.gps <- NULL

for (i in 1:nrow(files)) {
  
  # i = 2
  
  df.gps <- read_csv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_gps_interp.csv")) %>%
    unique() %>%
    dplyr::select(Bird, TripID, DateTime, Lat, Lon, Dist.km)
  
  df.all.gps <- rbind(df.all.gps, df.gps)
  
}

df.all.mcp <- mcp(SpatialPoints(coords = cbind(df.all.gps$Lon, df.all.gps$Lat)), percent = 100)

plot(df.all.mcp)
points(df.all.gps$Lon, df.all.gps$Lat, col= "green")

# Extract available enviro data

for (i in 1:nrow(files)) {
  
  # i = 2
  
  # Load data ####
  
  df.dives <- read_csv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_bout_stats.csv")) %>%
    mutate(BirdID = i) %>%
    dplyr::select(BirdID, TripID, BoutID, DateTime, Lat, Lon, ColonyDist.km)
  
  df.gps <- read_csv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_gps_interp.csv")) %>%
    unique() %>%
    dplyr::select(Bird, TripID, DateTime, Lat, Lon, Dist.km)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Create pseudo-absence data ####
  
  # Extract k times the number of points as there are dive points for each trip from:
  # a) The dive track &
  # b) The pop-level MCP
  
  # Run through each trip that has dives in it:
  for (j in 1:length(unique(df.dives$TripID))) {
    
    # j = 1

    # Extract current dive bout locations:
    tripj.divepoints <- subset(df.dives, TripID == unique(df.dives$TripID)[j])[,c("Lat", "Lon", "ColonyDist.km")] %>%
      mutate(Value = "Dive.Locs") %>%
      rename(Y = Lat, X = Lon, Dist.km = ColonyDist.km) %>%
      mutate(BoutNum = row_number()) %>%
      mutate(PointNum = 1)
        
    # Create dataframe of bird id, trip id and date of Trip j, & repeat 101 times:
    # (Change k * 1 to k * 2 if we extract MCP points as well)
    tripj.info <- bind_rows(replicate((k * 2 + 1),
                                      subset(df.dives, TripID == unique(df.dives$TripID)[j])[,c("BirdID", "TripID", "DateTime")], simplify = F))
    
    # Subset the gps track of Trip j:
    tripj <- subset(df.gps, TripID == unique(df.dives$TripID)[j])
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Extract k times as many random points as there are true dive locations
    
    # a) For each trip, extract 'non-dive' locations ####
    
    tripj.pspointsa <- st_line_sample(x = sf_linestring(obj = tripj, x = "Lon", y = "Lat", z = "Dist.km"),
                                      n = nrow(tripj.divepoints)*k, type = "random")
    
    tripj.pspointsa <- as.data.frame(st_coordinates(tripj.pspointsa)[,c(1:3)]) %>%
      mutate(Value = "Trip.Locs") %>%
      rename(Dist.km = Z) %>%
      mutate(BoutNum = rep(c(1:(n()/50)), each = 50)) %>%
      mutate(PointNum = rep(c(1:50), times = n()/50))
    # ~~~~~~~~~~~~~~~~~~~~~~~~
    
    # b)  MCP points ####
    
    tripj.pspointsb <- st_sample(x = st_as_sf(df.all.mcp), size = nrow(tripj.divepoints)*k)

    tripj.pspointsb <- as.data.frame(st_coordinates(tripj.pspointsb)) %>%
      mutate(Dist.km = distHaversine(round(c(diego.garcia[101,'lon'], diego.garcia[101,'lat']), digits = 3),
                                     cbind(X, Y))/1000) %>%
      mutate(Value = "Available.Locs") %>%
      mutate(BoutNum = rep(c(1:(n()/50)), each = 50)) %>%
      mutate(PointNum = rep(c(1:50), times = n()/50))
    
    plot(tripj.pspointsb$X, tripj.pspointsb$Y, col= "green")
    points(tripj.pspointsa$X, tripj.pspointsa$Y, col= "blue")
    points(tripj.divepoints$X, tripj.divepoints$Y, col = "red")
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Combine all different point data frames together and...
    # Extract the environmental data ####
    
    tripj.enviro <- cbind(tripj.info, rbind(tripj.divepoints, tripj.pspointsa, tripj.pspointsb)) %>%
      mutate(SST = rxtracto(sstInfo, parameter = 'sstMasked',
                            xcoord = X, ycoord = Y, tcoord = as.Date(DateTime),
                            xlen = .2, ylen = .2, progress_bar = TRUE)[[1]]) %>%
      mutate(Depth = rxtracto(bathyInfo, parameter = 'elevation',
                            xcoord = X, ycoord = Y, progress_bar = TRUE)[[1]]) %>%
      mutate(Chlor = rxtracto(chlorInfo, parameter = 'chlorophyll',
                              xcoord = X, ycoord = Y, tcoord = as.Date(DateTime),
                              xlen = .2, ylen = .2, progress_bar = TRUE)[[1]])
    
    bird.enviro <- rbind(bird.enviro, tripj.enviro)
    
  }
 
  all.enviro <- rbind(all.enviro, bird.enviro) 
  
}

test <- unique(all.enviro)

# rm(bathyInfo, chlorInfo, sstInfo)

summary(all.enviro)

write.csv(all.enviro, "RFB_Diving_Data/BIOT_AxyTrek_Processed/AllBirds_Bouts_EnviroData_new.csv")
