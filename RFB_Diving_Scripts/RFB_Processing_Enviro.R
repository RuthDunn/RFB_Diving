
library(tidyverse) # for general working
library(adehabitatHR) # for creating MCPs
library(sf) # for creating sf polygons and points
library(sfheaders) # for creating sf lines
library(rerddapXtracto) # for extracting ERDDAP enviro data at particular dates and locations

rm(list = ls(all = TRUE))

files <- as.data.frame(list.files(path = "RFB_Diving_Data/BIOT_AxyTrek_Dives_csv/", pattern = "*.csv")) %>%
  separate(1, into = "files", sep = ".csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Select the environmental data that I want:
# (Guide: https://cran.r-project.org/web/packages/rerddapXtracto/vignettes/UsingrerddapXtracto.html)

# MODIS AQUAL daytime SST data:
sstInfo <- rerddap::info('nasa_jpl_1ce2_ad3f_9bae')
sstInfo

# GEBCO bathymetry data:
bathyInfo <- rerddap::info('GEBCO_2020_Lon0360')
bathyInfo

# 
chlorInfo <- rerddap::info('erdMH1chla1day')
chlorInfo

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bird.enviro <- NULL
all.enviro <- NULL

k = 50 # (The number of random points to extract)

for (i in 1:nrow(files)) {
  
  # i = 1
  
  # Load data ####
  
  df.dives <- read_csv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_bout_stats.csv")) %>%
    mutate(BirdID = i)
  
  df.gps <- read_csv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_gps_interp.csv"))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Create pseudo-absence data ####
  
  # Extract k times the number of points as there are dive points for each trip from:
  # a) The dive track &
  # b) The MCP.
  
  # Run through each trip that has dives in it:
  for (j in 1:length(unique(df.dives$TripID))) {
    
    # j = 2

    # Extract current dive locations:
    tripj.divepoints <- subset(df.dives, TripID == unique(df.dives$TripID)[j])[,c("Lat", "Lon")] %>%
      mutate(Value = "Dive.Locs") %>%
      rename(X = 1, Y = 2) %>%
      mutate(N = row_number())
        
    # Create dataframe of bird id, trip id and date of Trip j, & repeat 101 times:
    # (Change k * 1 to k * 2 if we extract MCP points as well)
    tripj.info <- bind_rows(replicate((k * 1 + 1),
                                      subset(df.dives, TripID == unique(df.dives$TripID)[j])[,c("BirdID", "TripID", "DateTime")], simplify = F))
    
    # Subset the gps track of Trip j:
    df.gps <- df.gps[complete.cases(df.gps), ]
    tripj <- subset(df.gps, TripID == unique(df.dives$TripID)[j])
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Extract k times as many random points as there are true dive locations
    
    # a) For each trip, extract 'non-dive' locations ####
    
    tripj.pspointsa <- st_line_sample(x = sf_linestring(obj = df.gps, x = "Lon", y = "Lat"),
                                      n = nrow(tripj.divepoints)*k, type = "random")
    
    tripj.pspointsa <- as.data.frame(st_coordinates(tripj.pspointsa)[,c(1:2)]) %>%
      mutate(Value = "Trip.Locs") %>%
      mutate(N = row_number())
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~
    
    # b)  MCP points ####
    
    # Create 95% MCP
    
    # tripj.pspointsb <- st_sample(x = st_as_sf(mcp(SpatialPoints(coords = cbind(tripj$Lon, tripj$Lat)), percent = 95)),
    # size = nrow(tripj.divepoints)*k)
    # 
    # tripj.pspointsb <- as.data.frame(st_coordinates(tripj.pspointsb)) %>%
    # mutate(Value = "Available.Locs") %>%
    # mutate(N = row_number())
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Combine all different point data frames together and...
    # Extract the environmental data ####
    
    tripj.enviro <- cbind(tripj.info, rbind(tripj.divepoints, tripj.pspointsa)) %>%
      mutate(SST = rxtracto(sstInfo, parameter = 'sst',
                            xcoord = X, ycoord = Y, tcoord = as.Date(DateTime),
                            xlen = .2, ylen = .2, progress_bar = TRUE)[[1]]) %>%
      mutate(Depth = rxtracto(bathyInfo, parameter = 'elevation',
                            xcoord = X, ycoord = Y,
                            xlen = .2, ylen = .2, progress_bar = TRUE)[[1]]) %>%
      mutate(Chlor = rxtracto(chlorInfo, parameter = 'chlorophyll',
                              xcoord = X, ycoord = Y, tcoord = as.Date(DateTime),
                              xlen = .2, ylen = .2, progress_bar = TRUE)[[1]])
    
    bird.enviro <- rbind(bird.enviro, tripj.enviro)
    
  }
 
  rm(tripj.divepoints, tripj.pspointsa, tripj.pspointsb, tripj, tripj.info)
  
  all.enviro <- rbind(all.enviro, bird.enviro) 
  
}

# rm(bathyInfo, chlorInfo, sstInfo)

write.csv(all.enviro, "RFB_Diving_Data/BIOT_AxyTrek_Processed/AllBirds_EnviroData.csv")
