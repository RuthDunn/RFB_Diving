
library(tidyverse)
library(dplyr)
library(rnaturalearth)
library(sf)
library(ggnewscale)
library(viridis)
library(forcats)

chagos = st_read("map_files/Chagos/Chagos_v6.shp")
head(chagos)
chagos$DEPTHLABEL = fct_relevel(chagos$DEPTHLABEL, "land", "shallow", "variable", "deep")
  
chagos_eez = st_read("map_files/Chagos/ChagosEEZ.shp")

booby_img = readJPG("/Users/rob/Dropbox/R/chagos_trip_viewer/white morph red footed booby adult.png", native = TRUE)

bird_data = read.csv("RFB_GPS_all_foragetrips_metadata.csv")
long_trackid = bird_data %>%
  group_by(BirdID, TrackID) %>%
  summarise(
    len = length(Longitude)
  ) %>%
  arrange(len) %>%
  filter(len > 50)

bird_data = subset(bird_data, TrackID %in% long_trackid$TrackID)


bird_data %>%
  group_by(Colony) %>%
  summarise(
    nbirds = length(unique(BirdID)),
  )

bird_data %>%
  group_by(Colony, BirdID) %>%
  summarise(
    ntracks = length(unique(TripID)),
  ) %>%
  group_by(Colony) %>%
  summarise(
    min_tracks = min(ntracks),
    max_tracks = max(ntracks),
    med_tracks = median(ntracks),
  )

colonies = unique(bird_data$Colony)

for (col in colonies) {
  this_colony = subset(bird_data, Colony == col)
  
  birds = unique(this_colony$BirdID)
  
  for (bird in birds) {
    this_bird = subset(this_colony, BirdID == bird)
    
    bird_id = this_bird$BirdID[1]
    ntrip = length(unique(this_bird$TripID))
    
    # Chaotic mess to get evenly spaced segments of trips
    zip <- function(...) {
      mapply(list, ..., SIMPLIFY = FALSE)
    }
    tracks = unique(this_bird$TrackID)
    seqs = list()
    for (i in 1:length(tracks)) {
      # This track
      this_track = subset(this_bird, TrackID == tracks[i]) 
      # Points 7 times along the track
      points = seq(1, (length(this_track$Latitude) - 1), round(length(this_track$Latitude)/7))
      # Drop points at start and end
      points = points[2:(length(points) - 1)]
      # Get next point for each
      points_p1 = points + 1
      # Combine into pairs
      all_points = unlist(zip(points, points_p1))
      # Get values
      Longitude = this_track$Longitude[all_points]
      Latitude = this_track$Latitude[all_points]
      TrackID = this_track$TrackID[all_points]
      seqs[[i]] = data.frame(Longitude, Latitude, TrackID, id = paste(TrackID, sort(rep(seq(1, length(points_p1)), 2))))
    }
    half_way_point = rbindlist(seqs)
    
    file = sprintf("bird_plots/TrackPlot_%s_%s.png", col, bird_id)
    ragg::agg_png(file, width = 1200, height = 1200, units = 'px', res = 120)
    
    g = ggplot() + 
      geom_contour(data = chagos_bathy_fort, aes(x=x, y=y, z=z, color = stat(level)), alpha = 0.1, size = 0.5) +
      geom_text_contour(label.placer = label_placer_random()) + 
      scale_colour_distiller(palette = "Blues") + 
      new_scale_color() +
      geom_path(data = this_colony, aes(x = Longitude, y = Latitude), color = "lightgrey", alpha = 0.5) + 
      geom_sf(data = chagos, aes(group = DEPTHLABEL, color = DEPTHLABEL), fill = NA, alpha = 0.1) + 
      scale_color_brewer(palette = "Greens", type = "seq", direction=-1) + 
      new_scale_color() +
      geom_path(data = this_bird, 
                aes(x = Longitude, y = Latitude, 
                    group = TrackID, color = TrackID)) +
      
      geom_path(data = half_way_point,
                aes(x = Longitude, y = Latitude, group = id, color = TrackID),
                arrow = arrow(type = "closed", length = unit(0.15, "cm"))) +

      scale_color_brewer(palette = "Set1", type = "qual") + 
      theme_bw() + 
      coord_sf(xlim = c(min(this_colony$Longitude) - 0.5, max(this_colony$Longitude)) + 0.5, 
               ylim = c(min(this_colony$Latitude) - 0.5, max(this_colony$Latitude)) + 0.5) + 
      ggtitle(sprintf("Colony: %s, %s, ntrip: %d", col, bird_id, ntrip))
    print(g)
    invisible(dev.off())
    
  }
  
}

library(marmap)
chagos_bathy <- getNOAA.bathy(lon1 = 67, lon2 = 77,
                        lat1 = -3, lat2 = -10, resolution = 4)

plot(chagos_bathy)
plot(chagos_bathy, image = TRUE)
chagos_bathy_fort = fortify(chagos_bathy)
chagos_bathy_r = as.raster(chagos_bathy)
file = sprintf("bird_plots/TrackPlot_%s_%s.png", col, bird_id)
ragg::agg_png(file, width = 1200, height = 1200, units = 'px', res = 120)

g = ggplot() + 
  geom_contour(data = chagos_bathy_fort, aes(x=x, y=y, z=z, color = stat(level)), alpha = 0.1, size = 0.5) +
  scale_colour_distiller(palette = "Blues") + 
  new_scale_color() +
  geom_path(data = this_colony, aes(x = Longitude, y = Latitude), color = "lightgrey", alpha = 0.5) + 
  geom_sf(data = chagos, aes(group = DEPTHLABEL, color = DEPTHLABEL), fill = "grey", alpha = 0.1) + 
  scale_color_brewer(palette = "Blues", type = "seq", direction=-1) + 
  new_scale_color() +
  geom_path(data = this_bird, aes(x = Longitude, y = Latitude, group = TrackID, color = TrackID)) +
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + 
  coord_sf(xlim = c(min(this_colony$Longitude), max(this_colony$Longitude)), ylim = c(min(this_colony$Latitude), max(this_colony$Latitude))) + 
  ggtitle(sprintf("Colony: %s, %s, ntrip: %d", col, bird_id, ntrip))
print(g)
invisible(dev.off())

