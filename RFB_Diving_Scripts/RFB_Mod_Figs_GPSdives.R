rm(list = ls(all = TRUE))

# if (!requireNamespace("remotes")) {
#   install.packages("remotes")
# }
# remotes::install_github("paul-buerkner/brms")

library(tidyverse) # for piping etc
library(sf) # for loading and plotting the Chagos shapefile
library(ggspatial) # to plot a scale bar
library(ggpubr) # to plot figs side by side
library(brms) # for Bayesian modelling
library(tidybayes) # to dig into model outputs
library(grid) # for sharing axes across plots
library(data.table) # for rbindlist() function

chagos = read_sf("RFB_Diving_Data/Chagos_Maps/chagos_maps/Chagos_v6_land_simple.shp")

files <- as.data.frame(list.files(path = "RFB_Diving_Data/BIOT_AxyTrek_Processed/", pattern = "*_gps.csv")) %>%
  separate(1, into = "files", sep = "_gps.csv")

# Palette info: https://personal.sron.nl/~pault/#sec:qualitativepa

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Loop through .csv files to load data ####

# 1) GPS points ####

df.gps <- NULL

# half_way_points <- NULL

for (i in 1:nrow(files)) {
  
  # i = 1
  
  df.mini <- read_csv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_gps.csv")) %>%
    dplyr::select(DateTime, Lon, Lat, TripID, Bird) %>%
    mutate(Year = format(DateTime, format = "%Y")) %>%
    na.omit()
  
  df.gps <- rbind(df.gps, df.mini)
  
  # Robin's function to add trip direction arrows ####
  
  # for (bird in 1:length(unique(df.gps$Bird))) {
  #   
  #   # bird = 1
  #   
  #   # Extract one bird of data
  #   this_bird = subset(df.gps, Bird == unique(df.gps$Bird)[bird])
  #   
  #   # Extract the info that we'll need later
  #   bird_id = this_bird$Bird[1]
  #   ntrip = length(unique(this_bird$TripID))
  #   Year = this_bird$Year[1]
  #   
  #   zip <- function(...) {
  #     mapply(list, ..., SIMPLIFY = FALSE)
  #   }
  #   seqs = list()
  #   
  #   for (i in 1:length(unique(this_bird$TripID))) {
  #     
  #     # i = 1
  #     
  #     # Extract one track
  #     this_track = subset(this_bird, TripID == unique(this_bird$TripID)[i])
  #     
  #     # Points 4 times along the track
  #     points = seq(1, (length(this_track$Lat) - 1), round(length(this_track$Lat)/4))
  #     # Drop points at start and end
  #     points = points[2:(length(points) - 1)]
  #     # Get next point for each
  #     points_p1 = points + 1
  #     # Combine into pairs
  #     all_points = unlist(zip(points, points_p1))
  #     # Get values
  #     Lon = this_track$Lon[all_points]
  #     Lat = this_track$Lat[all_points]
  #     TripID = this_track$TripID[all_points]
  #     seqs[[i]] = data.frame(Lon, Lat, bird_id, TripID, Year, id = paste(bird_id, TripID, sort(rep(seq(1, length(points_p1)), 2))))
  #   }
  #   half_way_point = rbindlist(seqs)
  #   
  #   half_way_points <- rbind(half_way_points, half_way_point)
  #   
  # }
  
}

# rm(this_bird, bird_id, ntrip, zip, i, this_track, points, points_p1,
#    all_points, Lon, Lat, TripID, seqs, bird, Year, half_way_point)

# 2) Dive locations ####

df.dives <- NULL

for (i in 1:nrow(files)) {
  
  # i = 1
  
  df.mini <- read_csv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_bout_stats.csv")) %>%
    dplyr::select(DateTime, TripID, N.Dives, BoutDuration, InterBoutDist.km, ColonyDist.km, Lon, Lat) %>%
    mutate(BirdID = i) %>%
    mutate(Year = format(DateTime, format = "%Y"))
  
  df.dives <- rbind(df.dives, df.mini)
  
}

rm(files, i, df.mini)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Pull out overview metrics for manuscript ####

# N dives
sum(df.dives$N.Dives)
# N bouts
nrow(df.dives)
# N trips
nlevels(unique(as.factor(paste(df.dives$BirdID, df.dives$TripID))))
# N birds
nlevels(as.factor(df.dives$BirdID))
# min and max and mean of dives per bout
summary(df.dives$N.Dives)
sd(df.dives$N.Dives)

# Min and max and mean dist from colony that foraging took place at
summary(df.dives$ColonyDist.km)
# Also sd:
sd(df.dives$ColonyDist.km)

# Foraging bout duration stats:
hist(df.dives$BoutDuration)
summary(df.dives$BoutDuration)
sd(df.dives$BoutDuration)
# What % are < 3 min
test <- df.dives %>%
  mutate(calc = ifelse(df.dives$BoutDuration < 180, 1, NA)) %>%
  na.omit()
(nrow(test) * 100)/nrow(df.dives)

# Inter-foraging bout distance stats:
hist(df.dives$InterBoutDist.km)
summary(df.dives$InterBoutDist.km)
sd(df.dives$InterBoutDist.km, na.rm = T)
# What % are < 2 km
test2 <- df.dives %>%
  mutate(calc = ifelse(df.dives$InterBoutDist.km < 2, 1, NA)) %>%
  na.omit()
(nrow(test2) * 100)/nrow(df.dives)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plots of all trips ####
    
map <- ggplot() +
  geom_path(data = df.gps, aes(x = Lon, y = Lat, col = Year), alpha = 0.7) +
  
  # geom_path(data = half_way_points,
  #           aes(x = Lon, y = Lat, group = id, col = Year),
  #           arrow = arrow(type = "closed", length = unit(0.3, "cm"))) +
  
  scale_colour_manual(values = c("#BB5566", "#004488")) +
  geom_point(data = df.dives, aes(x = Lon, y = Lat, col = Year, size = N.Dives), alpha = 0.6) +
  scale_size_binned(name = "Dives per bout", range = c(0.1,5)) +
  geom_sf(data = chagos, fill = "#000000", col = "#000000") +
  theme_light() %+replace% theme(legend.position="bottom") +
  coord_sf(default_crs = sf::st_crs(4724), expand = T) +
  ylab("Latitude") +
  xlab("Longitude") +
  annotation_scale(location = "br", width_hint = 0.5)

map

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Run model of bout duration vs distance from colony ####

# log-transform stuff

df.dives <- df.dives %>%
  mutate(logBoutDur = log(BoutDuration)) %>%
  mutate(logColonyDist = log(ColonyDist.km)) %>%
  drop_na()

log.dur.mod <- brm(logBoutDur ~ 0 + logColonyDist +
                 (1|BirdID|TripID),
               data = df.dives, control = list(adapt_delta = 0.85))

# Look at output
print(log.dur.mod)
bayes_R2(log.dur.mod)

# Check diagnostics:
plot(log.dur.mod, ask = F)
pp_check(log.dur.mod)

# What proportion of the posterior distribution is above 0?
hypothesis(log.dur.mod, "logColonyDist<0")

# Plot bout duration model results

# Data frame of conditional effects:
ce.dur.mod <- plot(conditional_effects(log.dur.mod, effects = "logColonyDist",
                                       re_formula = NULL)) # Incorporate random effects variance
ce.dur.mod <- ce.dur.mod$logColonyDist$data

dur.points <- ggplot() +
  geom_smooth(data = ce.dur.mod, aes(x = exp(logColonyDist), y = exp(estimate__),
                                     ymin = exp(lower__), ymax = exp(upper__)),
              stat = "identity", alpha = 0.2, col = "#004488") +
  geom_point(data = df.dives, aes(x = ColonyDist.km, y = BoutDuration), alpha = 0.2, col = "#004488") +
  theme_light() %+replace% theme(legend.position = "bottom", legend.margin = margin(t = -10),
                                 strip.background = element_rect(fill = "white", colour = "grey70"),
                                 strip.text = element_text(colour = "black", size = rel(0.9), hjust = 0.05)) +
  ylab("Bout duration (s)") +
  xlab("Dist from DG (km)") +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10")
  
dur.post <- ggplot(as_draws_df(log.dur.mod)) +
  stat_eye(aes(y = b_logColonyDist,  x = 0), point_interval=median_hdi, fill = "#004488", color = "#004488",
               fatten_point = 2, slab_alpha = .2) +
  geom_hline(yintercept = 0, lty = 2) +
  theme_light() %+replace% theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
                                 axis.ticks.x = element_blank(), axis.text.x = element_blank(),
                                 axis.title.y = element_blank()) +
  xlab("") +
  annotate("text", x = -.35, y = .85, label = "Effect-size", size = 3, angle = 90)

dur.plots <- ggarrange(dur.points + rremove("xlab"), dur.post,
                       widths = c(1, 0.5))
# dur.plots <- annotate_figure(dur.plots, bottom = textGrob("Distance from colony (km)"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Run model of inter-bout distance vs distance from colony ####

# log-transform stuff

df.dives <- df.dives %>%
  mutate(logBoutDist = log(InterBoutDist.km)) %>%
  drop_na()

log.dist.mod <- brm(logBoutDist ~ 0 + logColonyDist +
                     (1|BirdID|TripID),
                   data = df.dives,
                   control = list(adapt_delta = 0.999, max_treedepth = 9))

# Look at output
print(log.dist.mod)
bayes_R2(log.dist.mod)

# Check diagnostics:
plot(log.dist.mod, ask = F)
pp_check(log.dist.mod)

# What proportion of the posterior distribution is above 0?
hypothesis(log.dist.mod, "logColonyDist<0")

# Data frame of conditional effects:
ce.dist.mod <- plot(conditional_effects(log.dist.mod, effects = "logColonyDist",
                                       re_formula = NULL)) # Incorporate random effects variance
ce.dist.mod <- ce.dist.mod$logColonyDist$data

dist.points <- ggplot() +
  geom_smooth(data = ce.dist.mod, aes(x = exp(logColonyDist), y = exp(estimate__),
                                     ymin = exp(lower__), ymax = exp(upper__)),
              stat = "identity", alpha = 0.2, col = "#BB5566") +
  geom_point(data = df.dives, aes(x = ColonyDist.km, y = InterBoutDist.km), alpha = 0.2, col = "#BB5566") +
  theme_light() %+replace% theme(legend.position = "bottom", legend.margin = margin(t = -10),
                                 strip.background = element_rect(fill = "white", colour = "grey70"),
                                 strip.text = element_text(colour = "black", size = rel(0.9), hjust = 0.05)) +
  ylab("Inter-bout distance (km)") +
  xlab("Dist from DG (km)") +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10")

dist.post <- ggplot(as_draws_df(log.dist.mod)) +
  stat_eye(aes(y = b_logColonyDist,  x = 0), point_interval=median_hdi, fill = "#BB5566", color = "#BB5566",
               fatten_point = 2, slab_alpha = .2) +
  geom_hline(yintercept = 0, lty = 2) +
  theme_light() %+replace% theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
                                 axis.ticks.x = element_blank(), axis.text.x = element_blank(),
                                 axis.title.y = element_blank()) +
  xlab("") +
  annotate("text", x = -.4, y = .55, label = "Effect-size", size = 3, angle = 90)

dist.post

dist.plots <- ggarrange(dist.points + rremove("xlab"), dist.post,
                        widths = c(1, 0.5))
dist.plots <- annotate_figure(dist.plots, bottom = textGrob("Distance from colony (km)"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Save up plots ####

ggarrange(map,
          ggarrange(dur.plots, dist.plots, nrow = 2, heights = c(1.01, 1)),
          ncol = 2,
          widths = c(2,1))

ggsave("RFB_Diving_Plots/Map_fig2.png", width = 8, height = 5.5)
