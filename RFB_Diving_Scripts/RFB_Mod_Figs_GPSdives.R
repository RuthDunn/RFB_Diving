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

chagos = read_sf("RFB_Diving_Data/Chagos_Maps/chagos_maps/Chagos_v6_land_simple.shp")

files <- as.data.frame(list.files(path = "RFB_Diving_Data/BIOT_AxyTrek_Processed/", pattern = "*_gps.csv")) %>%
  separate(1, into = "files", sep = "_gps.csv")

# Palette info: https://personal.sron.nl/~pault/#sec:qualitativepa

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Loop through .csv files to load data ####

# 1) GPS points

df.gps <- NULL

for (i in 1:nrow(files)) {
  
  # i = 1
  
  # Load data ####
  
  df.mini <- read_csv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_gps.csv")) %>%
    dplyr::select(DateTime, Lon, Lat) %>%
    mutate(Year = format(DateTime, format = "%Y"))
  
  df.gps <- rbind(df.gps, df.mini)
  
}

# 2) Dive locations

df.dives <- NULL

for (i in 1:nrow(files)) {
  
  # i = 1
  
  # Load data ####
  
  df.mini <- read_csv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_bout_stats.csv")) %>%
    dplyr::select(DateTime, TripID, N.Dives, BoutDuration, InterBoutDist.km, ColonyDist.km, Lon, Lat) %>%
    mutate(BirdID = i) %>%
    mutate(Year = format(DateTime, format = "%Y"))
  
  df.dives <- rbind(df.dives, df.mini)
  
}

rm(files, i, df.mini)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plots of all trips ####
    
map <- ggplot() +
  geom_path(data = df.gps, aes(x = Lon, y = Lat, col = Year), alpha = 0.7) +
  scale_colour_manual(values = c("#BB5566", "#004488")) +
  geom_point(data = df.dives, aes(x = Lon, y = Lat, col = Year)) +
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
               data = df.dives)

# Look at output
print(log.dur.mod)
bayes_R2(log.dur.mod)

# Check diagnostics:
plot(log.dur.mod, ask = F)
pp_check(log.dur.mod)

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
  annotate("text", x = -.35, y = .8, label = "Effect-size", size = 3, angle = 90)

dur.plots <- ggarrange(dur.points + rremove("xlab"), dur.post + rremove("xlab"), widths = c(1, 0.5))
dur.plots <- annotate_figure(dur.plots, bottom = textGrob("Distance from colony (km)"))

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
  annotate("text", x = -.4, y = .49, label = "Effect-size", size = 3, angle = 90)

dist.post

dist.plots <- ggarrange(dist.points + rremove("xlab"), dist.post + rremove("xlab"), widths = c(1, 0.5))
dist.plots <- annotate_figure(dist.plots, bottom = textGrob("Distance from colony (km)"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Save up plots ####

ggarrange(map,
          ggarrange(dur.plots, dist.plots, nrow = 2),
          ncol = 2,
          widths = c(2,1))

ggsave("RFB_Diving_Plots/Map_fig2.png", width = 8, height = 5.5)
