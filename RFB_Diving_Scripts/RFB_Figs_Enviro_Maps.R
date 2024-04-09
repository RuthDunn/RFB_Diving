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
library(rerddapXtracto) # for reading in ERDDAP data
library(ggnewscale) # for adding new scales to each facet
library(adehabitatHR) # for plotting utilisation distributions

source("RFB_Diving_Scripts/Functions/meltnc.R") # Function to extract data gained from "rxtracto_3D" function

# Palette info: https://personal.sron.nl/~pault/#sec:qualitativepa

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load data ####

# Chagos shapefile:

chagos <- read_sf("RFB_Diving_Data/Chagos_Maps/chagos_maps/Chagos_v6_land_simple.shp")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Bird data:

dat <- read_csv("RFB_Diving_Data/BIOT_AxyTrek_Processed/AllBirds_Bouts_EnviroData_new.csv") %>%
  dplyr::select(!...1) %>%
  unique() %>%
  filter(Value != "Available.Locs") %>%
  mutate(Date = as.Date(DateTime))

# Create KUDs for plotting:

# kud.dive <- getverticeshr(kernelUD(SpatialPointsDataFrame(dat %>%
#                                                             filter(Value == "Trip.Locs") %>%
#                                                             dplyr::select("X", "Y"),
#                                                           proj4string = CRS("+proj=longlat +datum=WGS84"),
#                                                           dat %>%
#                                                             filter(Value == "Trip.Locs") %>%
#                                                             dplyr::select("BirdID"))),
#                           percent = 90)

# plot(kud.dive)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Environmental data:

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

# Extract enviro data:
xpos <- c(70,76)
ypos <- c(-4,-10)
tpos <- c(unique(dat$Date))

Chlor <- rxtracto_3D(chlorInfo, xcoord = xpos, ycoord = ypos, tcoord = tpos, parameter = 'chlorophyll')
Bathy <- rxtracto_3D(bathyInfo, xcoord = xpos, ycoord = ypos, parameter = 'elevation')
SST <- rxtracto_3D(sstInfo, xcoord = xpos, ycoord = ypos, tcoord = tpos, parameter = 'sstMasked')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create map plot df ####

# Sort out cell widths and heights for the facet_grid function
n = 100

plot.df <- bind_rows(meltnc(Bathy)$data %>%
                    dplyr::rename("Bathy" = elevation) %>%
                    mutate(Variable = "Bathy") %>%
                    mutate(height = (max(latitude) - min(latitude))/n) %>%
                    mutate(width = (max(longitude) - min(longitude))/n) %>%
                      mutate(Chlor = NA) %>%
                      mutate(SST = NA) %>%
                      dplyr::select(longitude, latitude, Variable, Bathy, Chlor, SST, height, width),
                  meltnc(Chlor)$data %>%
                    dplyr::rename("Chlor" = chlorophyll) %>%
                    mutate(Variable = "Chlor") %>%
                    mutate(height = (max(latitude) - min(latitude))/n) %>%
                    mutate(width = (max(longitude) - min(longitude))/n) %>%
                    mutate(Bathy = NA) %>%
                    mutate(SST = NA) %>%
                    dplyr::select(longitude, latitude, Variable, Bathy, Chlor, SST, height, width),
                  meltnc(SST)$data %>%
                    dplyr::rename("SST" = sstMasked) %>%
                    mutate(Variable = "SST") %>%
                    mutate(height = (max(latitude) - min(latitude))/n) %>%
                    mutate(width = (max(longitude) - min(longitude))/n) %>%
                    mutate(Chlor = NA) %>%
                    mutate(Bathy = NA) %>%
                    dplyr::select(longitude, latitude, Variable, Bathy, Chlor, SST, height, width)) %>%
  mutate(longitude = round(longitude, 4)) %>%
  mutate(latitude = round(latitude, 4))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Map plot ####

map.plot <- ggplot() +
  
  # Environmental data:
  geom_tile(data = plot.df, aes(x = longitude, y = latitude, fill = Bathy,
                             height = height, width = width)) +
  scale_fill_gradient(low = "#6699CC", high = "#004488", na.value = NA, guide = guide_colorbar(order = 1, title = element_blank())) +
  new_scale_fill() +
  geom_tile(data = plot.df, aes(x = longitude, y = latitude, fill = Chlor,
                                height = height, width = width)) +
  scale_fill_gradient(low = "#EECC66", high = "#997700", na.value = NA, guide = guide_colorbar(order = 2, title = element_blank())) +
  new_scale_fill() +
  geom_tile(data = plot.df, aes(x = longitude, y = latitude, fill = SST,
  height = height, width = width)) +
  scale_fill_gradient(low = "#EE99AA", high = "#994455", na.value = NA, guide = guide_colorbar(order = 3, title = element_blank())) +
  new_scale_fill() +
  
  # Birds
  geom_path(data = dat[which(dat$Value=='Trip.Locs'),], aes(x = X, y = Y), col = "#000000", alpha = 0.3) +
  geom_point(data = dat[which(dat$Value=='Dive.Locs'),], aes(x = X, y = Y), col = "#000000", size = 2, shape = 21, alpha = 0.2) +

  # Chagos shapefile:
  geom_sf(data = chagos, fill = "#000000", col = "#000000") +
  
  # Theme
  facet_grid(.~Variable) +
  theme_light() %+replace% theme(legend.position="bottom",
                                 strip.background = element_blank(),
                                 strip.text = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 panel.grid.major = element_blank(),
                                 legend.key.height = unit(0.3, "cm"),
                                 legend.margin = margin(-5)) +
  # coord_sf(default_crs = sf::st_crs(4724), expand = T) +          # This messes everything up currently
  ylab("Latitude") +
  xlab("Longitude") +
  annotation_scale(location = "br", width_hint = 0.5)

map.plot

# ggsave("RFB_Diving_Plots/Habitat_map.png", width = 8, height = 3)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(brms) # for extracting brms model outputs

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load model data ####

# Load the raw data so that we can un-scale and un-centre the variables

dat <- read_csv("RFB_Diving_Data/BIOT_AxyTrek_Processed/AllBirds_Bouts_EnviroData_new.csv") %>%
  dplyr::select(!...1) %>%
  unique()

mean.sst <- mean(dat$SST, na.rm = T)
sd.sst <- sd(dat$SST, na.rm = T)
mean.chl <- mean(dat$Chlor, na.rm = T)
sd.chl <- sd(dat$Chlor, na.rm = T)
mean.dep <- mean(dat$Depth)
sd.dep <- sd(dat$Depth)

# Load the final model:

load("RFB_Diving_Data/Habitat_Modelling/Track_vs_Available_Model_new.rds")
track.mod <- mod.depth.chlor.sst
load("RFB_Diving_Data/Habitat_Modelling/Track_vs_Dive_Model.rds")
dive.mod <- mod.depth.chlor.sst
rm(mod.depth.chlor.sst)

# Pull out data frames of conditional effects:

# plot(conditional_effects(dive.mod))

ce.mod <- bind_rows(bind_rows(plot(conditional_effects(track.mod, effects = "sc.SST"))$sc.SST$data %>%
                                dplyr::select(c("sc.SST", "estimate__", "lower__", "upper__")) %>%
                                mutate(Variable = "SST") %>%
                                rename(Value = sc.SST) %>%
                                mutate(Value = (Value * sd.sst) + mean.sst),
                              plot(conditional_effects(track.mod, effects = "sc.Chlor"))$sc.Chlor$data %>%
                                dplyr::select(c("sc.Chlor", "estimate__", "lower__", "upper__")) %>%
                                mutate(Variable = "Chlor") %>%
                                rename(Value = sc.Chlor) %>%
                                mutate(Value = (Value * sd.chl) + mean.chl),
                              plot(conditional_effects(track.mod, effects = "sc.Depth"))$sc.Depth$data %>%
                                dplyr::select(c("sc.Depth", "estimate__", "lower__", "upper__")) %>%
                                mutate(Variable = "Depth") %>%
                                rename(Value = sc.Depth) %>%
                                mutate(Value = (Value * sd.dep) + mean.dep)) %>%
                      mutate(Variable = factor(Variable, levels = c("Depth", "Chlor", "SST"))) %>%
                      mutate(Model = "Track"),
                    bind_rows(plot(conditional_effects(dive.mod, effects = "sc.SST"))$sc.SST$data %>%
                                dplyr::select(c("sc.SST", "estimate__", "lower__", "upper__")) %>%
                                mutate(Variable = "SST") %>%
                                rename(Value = sc.SST) %>%
                                mutate(Value = (Value * sd.sst) + mean.sst),
                              plot(conditional_effects(dive.mod, effects = "sc.Chlor"))$sc.Chlor$data %>%
                                dplyr::select(c("sc.Chlor", "estimate__", "lower__", "upper__")) %>%
                                mutate(Variable = "Chlor") %>%
                                rename(Value = sc.Chlor) %>%
                                mutate(Value = (Value * sd.chl) + mean.chl),
                              plot(conditional_effects(dive.mod, effects = "sc.Depth"))$sc.Depth$data %>%
                                dplyr::select(c("sc.Depth", "estimate__", "lower__", "upper__")) %>%
                                mutate(Variable = "Depth") %>%
                                rename(Value = sc.Depth) %>%
                                mutate(Value = (Value * sd.dep) + mean.dep)) %>%
                      mutate(Variable = factor(Variable, levels = c("Depth", "Chlor", "SST"))) %>%
                      mutate(Model = "zDive"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create model outputs plot ####

facet.names <- as_labeller(c("Depth" = "Bathymetry (m)",
                             "Chlor" = "Chlorophyll-a concentration (mg m\u207B\u00B3)",
                             "SST" = "Sea surface temperature (\u00B0C)",
                             "zDive" = "Dive vs traversed; AUC = 0.75",
                             "Track" = "Traversed vs available; AUC = 0.73"))

plot.dat <- rbind(dat %>%
                    dplyr::select(c("Value", "SST", "Depth", "Chlor")) %>%
                    mutate(estimate__ = as.numeric(dplyr::recode(Value, Dive.Locs = "1", Available.Locs = "0", Trip.Locs = "0"))) %>%
                    unique() %>%
                    mutate(Model = ifelse(Value == "Dive.Locs" | Value == "Trip.Locs", "zDive", "Track")) %>%
                    gather(Variable, Value, SST:Chlor) %>%
                    mutate(Variable = factor(Variable, levels = c("Depth", "Chlor", "SST"))),
                  dat %>%
                    filter(Value == "Trip.Locs") %>%
                    dplyr::select(c("Value", "SST", "Depth", "Chlor")) %>%
                    mutate(estimate__ = as.numeric(dplyr::recode(Value, Trip.Locs = "1"))) %>%
                    mutate(Model = "Track") %>%
                    gather(Variable, Value, SST:Chlor) %>%
                    mutate(Variable = factor(Variable, levels = c("Depth", "Chlor", "SST"))))

model.plot <- ggplot() +
  geom_point(data = plot.dat, aes(x = Value, y = estimate__, col = Variable), alpha = 0.1, size = 0.3) +
  geom_smooth(data = ce.mod, aes(x = Value, y = estimate__,
                                 ymin = lower__, ymax = upper__, col = Variable, fill = Variable),
              stat = "identity", alpha = 0.5) +
  scale_colour_manual(values = c("#004488", "#DDAA33", "#BB5566")) +
  scale_fill_manual(values = c("#004488", "#DDAA33", "#BB5566")) +
  facet_grid(Model~Variable, scales = "free", labeller = facet.names) +
  theme_light() %+replace% theme(legend.position = "none",
                                 strip.background = element_rect(fill = "white", colour = "grey70"),
                                 strip.text = element_text(colour = "black", size = rel(1), hjust = 0.05),
                                 panel.grid.minor = element_blank(),
                                 panel.grid.major = element_blank(),
                                 axis.title.x = element_blank()) +
  ylim(c(0,1)) +
  scale_x_continuous(labels = scales::comma) +
  xlab("Environmental variable") +
  ylab("Probability of habitat use")

model.plot

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Save plots!

g <- ggarrange(model.plot, NULL, map.plot,
          nrow = 3, heights = c(1.5, -0.05, 1))

ggsave("RFB_Diving_Plots/Habitat_fig_map_new.png", plot = g, width = 8, height = 9)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Summary stats ####

summary(track.mod)
summary(dive.mod)

# Pull out some stats for the text:

# Trip & depth
test <- dat[which(dat$Value =='Trip.Locs'),11] %>%
  unique()
mean(test$Depth, na.rm = T)
sd(test$Depth, na.rm = T)

# Trip & chlor
test <- dat[which(dat$Value =='Trip.Locs'),12] %>%
  unique()
mean(test$Chlor, na.rm = T)
sd(test$Chlor, na.rm = T)

# Trip & sst
test <- dat[which(dat$Value =='Trip.Locs'),10] %>%
  unique()
mean(test$SST, na.rm = T)
sd(test$SST, na.rm = T)

# Dives & SST
test <- dat[which(dat$Value =='Dive.Locs'),10] %>%
  unique()
mean(test$SST, na.rm = T)
sd(test$SST, na.rm = T)

# Dives & Chlor
test <- dat[which(dat$Value =='Dive.Locs'),12] %>%
  unique()
mean(test$Chlor, na.rm = T)
sd(test$Chlor, na.rm = T)

