rm(list = ls(all = TRUE))

library(tidyverse)
library(brms)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load the raw data so that we can un-scale and un-centre the variables

dat <- read_csv("RFB_Diving_Data/Habitat_Modelling//AllBirds_Bouts_EnviroData.csv") %>%
  dplyr::select(!...1) %>%
  unique()

mean.sst <- mean(dat$SST, na.rm = T)
sd.sst <- sd(dat$SST, na.rm = T)
mean.chl <- mean(dat$Chlor, na.rm = T)
sd.chl <- sd(dat$Chlor, na.rm = T)
mean.dep <- mean(dat$Depth)
sd.dep <- sd(dat$Depth)

# Load the final model:

load("RFB_Diving_Data/Habitat_Modelling/Track_vs_Available_Model.Rdata")
track.mod <- mod.depth.chlor.sst
load("RFB_Diving_Data/Habitat_Modelling/Track_vs_Dive_Model.Rdata")
dive.mod <- mod.depth.chlor.sst
rm(mod.depth.chlor.sst)

# Pull out data frames of conditional effects:

# plot(conditional_effects(dive.mod))

ce.mod <- bind_rows(bind_rows(plot(conditional_effects(track.mod, effects = "sc.SST"))$sc.SST$data %>%
                                select(c("sc.SST", "estimate__", "lower__", "upper__")) %>%
                                mutate(Variable = "SST") %>%
                                rename(Value = sc.SST) %>%
                                mutate(Value = (Value * sd.sst) + mean.sst),
                              plot(conditional_effects(track.mod, effects = "sc.Chlor"))$sc.Chlor$data %>%
                                select(c("sc.Chlor", "estimate__", "lower__", "upper__")) %>%
                                mutate(Variable = "Chlor") %>%
                                rename(Value = sc.Chlor) %>%
                                mutate(Value = (Value * sd.chl) + mean.chl),
                              plot(conditional_effects(track.mod, effects = "sc.Depth"))$sc.Depth$data %>%
                                select(c("sc.Depth", "estimate__", "lower__", "upper__")) %>%
                                mutate(Variable = "Depth") %>%
                                rename(Value = sc.Depth) %>%
                                mutate(Value = (Value * sd.dep) + mean.dep)) %>%
                      mutate(Variable = factor(Variable, levels = c("Depth", "Chlor", "SST"))) %>%
                      mutate(Model = "Track"),
                    bind_rows(plot(conditional_effects(dive.mod, effects = "sc.SST"))$sc.SST$data %>%
                                select(c("sc.SST", "estimate__", "lower__", "upper__")) %>%
                                mutate(Variable = "SST") %>%
                                rename(Value = sc.SST) %>%
                                mutate(Value = (Value * sd.sst) + mean.sst),
                              plot(conditional_effects(dive.mod, effects = "sc.Chlor"))$sc.Chlor$data %>%
                                select(c("sc.Chlor", "estimate__", "lower__", "upper__")) %>%
                                mutate(Variable = "Chlor") %>%
                                rename(Value = sc.Chlor) %>%
                                mutate(Value = (Value * sd.chl) + mean.chl),
                              plot(conditional_effects(dive.mod, effects = "sc.Depth"))$sc.Depth$data %>%
                                select(c("sc.Depth", "estimate__", "lower__", "upper__")) %>%
                                mutate(Variable = "Depth") %>%
                                rename(Value = sc.Depth) %>%
                                mutate(Value = (Value * sd.dep) + mean.dep)) %>%
                      mutate(Variable = factor(Variable, levels = c("Depth", "Chlor", "SST"))) %>%
                      mutate(Model = "zDive"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

facet.names <- as_labeller(c("Depth" = "Bathymetry (m)",
                             "Chlor" = "Chlorophyll-a concentration (mg m\u207B\u00B3)",
                             "SST" = "Sea surface temperature (\u00B0C)",
                             "zDive" = "Dive vs traversed",
                             "Track" = "Traversed vs available"))

plot.dat <- rbind(dat %>%
                    select(c("Value", "SST", "Depth", "Chlor")) %>%
                    mutate(estimate__ = as.numeric(dplyr::recode(Value, Dive.Locs = "1", Available.Locs = "0", Trip.Locs = "0"))) %>%
                    unique() %>%
                    mutate(Model = ifelse(Value == "Dive.Locs" | Value == "Trip.Locs", "zDive", "Track")) %>%
                    gather(Variable, Value, SST:Chlor) %>%
                    mutate(Variable = factor(Variable, levels = c("Depth", "Chlor", "SST"))),
                  dat %>%
                    filter(Value == "Trip.Locs") %>%
                    select(c("Value", "SST", "Depth", "Chlor")) %>%
                    mutate(estimate__ = as.numeric(dplyr::recode(Value, Trip.Locs = "1"))) %>%
                    mutate(Model = "Track") %>%
                    gather(Variable, Value, SST:Chlor) %>%
                    mutate(Variable = factor(Variable, levels = c("Depth", "Chlor", "SST"))))
                    

ggplot() +
  geom_point(data = plot.dat, aes(x = Value, y = estimate__, col = Variable), alpha = 0.1, size = 0.3) +
  geom_smooth(data = ce.mod, aes(x = Value, y = estimate__,
                                     ymin = lower__, ymax = upper__, col = Variable, fill = Variable),
              stat = "identity", alpha = 0.5) +
  scale_colour_manual(values = c("#004488", "#DDAA33", "#BB5566")) +
  scale_fill_manual(values = c("#004488", "#DDAA33", "#BB5566")) +
  facet_grid(Model~Variable, scales = "free", labeller = facet.names) +
  theme_light() %+replace% theme(legend.position = "none",
                                 strip.background = element_rect(fill = "white", colour = "grey70"),
                                 strip.text = element_text(colour = "black", size = rel(0.9), hjust = 0.05),
                                 panel.grid.minor = element_blank()) +
  ylim(c(0,1)) +
  scale_x_continuous(labels = scales::comma) +
  xlab("Environmental variable") +
  ylab("Probability of habitat use")

ggsave("RFB_Diving_Plots/Habitat_fig.png", width = 8, height = 5.5)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

