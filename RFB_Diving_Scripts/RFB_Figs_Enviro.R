rm(list = ls(all = TRUE))

library(tidyverse)
library(brms)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load the raw data so that we can un-scale and un-centre the variables

dat <- read_csv("RFB_Diving_Data/Habitat_Modelling//AllBirds_Bouts_EnviroData.csv") %>%
  dplyr::select(!...1)

mean.sst <- mean(dat$SST, na.rm = T)
sd.sst <- sd(dat$SST, na.rm = T)
mean.chl <- mean(dat$Chlor, na.rm = T)
sd.chl <- sd(dat$Chlor, na.rm = T)
mean.dep <- mean(dat$Depth)
sd.dep <- sd(dat$Depth)

# Load the final model:

load("RFB_Diving_Data/Habitat_Modelling/Available_vs_Dive_Model.Rdata")
track.mod <- mod.chlor.sst
load("RFB_Diving_Data/Habitat_Modelling/Track_vs_Dive_Model.Rdata")
avail.mod <- mod.chlor.sst
rm(mod.chlor.sst)

# Pull out data frames of conditional effects:

# plot(conditional_effects(full.modi))

ce.mod <- bind_rows(bind_rows(plot(conditional_effects(track.mod, effects = "sc.SST"))$sc.SST$data %>%
                                select(c(1,9,11,12)) %>%
                                mutate(Variable = "SST") %>%
                                rename(Value = sc.SST) %>%
                                mutate(Value = (Value * sd.sst) + mean.sst),
                              plot(conditional_effects(track.mod, effects = "sc.Chlor"))$sc.Chlor$data %>%
                                select(c(1,9,11,12)) %>%
                                mutate(Variable = "Chlor") %>%
                                rename(Value = sc.Chlor) %>%
                                mutate(Value = (Value * sd.chl) + mean.chl)) %>%
                      mutate(Variable = factor(Variable, levels = c("Depth", "Chlor", "SST"))) %>%
                      mutate(Model = "Track"),
                    bind_rows(plot(conditional_effects(avail.mod, effects = "sc.SST"))$sc.SST$data %>%
                                select(c(1,9,11,12)) %>%
                                mutate(Variable = "SST") %>%
                                rename(Value = sc.SST) %>%
                                mutate(Value = (Value * sd.sst) + mean.sst),
                              plot(conditional_effects(avail.mod, effects = "sc.Chlor"))$sc.Chlor$data %>%
                                select(c(1,9,11,12)) %>%
                                mutate(Variable = "Chlor") %>%
                                rename(Value = sc.Chlor) %>%
                                mutate(Value = (Value * sd.chl) + mean.chl)) %>%
                      mutate(Variable = factor(Variable, levels = c("Depth", "Chlor", "SST"))) %>%
                      mutate(Model = "Available"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

facet.names <- as_labeller(c("Chlor" = "Chlorophyll-a concentration (mg m\u207B\u00B3)",
                             "SST" = "Sea surface temperature (\u00B0C)",
                             "Available" = "Used vs available",
                             "Track" = "Used vs traversed"))

plot.dat <- dat %>%
  select(c("Value", "SST", "Depth", "Chlor")) %>%
  mutate(estimate__ = as.numeric(dplyr::recode(Value, Dive.Locs = "1", Available.Locs = "0", Trip.Locs = "0"))) %>%
  unique() %>%
  gather(Variable, Value, SST:Chlor) %>%
  filter(Variable != "Depth")

ggplot() +
  geom_point(data = plot.dat, aes(x = Value, y = estimate__, col = Variable), alpha = 0.1, size = 0.3) +
  geom_smooth(data = ce.mod, aes(x = Value, y = estimate__,
                                     ymin = lower__, ymax = upper__, col = Variable, fill = Variable),
              stat = "identity", alpha = 0.5) +
  scale_colour_manual(values = c("#DDAA33", "#BB5566")) +
  scale_fill_manual(values = c("#DDAA33", "#BB5566")) +
  facet_grid(Model~Variable, scales = "free",
             labeller = facet.names) +
  theme_light() %+replace% theme(legend.position = "none",
                                 strip.background = element_rect(fill = "white", colour = "grey70"),
                                 strip.text = element_text(colour = "black", size = rel(0.9), hjust = 0.05),
                                 panel.grid.minor = element_blank()) +
  ylim(c(0,1)) +
  scale_x_continuous(labels = scales::comma) +
  xlab("Environmental variable") +
  ylab("Probability of habitat use")

ggsave("RFB_Diving_Plots/Habitat_fig.png", width = 5.5, height = 5.5)
