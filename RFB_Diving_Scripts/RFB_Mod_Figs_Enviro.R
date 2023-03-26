rm(list = ls(all = TRUE))

library(tidyverse) # for nice code
library(brms) # for modelling

dat <- read_csv("RFB_Diving_Data/BIOT_AxyTrek_Processed/AllBirds_Bouts_EnviroData.csv")
head(dat)

# Select data from the dive trip only
# Revalue 'Value' so that dive = 1 and trip = 0
trip.dat <- dat[which(dat$Value =='Dive.Locs' | dat$Value =='Trip.Locs'),] %>%
  select(!...1) %>%
  mutate(Value = as.numeric(recode(Value, Dive.Locs = "1", Trip.Locs = "0"))) %>%
  mutate(Weight = ifelse(Value == 1, 1, 50))

trip.dat <- trip.dat[c(0.:5000),]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Dive location habitat selection model

sst.mod <- brm(Value | weights(Weight) ~ 0 + s(SST) + Dist.km +
                   (1 | BirdID/TripID),
                 data = trip.dat, family = bernoulli)

plot(sst.mod)
pp_check(sst.mod)

summary(sst.mod)

mcmc_plot(sst.mod)
conditional_effects(sst.mod)
