rm(list = ls(all = TRUE))

library(tidyverse) # for nice code
library(brms) # for modelling
library(ROCR) # for area under a curve calculations

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load data ####
# Scale and center all variables
# Create new TripID variable that is unique to the particular bird ("BirdTrip")

dat <- read_csv("RFB_Diving_Data/BIOT_AxyTrek_Processed/AllBirds_Bouts_EnviroData.csv") %>%
  dplyr::select(!...1) %>%
  mutate(sc.SST = scale(SST)[,1]) %>%
  mutate(sc.Chlor = scale(Chlor)[,1]) %>%
  mutate(sc.Depth = scale(Depth)[,1]) %>%
  mutate(sc.Dist = scale(Dist.km)[,1]) %>%
  mutate(BirdTrip = paste(BirdID, TripID, sep = ".")) %>%
  mutate(BirdTripDive = paste(BirdID, TripID, DiveNum, sep = "."))
  
head(dat)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Select data from dives & the track only ####
# (Not the MCP - do that later)
# Revalue 'Value' so that dive = 1 and trip = 0
# Add weights

trip.dat <- dat[which(dat$Value =='Dive.Locs' | dat$Value =='Trip.Locs'),] %>%
  mutate(Value = as.numeric(dplyr::recode(Value, Dive.Locs = "1", Trip.Locs = "0")))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Quick plots to check co-linearity:

# ggplot(trip.dat) +
#   geom_point(aes(x = sc.Dist, y = sc.SST))
# ggplot(trip.dat) +
#   geom_point(aes(x = sc.Dist, y = sc.Chlor))
# ggplot(trip.dat) +
#   geom_point(aes(x = sc.Dist, y = sc.Depth))
# ggplot(trip.dat) +
#   geom_point(aes(x = sc.SST, y = sc.Chlor))
# ggplot(trip.dat) +
#   geom_point(aes(x = sc.SST, y = sc.Depth))
# ggplot(trip.dat) +
#   geom_point(aes(x = sc.Chlor, y = sc.Depth))

# There's no evidence of correlation between environmental variables

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Environment dive vs track - full model ####
# Test different ratios of points available

# Create object to store AUC values
auc.vals <- NULL

for(i in c(1,2,3,4,5,10,15,10,25,30,40,50)){
  
  # i = 1
  
  # Subset data to include a certain number of points and weight the points by this value
  trip.dati <- trip.dat %>%
    filter(PointNum <= i) %>%
    mutate(Weight = ifelse(Value == 1, 1, i))
  
  # See where NAs are present in the data & remove those comparisons
  trip.dati.na <- trip.dati[which(is.na(trip.dati$sc.SST) | is.na(trip.dati$sc.Chlor)),]
  trip.dati <- subset(trip.dati, !(trip.dati$BirdTripDive %in% trip.dati.na$BirdTripDive))
  
  # Run the model
  full.modi <- brm(Value | weights(Weight) ~ 0 + sc.Dist + sc.SST + sc.Chlor + sc.Depth +
                   (1 | BirdTrip),
                 data = trip.dati, family = bernoulli)
  
  # Model evaluation - 'area under curve' (AUC)
  # Tutorial: https://www.rensvandeschoot.com/tutorials/generalised-linear-models-with-brms/
  AUC <- performance(prediction(predict(full.modi, type = "response")[,1],
                                as.vector(pull(na.omit(trip.dati), Value))),
                     measure = "auc")
  AUC <- AUC@y.values[[1]]
  # A value of 0.50 means that the model does not classify better than chance
  # A good model should have an AUC score much higher than 0.50 (preferably higher than 0.80)
  
  # Also extract the R^2 value and the number of points used:
  R2 <- bayes_R2(full.modi)[,1]
  DivePoints <- nlevels(as.factor(trip.dati$BirdTripDive))
  
  # Save it up:
  auc.val <- cbind(i, AUC, R2, DivePoints)
  auc.vals <- rbind(auc.vals, auc.val)

  write_csv(as.data.frame(auc.vals), ("RFB_Diving_Data/Habitat_Modelling/Track_vs_Dive_random_point_AUCs.csv"))
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Drop 1 model selection:

trip.dat10 <- drop_na(trip.dat) %>%
  filter(PointNum <= 10) %>%
  mutate(Weight = ifelse(Value == 1, 1, 10))

mod10.depth.chlor <- brm(Value | weights(Weight) ~ 0 + sc.Dist + sc.Chlor + sc.Depth +
                   (1 | BirdTrip),
                 data = trip.dat10, family = bernoulli)

AUC <- performance(prediction(predict(mod10.depth.chlor, type = "response")[,1],
                              as.vector(pull(na.omit(trip.dat10), Value))),
                   measure = "auc")
AUC <- AUC@y.values[[1]]
auc.val <- cbind("depth.chlor", AUC)
auc.vals <- rbind(auc.vals, auc.val)

write_csv(as.data.frame(auc.vals), ("RFB_Diving_Data/Habitat_Modelling/Track_vs_Dive_model_selection_AUCs.csv"))

mod10.depth.sst <- brm(Value | weights(Weight) ~ 0 + sc.Dist + sc.SST + sc.Depth +
                           (1 | BirdTrip),
                         data = trip.dat10, family = bernoulli)

AUC <- performance(prediction(predict(mod10.depth.sst, type = "response")[,1],
                              as.vector(pull(na.omit(trip.dat10), Value))),
                   measure = "auc")
AUC <- AUC@y.values[[1]]
auc.val <- cbind("depth.sst", AUC)
auc.vals <- rbind(auc.vals, auc.val)

write_csv(as.data.frame(auc.vals), ("RFB_Diving_Data/Habitat_Modelling/Track_vs_Dive_model_selection_AUCs2.csv"))

mod10.chlor.sst <- brm(Value | weights(Weight) ~ 0 + sc.Dist + sc.SST + sc.Chlor +
                         (1 | BirdTrip),
                       data = trip.dat10, family = bernoulli)

AUC <- performance(prediction(predict(mod10.chlor.sst, type = "response")[,1],
                              as.vector(pull(na.omit(trip.dat10), Value))),
                   measure = "auc")
AUC <- AUC@y.values[[1]]
auc.val <- cbind("chlor.sst", AUC)
auc.vals <- rbind(auc.vals, auc.val)

write_csv(as.data.frame(auc.vals), ("RFB_Diving_Data/Habitat_Modelling/Track_vs_Dive_model_selection_AUCs3.csv"))

save(file="RFB_Diving_Data/Habitat_Modelling/FullModel_10.Rdata", list="full.modi")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(beepr)
beep(4)


plot(full.modi)
pp_check(full.modi)

summary(full.modi)

mcmc_plot(full.modi)
conditional_effects(full.modi)

bayes_R2(full.modi)
