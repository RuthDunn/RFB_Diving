rm(list = ls(all = TRUE))

library(tidyverse) # for piping etc
library(data.table) # for converting df to dt
library(diveMove) # for extracting dive stats
library(sf) # for loading and plotting the Chagos shapefile

chagos = read_sf("RFB_Diving_Data/Chagos_Maps/chagos_maps/Chagos_v6_land_simple.shp")

files <- as.data.frame(list.files(path = "RFB_Diving_Data/BIOT_AxyTrek_Dives_csv/", pattern = "*.csv")) %>%
  separate(1, into = "files", sep = ".csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Loop through .csv files to load data ####

df.dives <- NULL

for (i in 1:nrow(files)) {
  
  # i = 1
  
  # Load data ####
  
  df.mini <- read_csv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_dive_stats.csv")) %>%
    drop_na() %>%
    dplyr::select(DateTime, Dive, PostDiveTime) %>%
    mutate(DateTime = DateTime + 43200) %>%
    mutate(Year = format(DateTime, format = "%Y")) %>%
    mutate(Hour = format(DateTime, format = "%H")) %>%
    mutate(Hour_shade = ifelse(as.numeric(format(DateTime, "%H")) >= 19 |
                                 as.numeric(format(DateTime, "%H")) <= 7, "gray80", NA))
  
  df.dives <- rbind(df.dives, df.mini)
  
}

head(df.dives)

rm(df.mini, i)

# Plot the cumulative frequency of post-dive intervals on a logarithmic scale ####
# against gap length

cumfreq.df <- cbind(as.data.frame(table(df.dives$PostDiveTime)), cumsum(table(df.dives$PostDiveTime))) %>%
  rename(CumFreq = 3, TimeDiffs = 1) %>%
  mutate_if(is.factor, as.numeric)

ggplot(cumfreq.df, aes(x = TimeDiffs, y = CumFreq)) +
  geom_line(group = 1) +
  scale_y_continuous(trans = "log2")

# Estimate the break-points ####
# Based off this tutorial: rpubs.com/MarkusLoew/12164

library(segmented)

# create a linear model
my.lm <- lm(log(CumFreq) ~ TimeDiffs, data = cumfreq.df)
summary(my.lm)

# Extract te coefficients from the overall model
my.coef <- coef(my.lm)

# Add the regression line to the graph
ggplot(cumfreq.df, aes(y = log(CumFreq), x = TimeDiffs)) +
  geom_line(group = 1) +
  # scale_y_continuous(trans = "log2") +
  geom_abline(intercept = my.coef[1],
              slope = my.coef[2], col = "red")

# Provide estimates for breakpoints
my.seg <- segmented(my.lm, 
                    seg.Z = ~ TimeDiffs, 
                    psi = list(TimeDiffs = c(75, 200)))

# Display the summary
summary(my.seg)
# Get the breakpoints
my.seg$psi
# Get the slopes
slope(my.seg)

# get the fitted data
my.fitted <- fitted(my.seg)
my.model <- data.frame(Times = cumfreq.df$TimeDiffs, Freq = my.fitted)

# plot the fitted model
ggplot(my.model, aes(x = Times, y = Freq)) + geom_line()

# I'm less sure about the lower break point, but it does come out the same, even when I vary my estimate
# I also won't use the lower break point, so we're all good.

# Soooo... 221 seconds