rm(list = ls(all = TRUE))

library(tidyverse) # for piping etc
library(data.table) # for converting df to dt
library(diveMove) # for extracting dive stats
library(sf) # for loading and plotting the Chagos shapefile
library(ggpubr) # for saving 2 plots side by side

chagos = read_sf("RFB_Diving_Data/Chagos_Maps/chagos_maps/Chagos_v6_land_simple.shp")

files <- as.data.frame(list.files(path = "RFB_Diving_Data/BIOT_AxyTrek_Processed/", pattern = "*_dive_stats.csv")) %>%
  separate(1, into = "files", sep = "_dive_stats.csv")

# Colour palette: https://personal.sron.nl/~pault/#sec:qualitative

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Loop through .csv files to load data ####

df.dives <- NULL

for (i in 1:nrow(files)) {
  
  # i = 1
  
  # Load data ####
  
  df.mini <- read_csv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_dive_stats.csv")) %>%
    drop_na() %>%
    select(DateTime, Dive, MaxDepth) %>%
    mutate(DateTime = DateTime + 43200) %>% # Currently I'm just adding on 12 hours to make this look more realistic. Need to fix this!
    mutate(Year = format(DateTime, format = "%Y")) %>%
    mutate(Hour = format(DateTime, format = "%H")) %>%
    mutate(Hour_shade = ifelse(as.numeric(format(DateTime, "%H")) >= 19 |
                                 as.numeric(format(DateTime, "%H")) <= 7, "gray80", NA))
  
  df.dives <- rbind(df.dives, df.mini)
  
}

rm(df.mini, i)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Add hours that don't have dives as empty factors to df
df.dives$Hour <- as.factor(df.dives$Hour)
levels(df.dives$Hour)
levels(df.dives$Hour) <- c(levels(df.dives$Hour), 00:04, 21:24)
df.dives$Hour <- ordered(df.dives$Hour, levels = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12",
                                       "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plots the frequency of dives at different times of day ####
    
a <- ggplot(df.dives, aes(x = Hour, y = Dive)) +
      # geom_rect(aes(xmin = "00", xmax = "24", ymin = -Inf, ymax = Inf,
      #               fill = Hour_shade), alpha = .2) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), fill = "#DDAA33") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_discrete(drop=FALSE) +
  scale_x_discrete(drop=FALSE) +
  theme_light() %+replace% theme(axis.title.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.text.x = element_blank()) +
  ylab("Relative frequency of dive events")

# Plots the dive depths at different times of day ####

b <- ggplot(df.dives, aes(x = Hour, y = MaxDepth)) +
  geom_jitter(col = "grey70", alpha = 0.3) +
  geom_violin(fill = NA, col = "#DDAA33") +
  scale_fill_discrete(drop=FALSE) +
  scale_x_discrete(drop=FALSE) +
  theme_light() +
  ylab("Maximum depth (m)") +
  xlab("Hour of the day")

# Save plots:

ggarrange(a, b, nrow = 2)

ggsave("RFB_Diving_Plots/Dive_fig.png", width = 8, height = 5)
